"""Strict DOS status/output checks for DPMI clients in isolated profiles."""
import argparse
import hashlib
import json
from pathlib import Path
import runpy
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
ENV = runpy.run_path(str(ROOT / "tests/CHECK.PY"))
CASES = runpy.run_path(str(ROOT / "tests/dpmi/CATALOG.PY"))["BOUND"]

DEPENDENCIES = {"MAPEXIT": "PHYSMAP", "CBEXIT": "CBACK"}

COM_CASES = ("MAPEXIT", "CBEXIT")

EXPERIMENTS = {"ENTERH": "ENTERH native bootstrap: ENTRY32 native initialization: ok\nok",
               "ENTER16": "ENTERH native bootstrap: ENTRY16 native initialization: ok\nok"}


def check_result(folder, cases):
    failures = []
    for name, expected in cases.items():
        status, output = (folder / (name + suffix) for suffix in (".STS", ".LOG"))
        if not status.exists() or status.read_text().strip() != "0":
            failures.append(name + ": nonzero/missing DOS status")
        if not output.exists() or output.read_text(errors="replace").strip() != expected:
            failures.append(name + ": unexpected/missing output")
    return failures


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work", required=True, type=Path)
    parser.add_argument("--profile", choices=("xms", "raw", "vcpi", "hdpmi"), default="xms")
    parser.add_argument("--tools", type=Path, default=ROOT / "bin")
    parser.add_argument("--case", action="append", choices=tuple(CASES) + tuple(EXPERIMENTS))
    parser.add_argument("--build-only", action="store_true")
    parser.add_argument("--timeout", type=int, default=180)
    args = parser.parse_args()
    work = args.work.resolve()
    if work == ROOT or ROOT in work.parents:
        raise RuntimeError("scratch must be outside checkout")
    work.mkdir(parents=True, exist_ok=True)
    shutil.copytree(args.tools.resolve(), work / "bin", dirs_exist_ok=True)
    shutil.copytree(ROOT / "tests", work / "tests", dirs_exist_ok=True)
    external = args.profile == "hdpmi"
    cases = {"ENTRY16": "ENTRY16 native initialization: ok", "ENTRY32": "ENTRY32 native initialization: ok"} if external else CASES
    if args.case:
        available = dict(CASES, **EXPERIMENTS)
        cases = {name: available[name] for name in args.case}
    build_cases = dict(cases)
    for name in cases:
        if name in DEPENDENCIES:
            build_cases.setdefault(DEPENDENCIES[name], CASES[DEPENDENCIES[name]])
    folder = work / "tests/dpmi"
    native = external and not args.case
    if native or set(cases) & set(EXPERIMENTS):
        for width in (16, 32):
            subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                            "ENTRY.ASM", "-o", "ENTRY" + str(width) + ".COM"], cwd=folder, check=True)
    ENV["config"](work, "xms" if external else args.profile, False)
    for name in build_cases:
        for suffix in (".LOG", ".STS") + (() if native else ((".COM",) if name in COM_CASES else (".EXE", ".RDO", ".RDF"))):
            (folder / (name + suffix)).unlink(missing_ok=True)
    lines = ["@echo off", r"cd tests\dpmi"]
    if not native:
        for name in build_cases:
            if name in COM_CASES:
                lines += ["nasm -s -f bin " + name + ".ASM -o " + name + ".COM > " + name + ".BLD", "if errorlevel 1 goto bad"]
                continue
            source = "-DCLIENT32=0 ENTERH.ASM" if name == "ENTER16" else name + ".ASM"
            if name in ("OOMB0", "OOMB1", "OOMB32"):
                source = "-DBOUND=1 -DCLIENT32=1 -DSPARE=" + name[4:] + " REALLOOM.ASM"
            if name == "LDTAIL":
                source = "-DBOUND=1 -DCLIENT32=1 LDTAIL.ASM"
            if name == "CONTROL" and not external:
                source = "-DREALMODE=" + str(int(args.profile != "vcpi")) + " " + source
            commands = ["nasm -s -f rdf " + source + " -o " + name + ".RDO",
                        "rlink rdf /o=" + name + ".RDF " + name + ".RDO",
                        r"R:\BIN\ZRXBIND -y " + name + ".RDF " + name + ".EXE",
                        r"R:\BIN\ZRXSETUP /B0 " + name + ".EXE"]
            for index, command in enumerate(commands):
                lines += [command + (" > " if index == 0 else " >> ") + name + ".BLD", "if errorlevel 1 goto bad"]
    if external:
        lines += [r"C:\HX\BIN\HDPMI" + ("16" if native else "32") + r".EXE -r > HDPMI.LOG"]
    lines += ["nasm -s -f bin HOST.ASM -o HOST.COM > HOST.BLD", "if errorlevel 1 goto bad", "HOST.COM > HOST.LOG"]
    for number, name in enumerate({} if args.build_only else cases):
        if name == "ENTRY32":
            lines += [r"C:\HX\BIN\HDPMI16.EXE -u > UN16.LOG", r"C:\HX\BIN\HDPMI32.EXE -r > HOST32.LOG"]
        lines += [name + (".COM" if native or name in COM_CASES else ".EXE") + " > " + name + ".LOG",
                  "if errorlevel 1 goto fail" + str(number),
                  "echo 0 > " + name + ".STS", "goto done" + str(number),
                  ":fail" + str(number), "echo 1 > " + name + ".STS", ":done" + str(number)]
    lines += [r"cd ..\..", "echo OK > STATUS.LOG", "goto end", ":bad", "cd R:" + chr(92), "echo FAILED > STATUS.LOG", ":end"]
    ENV["dos"](work, "\n".join(lines) + "\n", args.timeout)
    failures = [] if args.build_only else check_result(folder, cases)
    provider = (folder / "HOST.LOG").read_text()
    expected = "External DPMI 1687: " + ("present" if external else "absent")
    if expected not in provider:
        failures.append("unexpected DPMI provider")
    manifest = {"source": ENV["fingerprint"](), "profile": args.profile, "provider": provider, "failures": failures, "cases": list(cases), "build_only": args.build_only, "built": list(build_cases),
                "tools": {n: hashlib.sha256((work / "bin" / n).read_bytes()).hexdigest() for n in ENV["TOOLS"]}}
    ENV["write"](work / "D09.JSN", json.dumps(manifest, indent=2) + "\n")
    print(provider, end="")
    for failure in failures:
        print(failure)
    print("FAIL" if failures else "PASS", args.profile, work)
    return bool(failures)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
        print("ERROR:", exc, file=sys.stderr)
        sys.exit(1)
