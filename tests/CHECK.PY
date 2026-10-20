"""Build host NASM OMF, run DOS tools and regression fixtures in scratch."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import runpy
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parent.parent
CFG = runpy.run_path(str(ROOT / "tests/dpmi/ZRCFG.PY"))
TOOLS = ("zrxbind.com", "zrxsetup.exe", "stub-le.exe", "stub-pe.exe", "stub-rdf.exe")


def write(path, text):
    path.write_bytes(text.replace("\r\n", "\n").replace("\n", "\r\n").encode("ascii"))


def command(args, cwd):
    result = subprocess.run(args, cwd=cwd, capture_output=True, text=True)
    if result.stderr:
        print(result.stderr, end="", flush=True)
    if result.returncode:
        raise RuntimeError("command failed: " + " ".join(map(str, args)))
    return result.stdout.strip()


def fingerprint():
    digest = hashlib.sha256()
    for path in sorted((ROOT / "src").rglob("*")):
        if path.is_file():
            digest.update(str(path.relative_to(ROOT)).encode())
            digest.update(path.read_bytes())
    return digest.hexdigest()


def config(work, profile, real_clip, lfn="true", core="auto"):
    original = CFG["require"]("ZRDX_DOSCONF").read_text()
    write(work / "USER.CON", original)
    base = re.split(r"(?m)^\[autoexec\]\s*$", original)[0]
    values = {"core": core, "lfn": lfn, "cycles": "max", "xms": "false" if profile == "raw" else "true",
              "ems": "emm386" if profile == "vcpi" else "false",
              "dos clipboard api": "true" if real_clip else "false"}
    for key, value in values.items():
        base = re.sub(r"(?m)^" + re.escape(key) + r"\s*=.*$", key + " = " + value, base)
    # Place advanced DOS settings inside [dos], not after another section.
    base = base.replace("[dos]\n", "[dos]\nvcpi = true\nemm386 startup active = " +
                        ("true" if profile == "vcpi" else "false") + "\n")
    auto = r"""[autoexec]
@echo off
mount -q C DRIVEC
mount -q D DRIVED
mount -q R WORKDIR
set WATCOM=D:\WATCOM
set INCLUDE=D:\WATCOM\H
set EDPATH=D:\WATCOM\EDDAT
set PATH=C:\BIN;D:\WATCOM\BINW;Z:\;Z:\SYSTEM;Z:\BIN
R:
call AUDIT.BAT
exit
""".replace("WORKDIR", str(work)).replace("DRIVEC", str(CFG["require"]("ZRDX_DRIVE_C"))).replace("DRIVED", str(CFG["require"]("ZRDX_DRIVE_D")))
    write(work / "DOSBOX.CON", base + auto)


def dos(work, batch, timeout):
    write(work / "AUDIT.BAT", batch)
    (work / "STATUS.LOG").unlink(missing_ok=True)
    (work / "STOP.TXT").unlink(missing_ok=True)
    env = dict(os.environ, SDL_VIDEODRIVER="dummy", SDL_AUDIODRIVER="dummy")
    with (work / "HOST.LOG").open("w") as log:
        proc = subprocess.Popen([CFG["dosbox"](), "-conf", str(work / "DOSBOX.CON"),
                                 "-nogui", "-nomenu"], stdout=log, stderr=subprocess.STDOUT, env=env)
        write(work / "DOSBOX.PID", str(proc.pid) + "\n")
        deadline = time.monotonic() + timeout
        while proc.poll() is None:
            if (work / "STOP.TXT").exists() or time.monotonic() >= deadline or (work / "HOST.LOG").stat().st_size > 4000000:
                proc.kill()
                proc.wait()
                raise RuntimeError("DOSBox watchdog/fault log limit; see " + str(work / "HOST.LOG"))
            time.sleep(0.2)
        status = proc.returncode
    if status:
        raise RuntimeError("DOSBox exited with host status " + str(status))
    statusfile = work / "STATUS.LOG"
    if not statusfile.exists() or statusfile.read_text().strip() != "OK":
        raise RuntimeError("DOS batch failed; inspect scratch logs")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work", type=Path)
    parser.add_argument("--profile", choices=("xms", "raw", "vcpi"), default="xms")
    parser.add_argument("--core", choices=("auto", "normal", "dynamic", "full"), default="auto")
    parser.add_argument("--lfn", choices=("true", "false", "auto"), default="true",
                        help="LFN in the private config; true requires both T40 cases to run")
    parser.add_argument("--skip-build", action="store_true")
    parser.add_argument("--supplied", action="store_true", help="diagnose supplied binaries instead of rebuilding")
    parser.add_argument("--bugs-only", action="store_true", help="skip the long existing regression campaign")
    parser.add_argument("--real-clip", action="store_true", help="use the DOSBox host clipboard instead of the fixture")
    parser.add_argument("--timeout", type=int, default=900)
    args = parser.parse_args()
    work = (args.work or Path(tempfile.mkdtemp(prefix="ZRX", dir=CFG["scratch"]()))).resolve()
    if work == ROOT or ROOT in work.parents:
        raise RuntimeError("work must be outside the checkout")
    work.mkdir(parents=True, exist_ok=True)
    print("Scratch:", work, flush=True)
    source_id = fingerprint()
    if args.skip_build:
        if (work / "BUILD.ID").read_text().strip() != fingerprint():
            raise RuntimeError("source changed since scratch build; rebuild first")
    else:
        for name in ("src", "bin"):
            shutil.copytree(ROOT / name, work / name, dirs_exist_ok=True)
    shutil.copytree(ROOT / "tests", work / "tests", dirs_exist_ok=True)
    config(work, "xms", False, lfn=args.lfn, core=args.core)
    nasm = shutil.which("nasm")
    print(command([nasm, "-v"], ROOT), flush=True)
    if not args.skip_build and not args.supplied:
        for stem in ("zrdx", "zrdxpe", "zrdxrdf"):
            target = work / "src" / (stem + ".obj")
            temporary = work / "src/NASM.NEW"
            command([nasm, "-O9", "-f", "obj", stem + ".asm", "-o", temporary.name], work / "src")
            if not target.exists() or target.read_bytes() != temporary.read_bytes():
                temporary.replace(target)
            else:
                temporary.unlink()
                target.touch()  # copied headers must not trigger DOS reassembly
        command([nasm, "-O9", "-f", "bin", "binder/bnd.asm", "-o", "binder/zrxbind.com"], work / "src")
        dos(work, r"""@echo off
cd src
wmake all > BUILD.LOG
if errorlevel 1 goto bad
upx -9 --8086 binder\zrxbind.com >> BUILD.LOG
if errorlevel 1 goto bad
wmake install >> BUILD.LOG
if errorlevel 1 goto bad
cd ..
echo OK > STATUS.LOG
goto end
:bad
cd ..
echo FAILED > STATUS.LOG
:end
""", args.timeout)
        write(work / "BUILD.ID", source_id + "\n")
        print("Fresh build OK", flush=True)
    for file in TOOLS:
        path = work / "bin" / file
        if not path.is_file():
            raise RuntimeError("missing tool " + file)
    command([nasm, "-f", "bin", "HELLO.ASM", "-o", "HELLO.BIN"], work / "tests/pe")
    for out, base in (("RELOC", "0x401000"), ("RELOC2", "0x501000"), ("RELOCH", "0x10001000")):
        command([nasm, "-f", "bin", "-DBASE=" + base, "RELOC.ASM", "-o", out + ".BIN"], work / "tests/pe")
    pe_gen = runpy.run_path(str(work / "tests/pe/GEN.PY"))
    pe_cases = pe_gen["generate"](work / "tests/pe")
    if (work / "tests/mz/GEN.PY").exists():
        runpy.run_path(str(work / "tests/mz/GEN.PY"))["generate"](work / "tests/mz")
    bind_cases = runpy.run_path(str(work / "tests/bind/GEN.PY"))["generate"](work / "tests/bind")
    config(work, args.profile, args.real_clip, lfn=args.lfn, core=args.core)
    batch = r"""@echo off
cd tests\dpmi
nasm -s -f bin HOST.ASM -o HOST.COM > BUILD.LOG
if errorlevel 1 goto bad
HOST.COM > HOST.LOG
cd ..\clip
nasm -s -f bin MOCK.ASM -o MOCK.COM > BUILD.LOG
if errorlevel 1 goto bad
MOCKLINE
cd ..\rdf
set ZB=R:\BIN
call BUILD.BAT > BUILD.LOG
cd ..\dpmi
call PROBE.BAT > BUILD.LOG
cd ..\pe
call RUN.BAT > SCREEN.LOG
MZLINE
cd R:\
echo OK > STATUS.LOG
goto end
:bad
cd R:\
echo FAILED > STATUS.LOG
:end
""".replace("MOCKLINE", "rem host clipboard" if args.real_clip else "MOCK.COM")
    batch = batch.replace("MZLINE", "cd ..\\mz\ncall RUN.BAT > SCREEN.LOG" if (work / "tests/mz/RUN.BAT").exists() else "rem no MZ fixtures")
    batch = batch.replace("echo OK > STATUS.LOG", "cd tests\\bind\ncall RUN.BAT > SCREEN.LOG\ncd ..\\..\necho OK > STATUS.LOG")
    if args.bugs_only:
        batch = batch.replace("call BUILD.BAT > BUILD.LOG", "rem skip legacy build")
    dos(work, batch, args.timeout)
    errors = []
    for name in ("PROBE", "UNSUP", "CBACK", "PHYSMAP", "MAPEXIT", "CBEXIT"):
        text = (work / "tests/dpmi" / (name + ".LOG")).read_text(errors="replace")
        good = text.count("PASS") == 3 if name == "PROBE" else ": ok" in text
        if not good or "FAIL" in text:
            errors.append(name + " failed: " + text.strip())
    pe = (work / "tests/pe/RESULT.LOG").read_text(errors="replace")
    for name in pe_cases:
        if name + " OK" not in pe or name + " FAILED" in pe or name + " BIND_FAILED" in pe:
            errors.append("PE " + name + " failed")
    if (pe.count("ZRDX loader error:") != len(pe_cases) - len(pe_gen["VALID"])
            or "PE single module: ok" not in pe or pe.count("PE reloc: ok") != 2):
        errors.append("PE expected loader diagnostics missing")
    mz = work / "tests/mz/RESULT.LOG"
    if mz.exists() and "FAILED" in mz.read_text():
        errors.append("MZ aligner fixtures failed")
    if mz.exists():
        try:
            runpy.run_path(str(work / "tests/mz/GEN.PY"))["verify"](work / "tests/mz")
        except RuntimeError as exc:
            errors.append(str(exc))
    bind = (work / "tests/bind/RESULT.LOG").read_text()
    for name in bind_cases:
        if name + " OK" not in bind or name + " FAILED" in bind:
            errors.append("binder " + name + " failed")
    if not errors and not args.bugs_only:
        dos(work, r"""@echo off
cd tests\clip
MOCKLINE
cd ..\rdf
call RUN.BAT > SCREEN.LOG
cd ..\..
echo OK > STATUS.LOG
""".replace("MOCKLINE", "rem host clipboard" if args.real_clip else "MOCK.COM"), args.timeout)
        rdf = (work / "tests/rdf/RUN.LOG").read_text(errors="replace")
        build = (work / "tests/rdf/BUILD.LOG").read_text(errors="replace")
        if "FAILED" in rdf or "BUILD FAILED" in build:
            errors.append("existing RDF regressions failed")
        if "T59X LDT exit cleanup: ok" not in rdf:
            errors.append("existing regression run incomplete")
        if not args.real_clip and "no clipboard" in rdf:
            errors.append("clipboard fixtures were unexpectedly skipped")
        if args.lfn == "true" and rdf.count("T40 lfn: ok") != 2:
            errors.append("LFN enabled: both T40 formats must pass without skips")
    legacy_cases = sorted({line.split()[0] for line in rdf.splitlines() if line.endswith("ok")}) if not args.bugs_only and not errors else []
    info = {"cases": legacy_cases, "schema": "legacy-1", "completed": not errors and not args.bugs_only,
            "source": source_id, "ok_lines": sum(line.endswith("ok") for line in rdf.splitlines()) if not args.bugs_only and not errors else 0,
            "lfn_passes": rdf.count("T40 lfn: ok") if not args.bugs_only and not errors else 0,
            "profile": args.profile, "core": args.core, "lfn": args.lfn, "supplied": args.supplied, "clipboard": "host" if args.real_clip else "mock",
            "tools": {n: hashlib.sha256((work / "bin" / n).read_bytes()).hexdigest() for n in TOOLS},
            "provider": (work / "tests/dpmi/HOST.LOG").read_text(), "errors": errors}
    write(work / "RESULT.JSN", json.dumps(info, indent=2) + "\n")
    print(info["provider"], end="")
    for error in errors:
        print(error)
    print("FAILED" if errors else "PASS", "profile", args.profile, "scratch", work, flush=True)
    return bool(errors)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
        print("ERROR:", exc, file=sys.stderr)
        sys.exit(1)
