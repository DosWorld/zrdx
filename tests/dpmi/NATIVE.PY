"""Build and check the explicit native host profile, outside ordinary builds."""
import argparse
import hashlib
import json
from pathlib import Path
import runpy
import re
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
ENV = runpy.run_path(str(ROOT / "tests/CHECK.PY"))
EXPECTED = runpy.run_path(str(ROOT / "tests/dpmi/CATALOG.PY"))["NATIVE"]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work", required=True, type=Path)
    parser.add_argument("--profile", choices=("raw", "xms", "vcpi"), default="xms")
    parser.add_argument("--case", action="append", choices=tuple(EXPECTED))
    parser.add_argument("--core", choices=("normal", "auto", "dynamic"), default="normal")
    parser.add_argument("--build-only", action="store_true")
    parser.add_argument("--qemu", type=Path)
    parser.add_argument("--timeout", type=int, default=120)
    args = parser.parse_args()
    work = args.work.resolve()
    if not work.is_relative_to(Path("/private/tmp")):
        raise ValueError("work must be an owned /private/tmp directory")
    work.mkdir(parents=True, exist_ok=True)
    source_digest = hashlib.sha256()
    shutil.copytree(ROOT / "src", work / "src", dirs_exist_ok=True)
    for path in sorted((work / "src").rglob("*")):
        if path.is_file():
            source_digest.update(str(path.relative_to(work)).encode())
            source_digest.update(path.read_bytes())
    subprocess.run([shutil.which("nasm"), "-f", "obj", "-l", str(work / "KLAYOUT.LST"),
                    str(ROOT / "tests/dpmi/KLAYOUT.ASM"), "-o", str(work / "KLAYOUT.OBJ")],
                   cwd=work / "src", check=True)
    layout = (work / "KLAYOUT.LST").read_text()
    match = re.search(r"[0-9A-F]{8} ([0-9A-F]{16}) +dd OffKernelStackStart, OffKernelStack1Start", layout)
    if not match:
        raise ValueError("cannot extract actual kernel-stack addresses")
    raw = bytes.fromhex(match[1])
    stacks = ["-DKSTACK" + str(i) + "=" + str(int.from_bytes(raw[i*4:i*4+4], "little")) for i in range(2)]
    match = re.search(r"[0-9A-F]{8} ([0-9A-F]{8}) +dd OffIret2KernelAsincTrap3", layout)
    if not match:
        raise ValueError("cannot extract the actual kernel IRQ trampoline")
    irq_trap = int.from_bytes(bytes.fromhex(match[1]), "little")
    subprocess.run([shutil.which("nasm"), "-f", "bin", str(ROOT / "tests/dpmi/A20SPY.ASM"), "-o", str(work / "A20X.COM")], check=True)
    subprocess.run([shutil.which("nasm"), "-f", "bin", str(ROOT / "tests/dpmi/LATENC.ASM"), "-o", str(work / "LATENC.COM")], check=True)
    for source, target in (("A20RM", "A20RM"), ("A20RAW", "A20R")):
        subprocess.run([shutil.which("nasm"), "-I", str(ROOT / "tests/dpmi") + "/", "-f", "bin", str(ROOT / ("tests/dpmi/" + source + ".ASM")), "-o", str(work / (target + ".COM"))], check=True)
    subprocess.run([shutil.which("nasm"), "-f", "bin", str(ROOT / "tests/dpmi/DRSAVE.ASM"), "-o", str(work / "DRSAVE.COM")], check=True)
    for width in (16, 32):
        subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)),
                        *stacks, "-f", "bin", str(ROOT / "tests/dpmi/STKEDGE.ASM"),
                        "-o", str(work / ("STK" + str(width) + ".COM"))], check=True)
        for prefix, spare in (("OOM", 0), ("ONE", 1), ("TAIL", 32)):
            subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)),
                            "-DSPARE=" + str(spare), "-f", "bin", str(ROOT / "tests/dpmi/REALLOOM.ASM"),
                            "-o", str(work / (prefix + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DPOSITIVE=1", "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                        str(ROOT / "tests/dpmi/BASECALL.ASM"), "-o", str(work / ("REG" + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DNEGATIVE=1", "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                        str(ROOT / "tests/dpmi/BASECALL.ASM"), "-o", str(work / ("ERR" + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DIRQEXEC=1", "-DKERNELIRET=" + str(irq_trap),
                        "-DCLIENT32=" + str(int(width == 32)), "-f", "bin", str(ROOT / "tests/dpmi/OWNPAR.ASM"),
                        "-o", str(work / ("IRQ" + str(width) + ".COM"))], check=True)
        for source, target in (("PREFINT", "PREF"), ("LDTAIL", "LDT"), ("CBKTEST", "CBK"), ("CODEVEC", "COD"), ("MEMINFO", "MEM"), ("LIMTEST", "LIM"), ("GATECHK", "GAT"), ("BASECALL", "BAS"), ("BRIDGE", "BRG")):
            subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                            str(ROOT / ("tests/dpmi/" + source + ".ASM")), "-o", str(work / (target + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                        str(ROOT / "tests/dpmi/RAWTEST.ASM"), "-o", str(work / ("RAW" + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)), *stacks, "-f", "bin",
                        str(ROOT / "tests/dpmi/KSTACK.ASM"), "-o", str(work / ("KST" + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                        str(ROOT / "tests/dpmi/DEFLT.ASM"), "-o", str(work / ("DEX" + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DACTUALNM=1", "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                        str(ROOT / "tests/dpmi/DEFLT.ASM"), "-o", str(work / ("NMX" + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                        str(ROOT / "tests/dpmi/DOSMEM.ASM"), "-o", str(work / ("NDOS" + str(width) + ".COM"))], check=True)
        for source, target in (("OWNPAR", "FRT"), ("OWNCHLD", "FRC")):
            subprocess.run([shutil.which("nasm"), "-DFATAL=2", "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                            str(ROOT / ("tests/dpmi/" + source + ".ASM")), "-o", str(work / (target + str(width) + ".COM"))], check=True)
        for source, target in (("OWNPAR", "FEX"), ("OWNCHLD", "FCH")):
            subprocess.run([shutil.which("nasm"), "-DFATAL=1", "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                            str(ROOT / ("tests/dpmi/" + source + ".ASM")), "-o", str(work / (target + str(width) + ".COM"))], check=True)
        for source, target in (("OWNPAR", "NEST"), ("OWNCHLD", "OWNC")):
            subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                            str(ROOT / ("tests/dpmi/" + source + ".ASM")), "-o", str(work / (target + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)), "-f", "bin",
                        str(ROOT / "tests/dpmi/MODECH.ASM"), "-o", str(work / ("MODE" + str(width) + ".COM"))], check=True)
        subprocess.run([shutil.which("nasm"), "-DCLIENT32=" + str(int(width == 32)),
                        "-f", "bin", str(ROOT / "tests/dpmi/ENTRY.ASM"), "-o",
                        str(work / ("ENTRY" + str(width) + ".COM"))], check=True)
    subprocess.run([shutil.which("nasm"), "-DCLIENT32=0", "-f", "bin", str(ROOT / "tests/dpmi/NATLOCK.ASM"), "-o", str(work / "NATL16.COM")], check=True)
    subprocess.run([shutil.which("nasm"), "-f", "bin", str(ROOT / "tests/dpmi/NATLOCK.ASM"), "-o", str(work / "NATLOCK.COM")], check=True)
    subprocess.run([shutil.which("nasm"), "-DFIRST32=1", "-f", "bin", str(ROOT / "tests/dpmi/NATMODE.ASM"), "-o", str(work / "NATM32.COM")], check=True)
    subprocess.run([shutil.which("nasm"), "-f", "bin", str(ROOT / "tests/dpmi/NATMODE.ASM"), "-o", str(work / "NATMODE.COM")], check=True)
    subprocess.run([shutil.which("nasm"), "-f", "bin", str(ROOT / "tests/dpmi/NATERR.ASM"), "-o", str(work / "NATERR.COM")], check=True)
    subprocess.run([shutil.which("nasm"), "-f", "bin", str(ROOT / "tests/dpmi/HOST.ASM"),
                    "-o", str(work / "HOST.COM")], check=True)
    subprocess.run([shutil.which("nasm"), "-O9", "-f", "obj", "-l", "NATIVE.LST",
                    "zrxdpmi.asm", "-o", "zrxdpmi.obj"], cwd=work / "src", check=True)
    ENV["config"](work, "xms", False)
    ENV["dos"](work, "\n".join([
        "@echo off", "cd src", "wlink option quiet option map=NATIVE.MAP name zrxdpmi.exe format dos file zrxdpmi.obj > LINK.LOG",
        "if errorlevel 1 goto bad", "copy zrxdpmi.exe .." + chr(92), "cd ..",
        "echo OK > STATUS.LOG", "goto end", ":bad", "cd R:" + chr(92), "echo FAILED > STATUS.LOG", ":end"]) + "\n", args.timeout)
    cases = args.case or [name for name in EXPECTED if args.qemu or not name.startswith("NMX")]
    failures = []
    if not args.build_only and args.qemu:
        command = [sys.executable, str(ROOT / "tests/dpmi/QCHECK.PY"), "--qemu", str(args.qemu),
                   "--native-host", str(work / "ZRXDPMI.EXE"), "--fixtures", str(work),
                   "--work", str(work / "QEMU"), "--profile", args.profile, "--timeout", str(args.timeout)]
        for name in cases:
            command += ["--case", name]
        if subprocess.run(command).returncode:
            failures.append("QEMU native campaign failed; inspect QEMU/RESULT.JSN")
    elif not args.build_only:
        ENV["config"](work, args.profile, False)
        conf = work / "DOSBOX.CON"
        ENV["write"](conf, re.sub(r"(?m)^core\s*=.*$", "core = " + args.core, conf.read_text()))
        lines = ["@echo off", "HOST.COM > BEFORE.LOG", "ZRXDPMI HOST.COM > INSIDE.LOG"]
        for number, name in enumerate(cases):
            for suffix in (".LOG", ".STS"):
                (work / (name + suffix)).unlink(missing_ok=True)
            lines += ["ZRXDPMI " + name + ".COM > " + name + ".LOG",
                      "if errorlevel 1 goto fail" + str(number), "echo 0 > " + name + ".STS",
                      "goto done" + str(number), ":fail" + str(number), "echo 1 > " + name + ".STS", ":done" + str(number)]
        lines += ["HOST.COM > AFTER.LOG", "echo OK > STATUS.LOG"]
        ENV["dos"](work, "\n".join(lines) + "\n", args.timeout)
        for name in cases:
            if (work / (name + ".STS")).read_text().strip() != "0":
                failures.append(name + ": nonzero DOS status")
            if (work / (name + ".LOG")).read_text(errors="replace").strip() != EXPECTED[name]:
                failures.append(name + ": unexpected output")
        for name, state in (("BEFORE", "absent"), ("INSIDE", "present"), ("AFTER", "absent")):
            if "External DPMI 1687: " + state not in (work / (name + ".LOG")).read_text():
                failures.append(name + ": incorrect provider lifetime")
    manifest = {"profile": args.profile, "core": args.core if not args.qemu else "qemu-tcg", "build_only": args.build_only, "cases": cases,
                "failures": failures, "source": source_digest.hexdigest(),
                "host": hashlib.sha256((work / "ZRXDPMI.EXE").read_bytes()).hexdigest(),
                "clients": {name: hashlib.sha256((work / (name + ".COM")).read_bytes()).hexdigest() for name in cases}}
    ENV["write"](work / "NATIVE.JSN", json.dumps(manifest, indent=2) + "\n")
    for failure in failures:
        print(failure)
    print("FAIL" if failures else "PASS", args.profile, work)
    return bool(failures)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as exc:
        print("ERROR:", exc, file=sys.stderr)
        sys.exit(1)
