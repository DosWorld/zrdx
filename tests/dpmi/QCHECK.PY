"""Run source-built DPMI fixtures in a private FreeDOS/QEMU disk image."""
import argparse
import hashlib
import json
from pathlib import Path
import runpy
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
Fat16 = runpy.run_path(str(ROOT / "tests/dpmi/FAT16.PY"))["Fat16"]
CATALOG = runpy.run_path(str(ROOT / "tests/dpmi/CATALOG.PY"))
EXPECTED = dict(CATALOG["BOUND"], HOST=None)
NATIVE_CASES = CATALOG["NATIVE"]
AUX_CASES = CATALOG["AUX"]


def textfile(path, text):
    path.write_bytes(text.replace("\r\n", "\n").replace("\n", "\r\n").encode("ascii", errors="backslashreplace"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--qemu", required=True, type=Path)
    parser.add_argument("--image", type=Path, default=Path("/Users/admin/dosbox-x/D/hdd-fd14.img"))
    parser.add_argument("--fixtures", required=True, type=Path)
    parser.add_argument("--work", required=True, type=Path)
    parser.add_argument("--profile", choices=("raw", "xms", "vcpi"), default="raw")
    parser.add_argument("--case", action="append", choices=tuple(EXPECTED) + tuple(NATIVE_CASES) + tuple(AUX_CASES))
    parser.add_argument("--native-host", type=Path)
    parser.add_argument("--trace", action="store_true")
    parser.add_argument("--icount", action="store_true", help="deterministic virtual-clock benchmark profile")
    parser.add_argument("--timeout", type=int, default=180)
    args = parser.parse_args()
    work = args.work.resolve()
    if not work.is_relative_to(Path("/private/tmp")):
        raise ValueError("work must be an owned /private/tmp directory")
    work.mkdir(parents=True, exist_ok=True)
    disk = work / "TEST.IMG"
    if disk.resolve() == args.image.resolve():
        raise ValueError("cannot overwrite source image")
    shutil.copyfile(args.image, disk)
    volume = Fat16(disk, writable=True)
    available = dict(EXPECTED, **NATIVE_CASES, **AUX_CASES)
    cases = {name: available[name] for name in dict.fromkeys(["HOST"] + args.case)} if args.case else EXPECTED
    inputs = {}
    fixture_manifest = args.fixtures.parent.parent / "D09.JSN"
    if "CONTROL" in cases and fixture_manifest.exists():
        built_profile = json.loads(fixture_manifest.read_text())["profile"]
        if (built_profile == "vcpi") != (args.profile == "vcpi"):
            raise ValueError("CONTROL requires fixtures built for the selected real/VCPI profile")
    if args.native_host:
        data = args.native_host.read_bytes()
        volume.put_root("ZRXDPMI.EXE", data)
        inputs["ZRXDPMI.EXE"] = hashlib.sha256(data).hexdigest()
    payloads = list(cases)
    if "DRSAVE" in cases:
        if not args.native_host or args.profile == "vcpi":
            raise ValueError("DRSAVE needs a native host and real-mode RAW/XMS entry")
        payloads += [n for n in ("BAS32", "NEST32") if n not in payloads]
    if "A20R" in cases:
        if not args.native_host or args.profile != "raw":
            raise ValueError("A20R needs --native-host and --profile raw")
        payloads += [n for n in ("A20RM", "NATL16") if n not in payloads]
    if "A20X" in cases:
        if not args.native_host or args.profile != "xms":
            raise ValueError("A20X needs --native-host and --profile xms")
        payloads += [n for n in ("BRG16", "NATL16") if n not in payloads]
    for name, dependency in (("MAPEXIT", "PHYSMAP"), ("CBEXIT", "CBACK")):
        if name in cases and dependency not in payloads:
            payloads.append(dependency)
    for name in payloads:
        suffix = ".COM" if name in ("HOST", "MAPEXIT", "CBEXIT") or name in NATIVE_CASES or name in AUX_CASES or name == "A20RM" else ".EXE"
        data = (args.fixtures / (name + suffix)).read_bytes()
        volume.put_root(name + suffix, data)
        inputs[name + suffix] = hashlib.sha256(data).hexdigest()
    if set(cases) & {"NATMODE", "NATM32"}:
        for width in (16, 32):
            name = "MODE" + str(width) + ".COM"
            data = (args.fixtures / name).read_bytes()
            volume.put_root(name, data)
            inputs[name] = hashlib.sha256(data).hexdigest()
    for width in (16, 32):
        if "FRT" + str(width) in cases:
            name = "FRC" + str(width) + ".COM"
            data = (args.fixtures / name).read_bytes()
            volume.put_root(name, data)
            inputs[name] = hashlib.sha256(data).hexdigest()
        if "FEX" + str(width) in cases:
            name = "FCH" + str(width) + ".COM"
            data = (args.fixtures / name).read_bytes()
            volume.put_root(name, data)
            inputs[name] = hashlib.sha256(data).hexdigest()
        if "NEST" + str(width) in cases or "IRQ" + str(width) in cases or ("DRSAVE" in cases and width == 32):
            name = "OWNC" + str(width) + ".COM"
            data = (args.fixtures / name).read_bytes()
            volume.put_root(name, data)
            inputs[name] = hashlib.sha256(data).hexdigest()
    subprocess.run([shutil.which("nasm"), "-f", "bin", str(ROOT / "tests/dpmi/QEXIT.ASM"),
                    "-o", str(work / "QEXIT.COM")], check=True)
    volume.put_root("QEXIT.COM", (work / "QEXIT.COM").read_bytes())
    config = ["DOS=LOW", "FILES=40", "BUFFERS=20"]
    if args.icount:
        config += ["SWITCHES=/F"]
    driver = {"raw": None, "xms": "HIMEMX.EXE", "vcpi": "JEMMEX.EXE"}[args.profile]
    if driver:
        config += ["DEVICE=C:" + chr(92) + "FREEDOS" + chr(92) + "BIN" + chr(92) + driver + (" NOEMS X=TEST NOVME NOINVLPG" if args.profile == "vcpi" else "")]
        inputs[driver] = hashlib.sha256(volume.get("FREEDOS/BIN/" + driver)).hexdigest()
    config += [r"SHELL=C:\COMMAND.COM C:\ /E:1024 /P=C:\FDAUTO.BAT"]
    volume.put_root("FDCONFIG.SYS", ("\r\n".join(config) + "\r\n").encode("ascii"))
    batch = ["@echo off", "C:", "cd " + chr(92)]
    for number, name in enumerate(cases):
        suffix = ".COM" if name in ("HOST", "MAPEXIT", "CBEXIT") or name in NATIVE_CASES or name in AUX_CASES or name == "A20RM" else ".EXE"
        prefix = "ZRXDPMI " if args.native_host and name not in ("HOST", "A20X", "A20R", "DRSAVE") else ""
        batch += [prefix + name + suffix + " > " + name + ".LOG",
                  "if errorlevel 1 goto fail" + str(number),
                  "echo 0 > " + name + ".STS", "goto done" + str(number),
                  ":fail" + str(number), "echo 1 > " + name + ".STS", ":done" + str(number)]
    batch += ["echo OK > QDONE.LOG", "QEXIT.COM"]
    volume.put_root("FDAUTO.BAT", ("\r\n".join(batch) + "\r\n").encode("ascii"))
    inputs["KERNEL.SYS"] = hashlib.sha256(volume.get("KERNEL.SYS")).hexdigest()
    inputs["COMMAND.COM"] = hashlib.sha256(volume.get("COMMAND.COM")).hexdigest()
    volume.file.close()
    command = [str(args.qemu), "-machine", "pc", "-accel", "tcg", "-cpu", "pentium",
               "-m", "32", "-display", "none", "-serial", "none", "-monitor", "stdio",
               "-drive", "file=" + str(disk) + ",format=raw,if=ide", "-boot", "c",
               "-no-reboot", "-device", "isa-debug-exit,iobase=0xf4,iosize=0x04"]
    if args.icount:
        command += ["-icount", "shift=3,align=off,sleep=off"]
    if args.trace:
        command += ["-d", "int", "-D", str(work / "TRACE.LOG")]
    timed_out = False
    textfile(work / "COMMAND.TXT", " ".join(command) + "\n")
    with (work / "QEMU.LOG").open("w") as log:
        proc = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=log, stderr=subprocess.STDOUT)
        try:
            status = proc.wait(timeout=args.timeout)
        except subprocess.TimeoutExpired:
            timed_out = True
            try:
                proc.stdin.write(('info registers\npmemsave 0xb8000 4000 "' + str(work / "VGA.BIN") + '"\npmemsave 0 0x800000 "' + str(work / "MEM.BIN") + '"\n').encode("ascii"))
                proc.stdin.flush()
                time.sleep(1)
            finally:
                proc.kill()
                proc.wait()
            vga = work / "VGA.BIN"
            if vga.exists():
                data = vga.read_bytes()[::2]
                textfile(work / "VGA.TXT", "\n".join(data[n:n+80].decode("cp437") for n in range(0, len(data), 80)) + "\n")
            status = proc.returncode
    # isa-debug-exit maps guest value zero to host status one.
    volume = Fat16(disk)
    failures = []
    if timed_out:
        failures.append("QEMU watchdog expired")
    if status != 1:
        failures.append("unexpected QEMU exit status " + str(status))
    try:
        completed = volume.get("QDONE.LOG").strip() == b"OK"
    except FileNotFoundError:
        completed = False
    if not completed:
        failures.append("FreeDOS campaign incomplete")
    outputs = {}
    for name, expected in cases.items():
        try:
            output = volume.get(name + ".LOG").decode("ascii", errors="replace").replace("\r\n", "\n").strip()
        except FileNotFoundError:
            output = "missing output"
        try:
            case_status = volume.get(name + ".STS").strip()
        except FileNotFoundError:
            case_status = b"missing"
        outputs[name] = output
        textfile(work / (name + ".LOG"), output + "\n")
        if case_status != b"0" or (expected is not None and output != expected):
            failures.append(name + " failed")
    provider = outputs["HOST"]
    if "External DPMI 1687: absent" not in provider:
        failures.append("unexpected external DPMI provider")
    for service, available in (("VCPI DE00", args.profile == "vcpi"), ("XMS 4300", args.profile != "raw")):
        if service + (": present" if available else ": absent") not in provider:
            failures.append("unexpected provider: " + service)
    version = subprocess.run([str(args.qemu), "--version"], capture_output=True, text=True, check=True).stdout.splitlines()[0]
    build_path = args.fixtures / "NATIVE.JSN" if args.native_host else fixture_manifest
    build = json.loads(build_path.read_text()) if build_path.exists() else {}
    manifest = {"schema": "qemu-1", "completed": completed and status == 1 and not timed_out,
                "source": build.get("source"), "tools": build.get("tools", {}), "profile": args.profile, "icount": args.icount, "icount_shift": 3 if args.icount else None, "qemu": version, "inputs": inputs, "provider": provider, "timed_out": timed_out, "native": bool(args.native_host), "cases": list(cases), "failures": failures}
    textfile(work / "RESULT.JSN", json.dumps(manifest, indent=2) + "\n")
    print(provider)
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
