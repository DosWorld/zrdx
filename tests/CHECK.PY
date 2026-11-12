"""Build host NASM OMF, run DOS tools and regression fixtures in
scratch.
"""
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
WIN32_CASES = ("T1", "W", "TAPI", "WB", "TPROC", "TTEB",
               "TSEH", "TUNW", "TTLS", "TTLD", "TUSR", "TKBS",
               "TNTD", "TADV", "TSHL", "TMFN", "TMFB", "TAMS",
               "TSEHX", "TMEM", "TFS", "TTXT", "TCRT", "TIO",
               "TFILE", "TFMT", "TSCAN", "TLIB", "TMATH",
               "TWCS", "TUCRT")
TOOLS = ("zrdxbind.com", "zrdxconf.exe", "stub-le.exe",
         "stub-pe.exe", "stub-pmx.exe", "stub-rdf.exe",
         "zrdxdpmi.exe")


def write(path, text):
    path.write_bytes(text.replace("\r\n", "\n").replace("\n",
        "\r\n").encode("ascii"))


def command(args, cwd):
    result = subprocess.run(args, cwd=cwd, capture_output=True,
                            text=True)
    if result.stderr:
        print(result.stderr, end="", flush=True)
    if result.returncode:
        raise RuntimeError("command failed: "
                           + " ".join(map(str, args)))
    return result.stdout.strip()


def fingerprint():
    digest = hashlib.sha256()
    for path in sorted((ROOT / "src").rglob("*")):
        if path.is_file():
            digest.update(str(path.relative_to(ROOT)).encode())
            digest.update(path.read_bytes())
    return digest.hexdigest()


def config(work, profile, real_clip, lfn="true", core="auto",
           hdpmi=False):
    original = CFG["require"]("ZRDX_DOSCONF").read_text()
    write(work / "USER.CON", original)
    base = re.split(r"(?m)^\[autoexec\]\s*$", original)[0]
    values = {"core": core, "lfn": lfn, "cycles": "max",
              "xms": "false" if profile == "raw" else "true",
              "ems": "emm386" if profile == "vcpi" else "false",
              "dos clipboard api": "true" if real_clip else
              "false"}
    for key, value in values.items():
        base = re.sub(r"(?m)^" + re.escape(key) + r"\s*=.*$",
                      key + " = " + value, base)
    # Place advanced DOS settings inside [dos], not after another
    # section.
    base = base.replace("[dos]\n",
                        "[dos]\nvcpi = true\nemm386 startup "
                        "active = " +
                        ("true" if profile
                         == "vcpi" else "false") + "\n")
    hdpmi_line = ("C:\\HX\\BIN\\HDPMI32.EXE" if hdpmi
                  else "rem built-in host")
    auto = r"""[autoexec]
@echo off
mount -q C DRIVEC
mount -q D DRIVED
mount -q R WORKDIR
set WATCOM=D:\WATCOM
set INCLUDE=D:\WATCOM\H
set EDPATH=D:\WATCOM\EDDAT
set PATH=C:\BIN;D:\WATCOM\BINW;Z:\;Z:\SYSTEM;Z:\BIN
HDPMILINE
R:
call AUDIT.BAT
exit
""".replace("HDPMILINE", hdpmi_line)
    for key, val in (("WORKDIR", str(work)),
                     ("DRIVEC", str(CFG["require"](
                         "ZRDX_DRIVE_C"))),
                     ("DRIVED", str(CFG["require"](
                         "ZRDX_DRIVE_D")))):
        auto = auto.replace(key, val)
    write(work / "DOSBOX.CON", base + auto)


def stage_cc(src, dst):
    """Copy the supplied Win32 compiler tools (task 184)."""
    dst.mkdir(exist_ok=True)
    for name in ("CC32W.EXE", "LINK32W.EXE", "LIB32W.EXE"):
        shutil.copy2(src / name, dst / name)
    for sub in ("include", "lib"):
        shutil.copytree(src / sub, dst / sub, dirs_exist_ok=True)
    shutil.copy2(src / "lib" / "WIN.DEF", dst / "WIN.DEF")
    shutil.copy2(src / "lib" / "LIBC32W.RDL",
                 dst / "LIBC32W.RDL")


def dos(work, batch, timeout):
    write(work / "AUDIT.BAT", batch)
    (work / "STATUS.LOG").unlink(missing_ok=True)
    (work / "STOP.TXT").unlink(missing_ok=True)
    env = dict(os.environ, SDL_VIDEODRIVER="dummy",
               SDL_AUDIODRIVER="dummy")
    with (work / "HOST.LOG").open("w") as log:
        proc = subprocess.Popen([CFG["dosbox"](), "-conf",
                                 str(work / "DOSBOX.CON"),
                                 "-nogui", "-nomenu"],
                                stdout=log,
                                stderr=subprocess.STDOUT,
                                env=env)
        write(work / "DOSBOX.PID", str(proc.pid) + "\n")
        deadline = time.monotonic() + timeout
        while proc.poll() is None:
            stop = (work / "STOP.TXT").exists()
            big = (work / "HOST.LOG").stat().st_size > 4000000
            if stop or time.monotonic() >= deadline or big:
                proc.kill()
                proc.wait()
                raise RuntimeError(
                    "DOSBox watchdog/fault log limit; see "
                                   + str(work / "HOST.LOG"))
            time.sleep(0.2)
        status = proc.returncode
    if status:
        raise RuntimeError("DOSBox exited with host status "
                           + str(status))
    statusfile = work / "STATUS.LOG"
    if (
        not statusfile.exists()
        or statusfile.read_text().strip() != "OK"):
        raise RuntimeError(
            "DOS batch failed; inspect scratch logs")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work", type=Path)
    parser.add_argument("--profile",
                        choices=("xms", "raw", "vcpi"),
                        default="xms")
    parser.add_argument("--core",
                        choices=("auto", "normal", "dynamic",
                                 "full"), default="auto")
    parser.add_argument("--lfn",
                        choices=("true", "false", "auto"),
                        default="true",
                        help="LFN in the private config; true "
                             "requires both T40 cases to run")
    parser.add_argument("--skip-build", action="store_true")
    parser.add_argument("--build-only", action="store_true",
                        help="stop after the build and the "
                             "tool check")
    parser.add_argument("--supplied", action="store_true",
                        help="diagnose supplied binaries "
                             "instead of rebuilding")
    parser.add_argument("--bugs-only", action="store_true",
                        help="skip the long existing "
                             "regression campaign")
    parser.add_argument("--real-clip", action="store_true",
                        help="use the DOSBox host clipboard "
                             "instead of the fixture")
    parser.add_argument("--hdpmi", action="store_true",
                        help="run only the PE fixtures with "
                             "HDPMI32 as external DPMI host")
    parser.add_argument("--timeout", type=int, default=900)
    args = parser.parse_args()
    work = (args.work or Path(tempfile.mkdtemp(prefix="ZRDX",
        dir=CFG["scratch"]()))).resolve()
    if work == ROOT or ROOT in work.parents:
        raise RuntimeError("work must be outside the checkout")
    work.mkdir(parents=True, exist_ok=True)
    print("Scratch:", work, flush=True)
    source_id = fingerprint()
    if args.skip_build:
        if (work
            / "BUILD.ID").read_text().strip() != fingerprint():
            raise RuntimeError(
                "source changed since scratch build; rebuild "
                "first")
    else:
        for name in ("src", "bin", "sdk", "win32"):
            shutil.copytree(ROOT / name, work / name,
                            dirs_exist_ok=True)
    shutil.copytree(ROOT / "tests", work / "tests",
                    dirs_exist_ok=True)
    config(work, "xms", False, lfn=args.lfn, core=args.core)
    nasm = shutil.which("nasm")
    print(command([nasm, "-v"], ROOT), flush=True)
    if not args.skip_build and not args.supplied:
        for stem in ("zrdx", "zrdxpe", "zrdxpm", "zrdxrdf",
                     "zrdxnp"):
            target = work / "src" / (stem + ".obj")
            temporary = work / "src/NASM.NEW"
            command([nasm, "-O9", "-f", "obj", stem + ".asm",
                     "-o", temporary.name], work / "src")
            if (
                not target.exists() or target.read_bytes()
                != temporary.read_bytes()):
                temporary.replace(target)
            else:
                temporary.unlink()
                # copied headers must not trigger DOS reassembly
                target.touch()
        command([nasm, "-O9", "-f", "bin", "binder/bnd.asm",
                 "-o", "binder/zrdxbind.com"], work / "src")
        dos(work, r"""@echo off
cd src
wmake all > BUILD.LOG
if errorlevel 1 goto bad
upx -9 --8086 binder\zrdxbind.com >> BUILD.LOG
if errorlevel 1 goto bad
wmake install >> BUILD.LOG
if errorlevel 1 goto bad
cd ..\tests\setup
call ALTEST.BAT
cd ..\pe\c
call BUILD.BAT
cd ..\..\..
echo OK > STATUS.LOG
goto end
:bad
cd ..
echo FAILED > STATUS.LOG
:end
""", args.timeout)
        if not (work / "tests/setup/ALTEST.RES").exists():
            raise RuntimeError(
                "wmake did not stop on a failed stub")
        write(work / "BUILD.ID", source_id + "\n")
        print("Fresh build OK", flush=True)
    for file in TOOLS:
        path = work / "bin" / file
        if not path.is_file():
            raise RuntimeError("missing tool " + file)
    if args.build_only:
        print("PASS build only", work, flush=True)
        return 0
    command([nasm, "-f", "bin", "HELLO.ASM", "-o",
             "HELLO.BIN"], work / "tests/pe")
    for out, base in (("RELOC", "0x401000"),
                      ("RELOC2", "0x501000"),
                      ("RELOCH", "0x10001000")):
        command([nasm, "-f", "bin", "-DBASE=" + base,
                 "RELOC.ASM", "-o", out + ".BIN"],
                work / "tests/pe")
    pe_gen = runpy.run_path(str(work / "tests/pe/GEN.PY"))
    for stem, src, defs in pe_gen["JOBS"]:
        for extra, out in (((), stem + ".BIN"),
                           (("-DBASE=0x501000",),
                            stem + "2.BIN")):
            command([nasm, "-f", "bin", *defs, *extra, src,
                     "-o", out], work / "tests/pe")
    pe_cases = pe_gen["generate"](work / "tests/pe")
    if (work / "tests/mz/GEN.PY").exists():
        runpy.run_path(str(work
                           / "tests/mz/GEN.PY"))["generate"](
                               work / "tests/mz")
    bind_cases = runpy.run_path(str(work
                                    / "tests/bind/GEN.PY"))[
                                        "generate"](work
                                            / "tests/bind")
    config(work, args.profile, args.real_clip, lfn=args.lfn,
           core=args.core, hdpmi=args.hdpmi)
    mock_line = ("rem host clipboard" if args.real_clip
                 else "MOCK.COM")
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
call RUN.BAT P1 > SCREEN.LOG
copy RESULT.LOG RESULT.PE
copy R:\BIN\stub-pe.exe R:\BIN\stub-pe.sav
copy R:\BIN\stub-pmx.exe R:\BIN\stub-pe.exe
call RUN.BAT P2 > SCREEN2.LOG
copy RESULT.LOG RESULT.PMX
copy R:\BIN\stub-pe.sav R:\BIN\stub-pe.exe
call UNB.BAT > SCREEN3.LOG
copy RESULT.LOG RESULT.UNB
UNBNLINE
UNBHLINE
MZLINE
WINLINE
cd R:\
echo OK > STATUS.LOG
goto end
:bad
cd R:\
echo FAILED > STATUS.LOG
:end
""".replace("MOCKLINE", mock_line)
    mz_line = "rem no MZ fixtures"
    if (work / "tests/mz/RUN.BAT").exists():
        mz_line = "cd ..\\mz\ncall RUN.BAT > SCREEN.LOG"
    batch = batch.replace("MZLINE", mz_line)
    batch = batch.replace("echo OK > STATUS.LOG",
                          "cd tests\\bind\ncall RUN.BAT > "
                          "SCREEN.LOG\ncd ..\\..\necho OK > "
                          "STATUS.LOG")
    batch = batch.replace("WINLINE",
                          "cd ..\\win32\ncall RUN.BAT > "
                          "SCREEN.LOG")
    command([sys.executable, "-I",
             str(ROOT / "win32/GENFWD.PY"),
             str(ROOT / "win32/KERNEL32.LNK"),
             str(work / "tests/win32/KERNELBASE.DLL"),
             "KERNELBASE.DLL"], ROOT)
    ccdir = os.environ.get("ZRDX_CCDIR")
    if ccdir:
        stage_cc(Path(ccdir), work / "tests/win32/CC")
    if args.bugs_only:
        batch = batch.replace("call BUILD.BAT > BUILD.LOG",
                              "rem skip legacy build")
    batch = batch.replace("UNBHLINE",
                          "rem host role needs the own host"
                          if args.hdpmi else
                          "call UNBH.BAT > SCREEN5.LOG\ncopy "
                          "RESULT.LOG RESULT.UNH")
    batch = batch.replace("UNBNLINE",
                          "call UNBN.BAT > SCREEN4.LOG\ncopy "
                          "RESULT.LOG RESULT.UNN" if args.
                          hdpmi else
                          "rem client role needs an external "
                          "host")
    if args.hdpmi:
        batch = batch.replace("call PROBE.BAT > BUILD.LOG",
                              "rem own DPMI probes do not apply")
    dos(work, batch, args.timeout)
    errors = []
    for name in (() if args.hdpmi else ("PROBE", "UNSUP",
                                        "CBACK", "PHYSMAP",
                                        "MAPEXIT", "CBEXIT")):
        text = (work / "tests/dpmi"
                / (name + ".LOG")).read_text(errors="replace")
        good = text.count(
            "PASS") == 3 if name == "PROBE" else ": ok" in text
        if not good or "FAIL" in text:
            errors.append(name + " failed: " + text.strip())
    for tag, label in (("PE", "PE"), ("PMX", "PMX"),
                       ("UNB", "UNB")) + ((("UNN", "UNN"),
                           ) if args.hdpmi else (("UNH",
                               "UNH"),)):
        pe = (work / "tests/pe"
              / ("RESULT." + tag)).read_text(errors="replace")
        cases = [n for n in pe_cases if tag != "PE"
                 or n not in pe_gen["PMXONLY"]]
        valid = [n for n in pe_gen["VALID"] if n in cases]
        for name in cases:
            if (
                name + " OK" not in pe or name
                + " FAILED" in pe or name
                + " BIND_FAILED" in pe):
                errors.append(label + " " + name + " failed")
        block = []
        for line in pe.splitlines():
            word = line.split()
            if len(word) == 2 and word[1] in ("OK",
                "FAILED") and word[0] in cases:
                want = pe_gen["EXPECT"].get(word[0])
                for part in ((want,
                             ) if isinstance(want,
                                 str) else want or ()):
                    if not any(part in text for text in block):
                        errors.append(label + " " + word[0]
                                      + " text missing: " + part)
                block = []
            else:
                block.append(line)
        if (pe.count("ZRDX loader error:") != len(cases)
            - len(valid)
                or "PE single module: ok" not in pe
            or pe.count("PE reloc: ok") != 2
                or (tag != "PE" and ("PE modules: ok" not in pe
                                   or "PE exports: ok" not in pe
                                   or "PE exports bad: ok" not
                                     in pe))
                or pe.count("PE import graph: ok")
            != len(pe_gen["GRAPH"])
                or pe.count("PE init: ok")
            != len(pe_gen["INITS"])
                or pe.count("PE runtime: ok")
            != len(pe_gen["RUNTIME"])
                or pe.count("PE unload: ok")
            != len(pe_gen["UNLOAD"])
                or pe.count("PE low memory: ok")
            != len(pe_gen["LOWMEMS"])):
            errors.append(label
                          + " expected loader diagnostics "
                            "missing")
    stubs = {n: (work / "bin"
                 / n).read_bytes()[:4096] for n in (
                     "stub-pe.exe", "stub-pmx.exe")}
    if stubs["stub-pe.exe"] == stubs["stub-pmx.exe"]:
        errors.append("PE stubs are not distinguishable")
    for name in [
        n for n in pe_cases if n not in pe_gen["PMXONLY"]]:
        data = (work / "tests/pe" / (name + ".PE")).read_bytes()
        want = "stub-pmx.exe" if pe_gen["wants_pmx"](
            data) else "stub-pe.exe"
        bound = (work / "tests/pe"
                 / (name + ".P1")).read_bytes()[:4096]
        if bound != stubs[want]:
            errors.append("PE " + name + " bound with the "
                          "wrong stub, want " + want)
    mz = work / "tests/mz/RESULT.LOG"
    if mz.exists() and "FAILED" in mz.read_text():
        errors.append("MZ aligner fixtures failed")
    if mz.exists():
        try:
            runpy.run_path(str(work
                               / "tests/mz/GEN.PY"))["verify"](
                                   work / "tests/mz")
        except RuntimeError as exc:
            errors.append(str(exc))
    win = (work
           / "tests/win32/RESULT.LOG").read_text(
               errors="replace")
    for name in WIN32_CASES:
        if name + " OK" not in win or name + " FAILED" in win:
            errors.append("win32 " + name + " failed")
    for log, want in (("W.LOG", "hello"), ("WB.LOG", "hello"),
                      ("TAPI.LOG", "API: ok"),
                      ("TPROC.LOG", "PROC: ok"),
                      ("TTEB.LOG", "TEB: ok"),
                      ("TSEH.LOG", "SEH: ok"),
                      ("TUNW.LOG", "UNW: ok"),
                      ("TTLS.LOG", "TLS: ok"),
                      ("TTLD.LOG", "TLD: ok"),
                      ("TTLD.LOG",
                       "TLSD cb detach\r\nTLSD main detach"),
                      ("TKBS.LOG", "KBS: ok"),
                      ("TNTD.LOG", "NTD: ok"),
                      ("TADV.LOG", "ADV: ok"),
                      ("TMFN.LOG", "TMFN.EXE|"),
                      ("TMFB.LOG", "TMFB.EXE|"),
                      ("TSHL.LOG", "SHL: ok"),
                      ("TUSR.LOG", "[cap] text"),
                      ("TUSR.LOG", "[wcap] wtext"),
                      ("TUSR.LOG", "USR: ok"),
                      ("TSEHX.LOG",
                       "Unhandled exception C0000094"),
                      ("TMEM.LOG", "MEM: ok"),
                      ("TFS.LOG", "FS: ok"),
                      ("TTXT.LOG", "TXT: ok"),
                      ("TCRT.LOG", "CRT: ok\r\nC\r\nB\r\nA"),
                      ("TIO.LOG", "IO: ok"),
                      ("TFILE.LOG", "puts-line\r\nP\r\n"),
                      ("TFILE.LOG", "FILE: ok"),
                      ("TFMT.LOG", "FMT: ok"),
                      ("TSCAN.LOG", "SCAN: ok"),
                      ("TLIB.LOG", "LIB: ok"),
                      ("TMATH.LOG", "MATH: ok"),
                      ("TWCS.LOG", "WCS: ok"),
                      ("TAMS.LOG", "UCRT: ok"),
                      ("TUCRT.LOG", "UCRT: ok")):
        text = (work / "tests/win32" / log).read_text(
            errors="replace").replace("\r\n", "\n")
        if want.replace("\r\n", "\n") not in text:
            errors.append("win32 " + log + " lacks " + want)
    if ccdir:
        for name in ("CCRET", "CCPRN", "CCLIB"):
            if (
                name + " OK" not in win or name
                + " FAILED" in win):
                errors.append("win32 " + name + " failed")
        cc = work / "tests/win32/CC"
        out = (cc / "CCPRN3.LOG").read_text(errors="replace")
        if "hello from cc 42" not in out:
            errors.append(
                "win32 CCPRN3.LOG lacks the program output")
        for log, want in (("CCLIB2.LOG", "CCPRN"),
                          ("CCLIB3.LOG", "__llmul")):
            if want not in (cc
                            / log).read_text(errors="replace"):
                errors.append("win32 " + log + " lacks " + want)
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
""".replace("MOCKLINE", mock_line), args.timeout)
        rdf = (work
               / "tests/rdf/RUN.LOG").read_text(errors="replace")
        build = (work
                 / "tests/rdf/BUILD.LOG").read_text(
                     errors="replace")
        if "FAILED" in rdf or "BUILD FAILED" in build:
            errors.append("existing RDF regressions failed")
        if "T59X LDT exit cleanup: ok" not in rdf:
            errors.append("existing regression run incomplete")
        if not args.real_clip and "no clipboard" in rdf:
            errors.append(
                "clipboard fixtures were unexpectedly skipped")
        if args.lfn == "true" and rdf.count("T40 lfn: ok") != 2:
            errors.append(
                "LFN enabled: both T40 formats must pass "
                "without skips")
    legacy_cases = []
    if not args.bugs_only and not errors:
        legacy_cases = sorted({line.split()[0]
                               for line in rdf.splitlines()
                               if line.endswith("ok")})
    info = {"cases": legacy_cases, "schema": "legacy-1",
            "completed": not errors and not args.bugs_only,
            "source": source_id,
            "ok_lines": sum(
                line.endswith(
                    "ok") for line in rdf.splitlines()) if not
            args.bugs_only and not errors else 0,
            "lfn_passes": rdf.count(
                "T40 lfn: ok") if not args.bugs_only
            and not errors else 0,
            "profile": args.profile, "core": args.core,
            "lfn": args.lfn, "supplied": args.supplied,
            "clipboard": "host" if args.real_clip else "mock",
            "tools": {n: hashlib.sha256((work / "bin"
                / n).read_bytes()).hexdigest() for n in TOOLS},
            "provider": (work
                         / "tests/dpmi/HOST.LOG").read_text(),
            "errors": errors}
    write(work / "RESULT.JSN", json.dumps(info, indent=2) + "\n")
    print(info["provider"], end="")
    for error in errors:
        print(error)
    print("FAILED" if errors else "PASS", "profile",
          args.profile, "scratch", work, flush=True)
    return bool(errors)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, RuntimeError,
            subprocess.SubprocessError) as exc:
        print("ERROR:", exc, file=sys.stderr)
        sys.exit(1)
