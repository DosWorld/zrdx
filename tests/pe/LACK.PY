"""Host without a DPMI function (task 169), test build FAKELACK.

Builds zrdxnp.asm with -DFAKELACK=N (the N-th trampoline
allocation fails like a host answering 8001h), links it in
DOSBox-X and runs fixtures on QEMU through that image:
N=1 breaks the start-up bind (LOWMEM must print the loader
error), N=2 breaks the first LoadLibrary (LACK must print ok).
Needs a CHECK.PY scratch with built fixtures (--fixtures).
"""
import argparse
import runpy
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ENV = runpy.run_path(str(ROOT / "tests/CHECK.PY"))


def build(work, level):
    src = work / ("src" + str(level))
    shutil.copytree(ROOT / "src", src, dirs_exist_ok=True)
    subprocess.run([shutil.which("nasm"), "-O9", "-f", "obj", "-DFAKELACK=" + str(level),
                    "zrdxnp.asm", "-o", "zrdxnp.obj"], cwd=src, check=True,
                   stderr=subprocess.DEVNULL)
    ENV["config"](work, "xms", False)
    ENV["dos"](work, "\n".join([
        "@echo off", "cd src" + str(level),
        "wlink option quiet name fake.exe format dos file zrdxnp.obj > LINK.LOG",
        "if errorlevel 1 goto bad", "cd ..", "echo OK > STATUS.LOG", "goto end",
        ":bad", "cd R:" + chr(92), "echo FAILED > STATUS.LOG", ":end"]) + "\n", 300)
    return src / "FAKE.EXE"


def run(args, work, level, case, want):
    image = build(work, level)
    out = work / ("Q" + str(level))
    shutil.rmtree(out, ignore_errors=True)
    subprocess.run([sys.executable, str(ROOT / "tests/pe/QPE.PY"), "--qemu", str(args.qemu),
                    "--fixtures", str(args.fixtures), "--host", str(image), "--work", str(out),
                    "--profile", "xms", "--case", case], stdout=subprocess.DEVNULL)
    text = (out / (case + ".LOG")).read_text(errors="replace")
    ok = want in text
    print(("PASS" if ok else "FAIL"), "FAKELACK=" + str(level), case, text.strip().splitlines()[-1:])
    return not ok


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--qemu", required=True, type=Path)
    parser.add_argument("--fixtures", required=True, type=Path)
    parser.add_argument("--work", required=True, type=Path)
    args = parser.parse_args()
    ENV["CFG"]["check_scratch"](args.work.resolve())
    args.work.mkdir(parents=True, exist_ok=True)
    work = args.work.resolve()
    bad = run(args, work, 1, "LOWMEM", "DPMI host lacks a function")
    bad |= run(args, work, 2, "LACK", "PE lack: ok")
    return bad


if __name__ == "__main__":
    sys.exit(main())
