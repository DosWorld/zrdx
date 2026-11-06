"""Build the SWAP variant in scratch and run SWAPT on QEMU.

Needs the ZRDX_* variables of tests/dpmi/ZRCFG.PY. --work must
be inside ZRDX_SCRATCH. Steps: copy the tree, enable SWAP and
the larger stub ceiling, run CHECK.PY (regression), build the
SWAPT fixture with the SWAP stub, run it on QEMU RAW and XMS.
"""
import argparse
from pathlib import Path
import re
import runpy
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
PY = sys.executable
CFG = runpy.run_path(str(ROOT / "tests/dpmi/ZRCFG.PY"))


def edit(path, old, new):
    text = path.read_bytes().decode()
    if old not in text:
        raise RuntimeError("missing in " + path.name)
    path.write_bytes(text.replace(old, new, 1).encode())


def run(args):
    print(" ".join(map(str, args)), flush=True)
    subprocess.run(args, check=True)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--qemu", required=True, type=Path)
    ap.add_argument("--image", required=True, type=Path)
    ap.add_argument("--work", required=True, type=Path)
    ap.add_argument("--mem", type=int, default=8)
    ap.add_argument("--fail", action="store_true",
                    help="SWAPFAIL build: disk error cases")
    args = ap.parse_args()
    work = args.work.resolve()
    CFG["check_scratch"](work)
    tree = work / "tree"
    if work.exists():
        shutil.rmtree(work)
    for name in ("src", "bin", "tests", "docs"):
        shutil.copytree(ROOT / name, tree / name)
    edit(tree / "src/segdefs.asm",
         "\r\n;%assign SWAP 1", "\r\n%assign SWAP 1")
    if args.fail:
        edit(tree / "src/segdefs.asm",
             "\r\n;%assign SWAPFAIL 1", "\r\n%assign SWAPFAIL 1")
    edit(tree / "src/makefile",
         "PSTUBSZ = 14336", "PSTUBSZ = 15872")
    check = ["--profile", "raw", "--core", "normal",
             "--bugs-only"]
    run([PY, tree / "tests/CHECK.PY", "--work", work / "sw1"]
        + check)
    dpmi = ROOT / "tests/dpmi"
    cases = ("SWAPW", "SWAPR") if args.fail else (
        "SWAPT", "SWAPF", "SWAPC", "SWAPK")
    case_args = [a for name in cases for a in ("--case", name)]
    run([PY, dpmi / "VERIFY.PY", "--work", work / "fx",
         "--profile", "raw", "--tools", work / "sw1/bin"]
        + case_args + ["--case", "SWAPN", "--case", "SWAPCH",
                       "--build-only"])
    for profile in (("raw",) if args.fail else ("raw", "xms")):
        common = [PY, dpmi / "QCHECK.PY", "--qemu", args.qemu,
                  "--image", args.image, "--fixtures",
                  work / "fx/tests/dpmi", "--profile", profile,
                  "--mem", str(args.mem), "--timeout", "600"]
        run(common + ["--work", work / profile] + case_args
            + ([] if args.fail else ["--case", "SWAPN"]))
        run(common + ["--work", work / (profile + "n"),
                      "--native-host", work / "sw1/bin/zrdxdpmi.exe"]
            + case_args
            + ([] if args.fail else ["--case", "SWAPN"]))
    print("swap OK")


if __name__ == "__main__":
    sys.exit(main())
