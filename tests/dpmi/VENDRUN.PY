"""Build the VENDOR variant (task 149) in scratch, test on QEMU.

Same steps as tests/swap/SWAPRUN.PY, with VENDOR enabled and the
VENDOR fixture run on RAW, XMS and VCPI.
"""
import argparse
from pathlib import Path
import runpy
import shutil
import sys

ROOT = Path(__file__).resolve().parents[2]
SW = runpy.run_path(str(ROOT / "tests/swap/SWAPRUN.PY"))
CFG, PY, edit, run = SW["CFG"], SW["PY"], SW["edit"], SW["run"]


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--qemu", required=True, type=Path)
    ap.add_argument("--image", required=True, type=Path)
    ap.add_argument("--work", required=True, type=Path)
    ap.add_argument("--mem", type=int, default=32)
    args = ap.parse_args()
    work = args.work.resolve()
    CFG["check_scratch"](work)
    tree = work / "tree"
    if work.exists():
        shutil.rmtree(work)
    for name in ("src", "bin", "tests", "docs"):
        shutil.copytree(ROOT / name, tree / name)
    edit(tree / "src/segdefs.asm",
         "\r\n;%assign VENDOR 1", "\r\n%assign VENDOR 1")
    run([PY, tree / "tests/CHECK.PY", "--work", work / "sw1",
         "--profile", "raw", "--core", "normal", "--bugs-only"])
    dpmi = ROOT / "tests/dpmi"
    run([PY, dpmi / "VERIFY.PY", "--work", work / "fx",
         "--profile", "raw", "--tools", work / "sw1/bin",
         "--case", "VENDOR", "--build-only"])
    for profile in ("raw", "xms", "vcpi"):
        run([PY, dpmi / "QCHECK.PY", "--qemu", args.qemu,
             "--image", args.image, "--fixtures",
             work / "fx/tests/dpmi", "--profile", profile,
             "--mem", str(args.mem), "--timeout", "180",
             "--work", work / profile, "--case", "VENDOR"])
    print("vendor OK")


if __name__ == "__main__":
    sys.exit(main())
