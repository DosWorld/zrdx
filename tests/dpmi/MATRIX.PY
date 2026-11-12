"""The release matrix of docs/BUILD.MD in one command.

Usage: MATRIX.PY --qemu PATH --image SEED.IMG --work DIR

DIR must be inside ZRDX_SCRATCH and not exist. Steps: CHECK.PY
(DOSBox), the native host build, then for RAW, XMS and VCPI the
native QCHECK.PY run and the bound VERIFY.PY + QCHECK.PY run, at
last SIZES.PY. A failed step stops the run. MATRIX.JSN in DIR has
the time of every step.
"""
import argparse
import json
from pathlib import Path
import runpy
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
PY = sys.executable
CFG = runpy.run_path(str(ROOT / "tests/dpmi/ZRCFG.PY"))
CATALOG = runpy.run_path(str(ROOT / "tests/dpmi/CATALOG.PY"))
CASES = CATALOG["NATIVE"]


def step(log, name, command):
    print("== " + name, flush=True)
    start = time.monotonic()
    done = subprocess.run([str(c) for c in command], cwd=ROOT)
    log.append({"step": name, "rc": done.returncode,
                "seconds": round(time.monotonic() - start)})
    if done.returncode:
        raise SystemExit("matrix stopped at " + name)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--qemu", required=True, type=Path)
    ap.add_argument("--image", required=True, type=Path)
    ap.add_argument("--work", required=True, type=Path)
    args = ap.parse_args()
    work = args.work.resolve()
    CFG["check_scratch"](work)
    if work.exists():
        raise SystemExit("work exists: " + str(work))
    work.mkdir(parents=True)
    dpmi = ROOT / "tests/dpmi"
    log = []
    try:
        step(log, "CHECK",
             [PY, ROOT / "tests/CHECK.PY", "--work",
                            work / "load", "--timeout", "3000"])
        step(log, "NATIVE build",
             [PY, dpmi / "NATIVE.PY", "--work",
                                   work / "nat", "--build-only"])
        for profile in ("raw", "xms", "vcpi"):
            nat = work / "nat"
            command = [PY, dpmi / "QCHECK.PY", "--qemu",
                       args.qemu,
                       "--image", args.image, "--fixtures", nat,
                       "--native-host", nat / "ZRDXDPMI.EXE",
                       "--work", work / ("n" + profile),
                       "--profile", profile, "--timeout", "600"]
            extra = [] if profile == "vcpi" else [
                "DRSAVE", "A20R" if profile == "raw" else "A20X"]
            for name in list(CASES) + extra:
                command += ["--case", name]
            step(log, "native " + profile, command)
            bound = work / ("b" + profile)
            step(log, "verify " + profile,
                 [PY, dpmi / "VERIFY.PY", "--work", bound,
                  "--tools",
                  work / "load/bin", "--profile", profile,
                  "--build-only", "--timeout", "600"])
            step(log, "bound " + profile,
                 [PY, dpmi / "QCHECK.PY", "--qemu", args.qemu,
                  "--image", args.image, "--fixtures",
                  bound / "tests/dpmi", "--work", bound / "QEMU",
                  "--profile", profile, "--timeout", "600"])
        step(log, "SIZES", [PY, dpmi / "SIZES.PY", "--work",
                            work / "size", "--native",
                            work / "nat",
                            "--ordinary", work / "load"])
    finally:
        (work / "MATRIX.JSN").write_text(json.dumps(log,
            indent=1))
    print("matrix OK")


if __name__ == "__main__":
    sys.exit(main())
