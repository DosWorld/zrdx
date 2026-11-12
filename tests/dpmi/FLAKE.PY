"""Repeat a test command and measure how often it fails.

Usage: FLAKE.PY --runs N --label NAME --keep DIR [options] --
       COMMAND... ({work} in the command is a private work dir)

The tool of tasks 146 and 177 (unstable tests): every run gets a
fresh work dir inside ZRDX_SCRATCH; a passing run removes it, a
failing run keeps it (QEMU.LOG, MEM.BIN, ...) under --keep. The
load average is read before and after each run, and --max-load
makes a run wait for a quiet machine (a failure under load is
different evidence from one on an idle host). FLAKE.JSN in --keep
has the rate, the loads and the first failure line of every run.
"""
import argparse
import json
import os
from pathlib import Path
import runpy
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
CFG = runpy.run_path(str(ROOT / "tests/dpmi/ZRCFG.PY"))


def wait_quiet(limit, patience):
    """Wait until the 1 minute load is at most LIMIT."""
    start = time.monotonic()
    while limit and os.getloadavg()[0] > limit:
        if time.monotonic() - start > patience:
            return False
        time.sleep(10)
    return True


def first_fail(text):
    for line in text.splitlines():
        if "FAIL" in line or "Error" in line or "error" in line:
            return line.strip()[:160]
    return ""


def one(args, number, keep):
    name = "FLK_%s_%03d" % (args.label, number)
    work = CFG["scratch"]() / name
    shutil.rmtree(work, ignore_errors=True)
    quiet = wait_quiet(args.max_load, args.patience)
    before = os.getloadavg()[0]
    command = [str(x).replace("{work}", str(work))
               for x in args.command]
    start = time.monotonic()
    done = subprocess.run(command, capture_output=True,
                          text=True,
                          timeout=args.timeout)
    row = {"run": number, "rc": done.returncode,
           "seconds": round(time.monotonic() - start, 1),
           "load_before": round(before, 2),
           "load_after": round(os.getloadavg()[0], 2),
           "quiet": quiet}
    if done.returncode:
        row["first_failure"] = first_fail(done.stdout
            + done.stderr)
        target = keep / ("run%03d" % number)
        shutil.rmtree(target, ignore_errors=True)
        target.mkdir(parents=True)
        (target / "OUT.TXT").write_text(done.stdout
                                        + done.stderr)
        if work.exists():
            shutil.move(str(work), str(target / "work"))
    else:
        shutil.rmtree(work, ignore_errors=True)
    return row


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs", type=int, required=True)
    parser.add_argument("--label", required=True)
    parser.add_argument("--keep", type=Path, required=True)
    parser.add_argument("--max-load", type=float, default=0.0)
    parser.add_argument("--patience", type=int, default=1800)
    parser.add_argument("--timeout", type=int, default=3600)
    parser.add_argument("--stop-on-fail", action="store_true")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    args.command = [c for c in args.command if c != "--"]
    keep = args.keep.resolve()
    keep.mkdir(parents=True, exist_ok=True)
    rows = []
    for number in range(1, args.runs + 1):
        rows.append(one(args, number, keep))
        bad = sum(1 for r in rows if r["rc"])
        print("run %d/%d rc=%d load %.1f (%d failed)" % (
            number, args.runs, rows[-1]["rc"],
            rows[-1]["load_before"], bad), flush=True)
        if bad and args.stop_on_fail:
            break
    bad = [r for r in rows if r["rc"]]
    summary = {"label": args.label, "runs": len(rows),
               "failed": len(bad), "rows": rows,
               "command": [str(c) for c in args.command]}
    (keep / "FLAKE.JSN").write_text(json.dumps(summary,
        indent=1))
    print("%s: %d of %d failed" % (args.label, len(bad),
                                   len(rows)))
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
