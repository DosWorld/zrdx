"""Byte map of the 16-bit segments (Text16, IText16, IData16).

Assembles the four roots in a scratch copy of src with a NASM
listing and sums the bytes per label (a label covers the code up
to the next label; db/dw/times data shows only partly in a
listing, so totals are about 10% short). Use it to pick what to shrink (task 142).

  BYTEMAP.PY --work <scratch dir> [--swap] [--top N]
"""
import argparse
from pathlib import Path
import re
import runpy
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[2]
CFG = runpy.run_path(str(ROOT / "tests/dpmi/ZRCFG.PY"))
ROOTS = (("native", "zrdxdpmi"), ("le", "zrdx"))
SEGS = ("Text16", "IText16", "IData16")
LINE = re.compile(r"\s*\d+\s+([0-9A-F]{8})\s+([0-9A-F]+)\s+(<\d+>)?(.*)")
LABEL = re.compile(r"\s*([A-Za-z_][\w@$]*):")
NOADDR = re.compile(r"\s*\d+\s+(<\d+>)?\s*(.*)")


def enable(src, swap):
    seg = src / "segdefs.asm"
    text = seg.read_text()
    if swap:
        text = text.replace("\n;%assign SWAP 1", "\n%assign SWAP 1")
    # lift every 16-bit ceiling: only the sizes are wanted
    text = re.sub(r"SetupRTSeg \d+, \d+, (Text16|IText16)",
                  r"SetupRTSeg 6000, 6000, \1", text)
    seg.write_bytes(text.replace("\r\n", "\n").replace("\n", "\r\n").encode())


def scan(listing):
    stack = ["none"]
    sizes = {name: {} for name in SEGS}
    label = {name: "(top)" for name in SEGS}
    for line in listing.read_text(errors="replace").splitlines():
        match = LINE.match(line)
        body = match.group(4) if match else None
        nest = match.group(3) if match else None
        if match is None:
            other = NOADDR.match(line)
            if not other:
                continue
            nest, body = other.groups()
        mark = re.match(r"\s*(SEGM|ESEG|segment)\s+(\w+)", body)
        if mark and nest in (None, "<1>") and "%" not in body:
            word, name = mark.groups()
            if word == "SEGM":
                stack.append(name)
            elif word == "ESEG" and len(stack) > 1:
                stack.pop()
            elif word == "segment":
                stack[-1] = name
            continue
        cur = stack[-1]
        if cur not in SEGS:
            continue
        found = LABEL.match(body)
        if found and not found.group(1).startswith("."):
            label[cur] = found.group(1)
        if match:
            count = len(match.group(2)) // 2
            sizes[cur][label[cur]] = sizes[cur].get(label[cur], 0) + count
    return sizes


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work", required=True, type=Path)
    parser.add_argument("--swap", action="store_true")
    parser.add_argument("--top", type=int, default=12)
    args = parser.parse_args()
    work = args.work.resolve()
    CFG["check_scratch"](work)
    if work.exists():
        shutil.rmtree(work)
    src = work / "src"
    shutil.copytree(ROOT / "src", src)
    enable(src, args.swap)
    for name, root in ROOTS:
        listing = work / (name + ".LST")
        subprocess.run([shutil.which("nasm"), "-O9", "-f", "obj", "-l", str(listing),
                        root + ".asm", "-o", str(work / (name + ".OBJ"))],
                       cwd=src, capture_output=True, check=True)
        sizes = scan(listing)
        for seg in SEGS:
            print("%s %s%s: %d bytes" % (name, seg, " (SWAP)" if args.swap else "",
                                         sum(sizes[seg].values())))
            for label, count in sorted(sizes[seg].items(), key=lambda x: -x[1])[:args.top]:
                print("  %5d %s" % (count, label))


if __name__ == "__main__":
    main()
