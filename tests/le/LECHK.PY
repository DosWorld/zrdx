"""LE regression: invalid object flags must not hang.

Builds an LE with RLINK, adds a selector fixup to its last
object (RLINK drops "dw seg"), clears that object's flags in a
copy and expects exit 255 instead of a loop (task 129).
Self test: --self.
"""
import argparse
from pathlib import Path
import runpy
import shutil
import struct
import sys

ROOT = Path(__file__).resolve().parent.parent.parent
OBJSIZE = 24
FLAGS = 8


def le_start(data):
    start = struct.unpack_from("<I", data, 0x3C)[0]
    if data[start:start + 2] != b"LE":
        raise ValueError("no LE header")
    return start


def selector_targets(data):
    """Return 1-based target objects of selector fixups."""
    s = le_start(data)
    npages = struct.unpack_from("<I", data, s + 0x14)[0]
    fixpages = s + struct.unpack_from("<I", data, s + 0x68)[0]
    fixrecs = s + struct.unpack_from("<I", data, s + 0x6C)[0]
    fmt = "<%dI" % (npages + 1)
    pages = struct.unpack_from(fmt, data, fixpages)
    found = []
    pos, end = fixrecs + pages[0], fixrecs + pages[npages]
    while pos < end:
        src, flags = data[pos], data[pos + 1]
        size = 4
        obj = data[pos + 4]
        if flags & 0x40:
            obj = data[pos + 4] | (data[pos + 5] << 8)
            size += 2
        else:
            size += 1
        if src & 15 == 2:
            found.append(obj)
        else:
            size += 4 if flags & 0x10 else 2
        pos += size
    return found


def add_selector_fixup(data, srcoff, obj):
    """Write one selector fixup (page 1 -> object obj) into the
    free space before the data pages of an LE without fixups."""
    s = le_start(data)
    npages = struct.unpack_from("<I", data, s + 0x14)[0]
    fixpages = s + struct.unpack_from("<I", data, s + 0x68)[0]
    fixrecs = s + struct.unpack_from("<I", data, s + 0x6C)[0]
    datapages = struct.unpack_from("<I", data, s + 0x80)[0]
    fmt = "<%dI" % (npages + 1)
    pages = struct.unpack_from(fmt, data, fixpages)
    record = bytes((2, 0, srcoff & 255, srcoff >> 8, obj))
    if any(pages) or fixrecs + len(record) + 1 > datapages:
        raise ValueError("no room for a fixup")
    out = bytearray(data)
    out[fixrecs:fixrecs + len(record)] = record
    for i in range(1, npages + 1):
        at = fixpages + 4 * i
        struct.pack_into("<I", out, at, len(record))
    size = struct.unpack_from("<I", data, s + 0x30)[0]
    struct.pack_into("<I", out, s + 0x30, size + len(record))
    return bytes(out)


def clear_flags(data, obj):
    """Return a copy with flags of 1-based object obj zeroed."""
    s = le_start(data)
    table = s + struct.unpack_from("<I", data, s + 0x40)[0]
    count = struct.unpack_from("<I", data, s + 0x44)[0]
    if not 1 <= obj <= count:
        raise ValueError("object index out of range")
    out = bytearray(data)
    at = table + (obj - 1) * OBJSIZE + FLAGS
    struct.pack_into("<I", out, at, 0)
    return bytes(out)


def synthetic():
    """Tiny LE: 3 objects, one page, one selector fixup to #3."""
    d = bytearray(0x400)
    d[0x3C:0x40] = struct.pack("<I", 0x40)
    s = 0x40
    d[s:s + 2] = b"LE"
    struct.pack_into("<I", d, s + 0x14, 1)
    struct.pack_into("<I", d, s + 0x40, 0xC0)
    struct.pack_into("<I", d, s + 0x44, 3)
    struct.pack_into("<I", d, s + 0x68, 0x100)
    struct.pack_into("<I", d, s + 0x6C, 0x110)
    for i in range(3):
        at = s + 0xC0 + i * OBJSIZE + FLAGS
        struct.pack_into("<I", d, at, 0x2005)
    struct.pack_into("<2I", d, s + 0x100, 0, 5)
    d[s + 0x110:s + 0x115] = bytes((2, 0, 0x10, 0, 3))
    return bytes(d)


def selftest():
    data = synthetic()
    assert selector_targets(data) == [3], "fixup scan"
    bad = clear_flags(data, 3)
    diff = [i for i, v in enumerate(data) if v != bad[i]]
    at = 0x40 + 0xC0 + 2 * OBJSIZE + FLAGS
    assert diff == [at, at + 1], diff
    plain = bytearray(data)
    plain[0x40 + 0x110:0x40 + 0x115] = bytes(5)
    struct.pack_into("<2I", plain, 0x40 + 0x100, 0, 0)
    struct.pack_into("<I", plain, 0x40 + 0x80, 0x40 + 0x120)
    added = add_selector_fixup(bytes(plain), 0x10, 3)
    assert selector_targets(added) == [3], "added fixup"
    try:
        add_selector_fixup(added, 0x10, 3)
    except ValueError:
        pass
    else:
        raise AssertionError("second fixup must be refused")
    for i in (0, 4):
        try:
            clear_flags(data, i)
        except ValueError:
            continue
        raise AssertionError("range check")
    print("le self test OK")


def batch(lines):
    tail = ["echo OK > STATUS.LOG", "goto end", ":bad",
            "echo FAILED > STATUS.LOG", ":end"]
    return "\n".join(["@echo off"] + lines + tail) + "\n"


def check_lines(name, code):
    """Bind and run NAME.EXE; STS=0 only for exit CODE."""
    bad = "g" + name
    lines = ["ZRDXBIND -r -n -y %s.EXE > %s.BLD" % (name, name),
             "if errorlevel 1 goto bad",
             "%s.EXE > %s.LOG" % (name, name)]
    if code < 255:
        above = "if errorlevel %d goto %s"
        lines.append(above % (code + 1, bad))
    lines += ["if not errorlevel %d goto %s" % (code, bad),
              "echo 0 > %s.STS" % name, "goto d" + name,
              ":" + bad, "echo 1 > %s.STS" % name, ":d" + name]
    return lines


def run_dos(args):
    env = runpy.run_path(str(ROOT / "tests/CHECK.PY"))
    w = args.work.resolve()
    env["CFG"]["check_scratch"](w)
    w.mkdir(parents=True, exist_ok=True)
    for name in env["TOOLS"]:
        shutil.copyfile(args.tools / name, w / name)
    src = "BADOBJ.ASM"
    shutil.copyfile(ROOT / "tests/le" / src, w / src)
    env["config"](w, "xms", False, core="normal")
    env["dos"](w, batch([
        "nasm -f rdf -o BADOBJ.RDO BADOBJ.ASM",
        "if errorlevel 1 goto bad",
        "rlink le /zrdx /o=GOOD.EXE BADOBJ.RDO",
        "if errorlevel 1 goto bad"]), 60)
    raw = (w / "GOOD.EXE").read_bytes()
    head = le_start(raw)
    count = struct.unpack_from("<I", raw, head + 0x44)[0]
    good = add_selector_fixup(raw, 0x10, count)
    (w / "GOOD.EXE").write_bytes(good)
    targets = selector_targets(good)
    if targets != [count]:
        raise RuntimeError("fixup not added: %r" % targets)
    bad = clear_flags(good, targets[0])
    (w / "BAD.EXE").write_bytes(bad)
    env["dos"](w, batch(check_lines("GOOD", 42)
                        + check_lines("BAD", 255)), 60)
    failed = [n for n in ("GOOD", "BAD")
              if (w / (n + ".STS")).read_text().strip() != "0"]
    if failed:
        raise RuntimeError("LE failed: " + " ".join(failed))
    print("le DOS cases OK")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--self", action="store_true")
    parser.add_argument("--work", type=Path)
    parser.add_argument("--tools", type=Path,
                        default=ROOT / "bin")
    args = parser.parse_args()
    selftest()
    if not args.self:
        if args.work is None:
            parser.error("--work is required for the DOS run")
        run_dos(args)


if __name__ == "__main__":
    sys.exit(main())
