"""Writes a forwarder-only PE DLL (task 211).

Usage: python3 GENFWD.PY KERNEL32.LNK KERNELBA.DLL KERNELBASE.DLL

Every "export Name=..." line of the .LNK file (the source list)
becomes a forwarder "KERNEL32.Name" in the export table of the
new DLL; the image has no code and no entry point.
"""
import re
import struct
import sys

BASE = 0x10000000
VA = 0x1000
RAW = 0x200


def names(lnk):
    text = open(lnk).read().replace("\r", "")
    return sorted(set(re.findall(r"^export (\w+)=", text, re.M)))


def export_table(dll, target, funcs):
    """Export directory at VA with the forwarder strings."""
    n = len(funcs)
    head = 40 + 10 * n
    strs = bytearray(dll.encode() + b"\0")
    where, fwd = [], []
    for f in funcs:
        where.append(VA + head + len(strs))
        strs += f.encode() + b"\0"
    for f in funcs:
        fwd.append(VA + head + len(strs))
        strs += (target + "." + f).encode() + b"\0"
    table = struct.pack("<IIHHIIIIIII", 0, 0, 0, 0, VA + head,
                        1, n, n,
                        VA + 40, VA + 40 + 4 * n,
                        VA + 40 + 8 * n)
    table += b"".join(struct.pack("<I", x) for x in fwd)
    table += b"".join(struct.pack("<I", x) for x in where)
    table += b"".join(struct.pack("<H", i) for i in range(n))
    return table + bytes(strs)


def image(blob):
    size = (len(blob) + RAW - 1) // RAW * RAW
    full = VA + (size + 0xFFF) // 0x1000 * 0x1000
    opt = struct.pack("<HBBIIIIIIIIIHHHHHHIIIIHHIIIIII",
                      0x10B, 6, 0, 0, size, 0, 0, VA, VA, BASE,
                      0x1000, RAW, 4, 0, 0, 0, 4, 0, 0, full,
                      RAW, 0,
                      3, 0, 0x100000, 0x1000, 0x100000, 0x1000,
                      0, 16)
    dirs = bytearray(16 * 8)
    struct.pack_into("<II", dirs, 0, VA, len(blob))
    sec = struct.pack("<8sIIIIIIHHI", b".edata", len(blob), VA,
                      size,
                      RAW, 0, 0, 0, 0, 0x40000040)
    coff = struct.pack("<HHIIIHH", 0x14C, 1, 0, 0, 0,
                       len(opt) + len(dirs), 0x2102)
    dos = b"MZ" + bytes(0x3A) + struct.pack("<I", 0x40)
    head = dos + b"PE\0\0" + coff + opt + bytes(dirs) + sec
    return head.ljust(RAW, b"\0") + blob.ljust(size, b"\0")


def main():
    if len(sys.argv) != 4:
        return 1
    lnk, out, dll = sys.argv[1:]
    data = image(export_table(dll, "KERNEL32", names(lnk)))
    with open(out, "wb") as f:
        f.write(data)
    return 0


if __name__ == "__main__":
    sys.exit(main())
