"""Build small PE32 fixtures from HELLO.BIN and RELOC.BIN.

RELOC.BIN/RELOC2.BIN/RELOCH.BIN are one program linked at
0x401000, 0x501000 and 0x10001000; the bytes that differ between
the first two are its relocation sites. No DLL/import features.
"""
from pathlib import Path
import struct

VALID = ("GOOD", "MULTI", "BASEHI")
PE = 0x80
RELOC = 0x42000040


def roundup(n, a):
    return (n + a - 1) // a * a


def reloc_blocks(sites):
    """One HIGHLOW block per 4 KB page, padded to 4 bytes."""
    out = b""
    for page in sorted({s >> 12 for s in sites}):
        ent = [0x3000 | (s & 0xfff) for s in sorted(sites) if s >> 12 == page]
        ent += [0] * (len(ent) % 2)
        out += struct.pack("<II", page << 12, 8 + 2 * len(ent))
        out += b"".join(struct.pack("<H", e) for e in ent)
    return out


def sites_of(lo, hi, rva):
    """RVAs of dwords that differ between two links (byte 2 changes)."""
    return [rva + i - 2 for i in range(len(lo)) if lo[i] != hi[i]]


def make_pe(secs, base, chars=0x102):
    """secs: (name, rva, data, flags); .reloc is the reloc directory."""
    image = bytearray(0x200)
    def w(off, val):
        struct.pack_into("<H", image, off, val)
    def d(off, val):
        struct.pack_into("<I", image, off, val)
    image[:2] = b"MZ"
    w(2, 0x80)
    w(4, 1)
    w(8, 4)
    w(24, 0x40)
    d(0x3c, PE)
    image[PE:PE+4] = b"PE\0\0"
    w(PE+4, 0x14c)
    w(PE+6, len(secs))
    w(PE+20, 224)
    w(PE+22, chars)
    w(PE+24, 0x10b)
    d(PE+40, 0x1000)
    d(PE+52, base)
    d(PE+56, 0x1000)
    d(PE+60, 0x200)
    d(PE+80, max(r + roundup(len(x), 0x1000) for _, r, x, _ in secs))
    d(PE+84, 0x200)
    d(PE+96, 0x10000)
    d(PE+116, 16)
    for n, (name, rva, data, flags) in enumerate(secs):
        off = PE + 248 + 40 * n
        raw = roundup(len(data), 0x200)
        image[off:off+len(name)] = name
        d(off+8, len(data))
        d(off+12, rva)
        d(off+16, raw)
        d(off+20, len(image))
        d(off+36, flags)
        if name == b".reloc":
            d(PE+160, rva)
            d(PE+164, len(data))
        image += data.ljust(raw, b"\0")
    return image


def reloc_cases(folder):
    """Valid multi-block images, a stripped one and damaged blocks."""
    lo, hi, high = (
        (folder / n).read_bytes() for n in ("RELOC.BIN", "RELOC2.BIN", "RELOCH.BIN"))
    sites = sites_of(lo[:0x1000], hi[:0x1000], 0x1000)
    sites += sites_of(lo[0x1000:], hi[0x1000:], 0x2000)
    rel = reloc_blocks(sites)
    def build(text, base, reloc=rel, chars=0x102):
        secs = [(b".text", 0x1000, text[:0x1000], 0x60000020),
                (b".data", 0x2000, text[0x1000:], 0xc0000040)]
        if reloc:
            secs.append((b".reloc", 0x3000, reloc, RELOC))
        return bytes(make_pe(secs, base, chars))
    cases = {"MULTI": build(lo, 0x400000), "BASEHI": build(high, 0x10000000),
             "NOREL": build(lo, 0x7f000000, b"", 0x103)}
    second = rel.index(struct.pack("<I", 0x2000))
    damaged = bytearray(rel)
    struct.pack_into("<I", damaged, second + 4, 7)
    cases["BAD2ND"] = build(lo, 0x400000, bytes(damaged))
    damaged = bytearray(rel)
    struct.pack_into("<H", damaged, 8, 0x1001)
    cases["TYPEHI"] = build(lo, 0x400000, bytes(damaged))
    damaged = bytearray(rel)
    struct.pack_into("<H", damaged, 8, 0xa001)
    cases["TYPE10"] = build(lo, 0x400000, bytes(damaged))
    return cases


def generate(folder):
    folder = Path(folder)
    code = (folder / "HELLO.BIN").read_bytes()
    pe = PE
    sec = pe + 248
    image = make_pe([(b".text", 0x1000, code, 0x60000020),
                     (b".reloc", 0x2000,
                      struct.pack("<IIHH", 0x1000, 12, 0x3001, 0), RELOC)],
                    0x400000)
    cases = {"GOOD": bytes(image)}
    def change(name, off, val, fmt="<I"):
        b = bytearray(image)
        struct.pack_into(fmt, b, off, val)
        cases[name] = bytes(b)
    change("OPTSHORT", pe+20, 95, "<H")
    change("DIRCOUNT", pe+116, 17)
    change("SECTOVR", sec+8, 0xfffffff0)
    change("SECTHDR", sec+12, 0x100)
    change("OVERLAP", sec+40+12, 0x1000)
    change("RAWOVR", sec+20, 0xfffffff0)
    change("RAWSHORT", sec+20, 0x500)
    change("RELOC4", pe+164, 4)
    change("RELOC7", 0x404, 7)
    change("RELOC9", 0x404, 9)
    b = bytearray(cases["RELOC9"])
    struct.pack_into("<I", b, pe+164, 9)
    cases["RELOC9"] = bytes(b)
    change("RELOCWR", 0x400, 0xffffffff)
    change("TARGET", 0x408, 0x3fff, "<H")
    b = bytearray(cases["TARGET"])
    struct.pack_into("<I", b, 0x400, 0x2000)
    cases["TARGET"] = bytes(b)
    b = bytearray(cases["TARGET"])
    struct.pack_into("<I", b, pe+80, 0x2ffe)
    struct.pack_into("<H", b, 0x408, 0x3ffb)
    cases["BYTEEND"] = bytes(b)
    change("HEADERS", pe+84, 0)
    change("ENTRY", pe+40, 0x3000)
    change("IMPORT", pe+128, 0x1000)
    cases["TRUNC"] = bytes(image[:pe+250])
    cases.update(reloc_cases(folder))
    for name, data in cases.items():
        (folder / (name + ".PE")).write_bytes(data)
    lines = ["@echo off", "if exist RESULT.LOG del RESULT.LOG"]
    for name in cases:
        lines += ["R:" + chr(92) + "BIN" + chr(92) + "zrxbind -y " + name + ".PE " + name + ".EXE > NUL",
                  "if errorlevel 1 echo " + name + " BIND_FAILED >> RESULT.LOG",
                  name + ".EXE >> RESULT.LOG"]
        if name in VALID:
            lines += ["if errorlevel 1 echo " + name + " FAILED >> RESULT.LOG",
                      "if not errorlevel 1 echo " + name + " OK >> RESULT.LOG"]
        else:
            lines += ["if errorlevel 255 echo " + name + " OK >> RESULT.LOG",
                      "if not errorlevel 255 echo " + name + " FAILED >> RESULT.LOG"]
    # DOS paths contain one backslash, regardless of host string escaping.
    (folder / "RUN.BAT").write_bytes(("\r\n".join(lines) + "\r\n").encode("ascii"))
    return list(cases)


if __name__ == "__main__":
    generate(Path(__file__).parent)
