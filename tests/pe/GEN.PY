"""Build small PE32 fixtures from HELLO.BIN; no DLL/import features."""
from pathlib import Path
import struct


def generate(folder):
    folder = Path(folder)
    code = (folder / "HELLO.BIN").read_bytes()
    pe = 0x80
    image = bytearray(0x600)
    def w(off, val):
        struct.pack_into("<H", image, off, val)
    def d(off, val):
        struct.pack_into("<I", image, off, val)
    image[:2] = b"MZ"
    w(2, 0x80)
    w(4, 1)
    w(8, 4)
    w(24, 0x40)
    d(0x3c, pe)
    image[pe:pe+4] = b"PE\0\0"
    w(pe+4, 0x14c)
    w(pe+6, 2)
    w(pe+20, 224)
    w(pe+22, 0x102)
    w(pe+24, 0x10b)
    d(pe+40, 0x1000)
    d(pe+52, 0x400000)
    d(pe+56, 0x1000)
    d(pe+60, 0x200)
    d(pe+80, 0x3000)
    d(pe+84, 0x200)
    d(pe+96, 0x10000)
    d(pe+116, 16)
    d(pe+160, 0x2000)
    d(pe+164, 12)
    sec = pe+248
    for off, name, rva, raw, size, flags in [
        (sec, b".text", 0x1000, 0x200, len(code), 0x60000020),
        (sec+40, b".reloc", 0x2000, 0x400, 12, 0x42000040),
    ]:
        image[off:off+len(name)] = name
        d(off+8, size)
        d(off+12, rva)
        d(off+16, 0x200)
        d(off+20, raw)
        d(off+36, flags)
    image[0x200:0x200+len(code)] = code
    struct.pack_into("<IIHH", image, 0x400, 0x1000, 12, 0x3001, 0)
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
    for name, data in cases.items():
        (folder / (name + ".PE")).write_bytes(data)
    lines = ["@echo off", "if exist RESULT.LOG del RESULT.LOG"]
    for name in cases:
        lines += ["R:" + chr(92) + "BIN" + chr(92) + "zrxbind -y " + name + ".PE " + name + ".EXE > NUL",
                  "if errorlevel 1 echo " + name + " BIND_FAILED >> RESULT.LOG",
                  name + ".EXE >> RESULT.LOG"]
        if name == "GOOD":
            lines += ["if errorlevel 1 echo GOOD FAILED >> RESULT.LOG",
                      "if not errorlevel 1 echo GOOD OK >> RESULT.LOG"]
        else:
            lines += ["if errorlevel 255 echo " + name + " OK >> RESULT.LOG",
                      "if not errorlevel 255 echo " + name + " FAILED >> RESULT.LOG"]
    # DOS paths contain one backslash, regardless of host string escaping.
    (folder / "RUN.BAT").write_bytes(("\r\n".join(lines) + "\r\n").encode("ascii"))
    return list(cases)


if __name__ == "__main__":
    generate(Path(__file__).parent)
