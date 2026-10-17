"""Generate malformed MZ inputs which the binder must reject promptly."""
from pathlib import Path
import struct


def generate(folder):
    folder = Path(folder)
    base = bytearray(512)
    struct.pack_into("<5H", base, 0, 0x5a4d, 0, 1, 0, 2)
    struct.pack_into("<H", base, 24, 28)
    cases = {}
    cases["ZERO"] = bytearray(base)
    struct.pack_into("<H", cases["ZERO"], 4, 0)
    cases["REMAIN"] = bytearray(base)
    struct.pack_into("<H", cases["REMAIN"], 2, 513)
    cases["SHORT"] = base[:20]
    cases["NEWHDR"] = base[:32]
    struct.pack_into("<H", cases["NEWHDR"], 24, 0x40)
    lines = ["@echo off", "if exist RESULT.LOG del RESULT.LOG"]
    for name, data in cases.items():
        (folder / (name + ".BAD")).write_bytes(data)
        lines += [r"R:\BIN\ZRXBIND -y " + name + ".BAD " + name + ".EXE",
                  "if errorlevel 1 if not errorlevel 2 echo " + name + " OK >> RESULT.LOG",
                  "if not errorlevel 1 echo " + name + " FAILED >> RESULT.LOG",
                  "if errorlevel 2 echo " + name + " FAILED >> RESULT.LOG"]
    (folder / "RUN.BAT").write_bytes(("\r\n".join(lines) + "\r\n").encode("ascii"))
    return tuple(cases)
