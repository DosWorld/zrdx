"""Generate bounded MZ aligner inputs and verify the preserved load image."""
from pathlib import Path
import struct


def original():
    b = bytearray(1024)
    struct.pack_into("<5H", b, 0, 0x5a4d, 0, 2, 0, 2)
    struct.pack_into("<H", b, 24, 28)
    b[32:] = bytes((i * 13 + 7) & 255 for i in range(992))
    return b


def generate(folder):
    folder = Path(folder)
    cases = {"FULL": (original(), 0), "LEGACY": (original(), 0),
             "RELOCS": (original(), 4), "HEADER": (original(), 4),
             "PAGES": (original(), 4), "SMALL": (original(), 2)}
    struct.pack_into("<H", cases["LEGACY"][0], 2, 512)
    struct.pack_into("<H", cases["RELOCS"][0], 6, 40)
    struct.pack_into("<H", cases["HEADER"][0], 8, 0xf000)
    struct.pack_into("<H", cases["PAGES"][0], 4, 0xffff)
    lines = ["@echo off", "if exist RESULT.LOG del RESULT.LOG"]
    for name, (data, status) in cases.items():
        (folder / (name + ".EXE")).write_bytes(data)
        size = 1024 if name == "SMALL" else 1536
        lines += [r"R:\src\aligner.exe " + name + ".EXE " + str(size)]
        if status == 0:
            lines += ["if errorlevel 1 echo " + name + " FAILED >> RESULT.LOG",
                      "if not errorlevel 1 echo " + name + " OK >> RESULT.LOG"]
        else:
            lines += ["if errorlevel " + str(status) + " if not errorlevel " + str(status+1) + " echo " + name + " OK >> RESULT.LOG",
                      "if not errorlevel " + str(status) + " echo " + name + " FAILED >> RESULT.LOG",
                      "if errorlevel " + str(status+1) + " echo " + name + " FAILED >> RESULT.LOG"]
    (folder / "RUN.BAT").write_bytes(("\r\n".join(lines) + "\r\n").encode("ascii"))


def verify(folder):
    folder = Path(folder)
    for name in ("FULL", "LEGACY"):
        data = (folder / (name + ".EXE")).read_bytes()
        if len(data) != 1536 or struct.unpack_from("<HH", data, 2) != (0, 3):
            raise RuntimeError("aligner size/page header mismatch: " + name)
        if data[64:1056] != original()[32:] or any(data[1056:]):
            raise RuntimeError("aligner lost or changed load bytes: " + name)
    for name in ("RELOCS", "HEADER", "PAGES", "SMALL"):
        data = (folder / (name + ".EXE")).read_bytes()
        expected = original()
        changes = {"RELOCS": (6, 40), "HEADER": (8, 0xf000), "PAGES": (4, 0xffff)}
        if name in changes:
            offset, value = changes[name]
            struct.pack_into("<H", expected, offset, value)
        if data != expected:
            raise RuntimeError("aligner modified rejected input: " + name)
