"""Host test of aligner.c: GEN.PY cases, kept tail, 3Ch."""
from pathlib import Path
import struct
import subprocess
import sys
import tempfile

HERE = Path(__file__).resolve().parent
SRC = HERE.parent.parent / "src" / "aligner" / "aligner.c"
GEN = __import__("runpy").run_path(str(HERE / "GEN.PY"))


def build(tmp):
    exe = tmp / "aligner"
    cmd = ["cc", "-std=gnu89", "-Wall", "-Werror"]
    cmd += ["-I", str(HERE / "shim"), "-o", str(exe), str(SRC)]
    subprocess.run(cmd, check=True)
    return exe


def align(exe, tmp, name, data, size):
    path = tmp / name
    path.write_bytes(data)
    cmd = [str(exe), str(path), str(size)]
    code = subprocess.run(cmd).returncode
    return code, path.read_bytes()


def main():
    orig = GEN["original"]
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        exe = build(tmp)
        code, out = align(exe, tmp, "FULL.EXE", orig(), 1536)
        assert code == 0 and len(out) == 1536, "full"
        pages = struct.unpack_from("<HH", out, 2)
        assert pages == (0, 3), "pages"
        assert out[64:1056] == orig()[32:], "load bytes"
        field = struct.unpack_from("<I", out, 0x3C)[0]
        assert field == 1536, "3Ch"
        code, out = align(exe, tmp, "SMALL.EXE", orig(), 1024)
        assert code == 2, "small"
        assert out == orig(), "small kept"
        tail = bytes(orig()) + bytes(range(16))
        code, out = align(exe, tmp, "TAIL.EXE", tail, 1536)
        assert code == 0 and len(out) == 1536 + 16, "tail size"
        assert out[1536:] == tail[-16:], "tail kept"
        assert out[64:1056] == orig()[32:], "tail load bytes"
        bad = bytearray(orig())
        struct.pack_into("<H", bad, 8, 0xf000)
        code, out = align(exe, tmp, "HEAD.EXE", bad, 1536)
        assert code == 4, "bad header size"
        assert out == bad, "bad header kept"
        code, out = align(exe, tmp, "ZERO.EXE", orig(), 0)
        assert code == 3, "zero size"
    print("aligner OK")


if __name__ == "__main__":
    sys.exit(main())
