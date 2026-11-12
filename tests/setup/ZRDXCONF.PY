"""Host test of zrdxconf.c: long names, view and change modes."""
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parent.parent.parent
SRC = ROOT / "src" / "zrdxconf.c"


def run(exe, cwd, *args):
    r = subprocess.run([str(exe)] + [str(a) for a in args],
                       capture_output=True, text=True, cwd=cwd)
    return r.returncode, r.stdout


def main():
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        exe = tmp / "zrdxconf"
        subprocess.run(["cc", "-std=c89", "-Wall", "-Werror",
                        "-o", str(exe), str(SRC)], check=True)
        trailer = b"ZRDX" + bytes(10)
        long = tmp / ("d" * 60) / ("f" * 200)
        long.parent.mkdir()
        rel = long.relative_to(tmp)
        long.write_bytes(b"MZ" + bytes(100) + trailer)
        code, out = run(exe, tmp, rel)
        assert code == 0 and str(rel) in out, (code, out)
        code, out = run(exe, tmp, "/X10", "/M1", rel)
        assert code == 0 and "New configuration" in out, "change"
        data = long.read_bytes()
        assert data[-14:-10] == b"ZRDX", "signature kept"
        assert data[-6:-2] == (0x10).to_bytes(4, "little"), "xms"
        assert data[-10] & 2, "mode flag"
        code, out = run(exe, tmp, "/S20", rel)
        assert code == 0 and "swap file size" in out, "swap"
        data = long.read_bytes()
        assert data[-9] == 0x20 and data[-10] & 2, "swap byte"
        code, out = run(exe, tmp, "/S81", rel)
        assert code == 1 and "out of range" in out, "swap range"
        code, out = run(exe, tmp, "/T10", rel)
        assert code == 1 and "out of range" in out, "range check"
        dflt = (b"ZRDX" + bytes([0, 0, 0, 4]) +
                b"\xff\xff\xff\xff" + b"\x00\xf0")
        for arg in ("/X-1", "/X1FFFFFFFF", "/X 10", "/X+1"):
            code, out = run(exe, tmp, arg, rel)
            assert code == 1 and "syntax" in out, arg
        bad = tmp / "BAD.EXE"
        for body in (bytes(40), b"MZ\x01"):
            bad.write_bytes(body)
            code, out = run(exe, tmp, "/M1", bad.name)
            assert code == 0 and "creating" in out, "create msg"
            data = bad.read_bytes()
            assert len(data) == len(body) + 14, "grown by 14"
            assert data[:len(body)] == body, "body kept"
            want = bytearray(dflt)
            want[4] = 2
            assert data[len(body):] == want, "defaults"
            code, out = run(exe, tmp, bad.name)
            assert code == 0 and "creating" not in out, "reread"
            assert bad.read_bytes() == data, "view keeps file"
        bad.write_bytes(bytes(40))
        code, out = run(exe, tmp, bad.name)
        assert code == 0, "view"
        assert bad.read_bytes()[40:] == dflt, "view data"
        bad.write_bytes(bytes(40))
        code, out = run(exe, tmp, "/S81", bad.name)
        assert code == 1, "bad opt"
        assert bad.read_bytes() == bytes(40), "bad opt kept"
    print("zrdxconf OK")


if __name__ == "__main__":
    sys.exit(main())
