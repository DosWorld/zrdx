"""Host check of PE GEN.PY: COM cases differ by one field."""
from pathlib import Path
import runpy
import struct
import subprocess
import sys
import tempfile

HERE = Path(__file__).resolve().parent
PE = 0x80


def asm(tmp, src, out, *defs):
    cmd = ["nasm", "-f", "bin"] + list(defs)
    cmd += [str(HERE / src), "-o", str(tmp / out)]
    subprocess.run(cmd, check=True)


def main():
    gen = runpy.run_path(str(HERE / "GEN.PY"))
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        asm(tmp, "HELLO.ASM", "HELLO.BIN")
        for out, base in (("RELOC", "0x401000"),
                          ("RELOC2", "0x501000"),
                          ("RELOCH", "0x10001000")):
            asm(tmp, "RELOC.ASM", out + ".BIN", "-DBASE=" + base)
        names = gen["generate"](tmp)
        good = (tmp / "GOOD.PE").read_bytes()
        dirs = struct.unpack_from("<I", good, PE + 116)[0]
        assert dirs >= 15
        fields = (("COM", PE + 232), ("COMSZ", PE + 236))
        for name, off in fields:
            assert name in names, name
            bad = (tmp / (name + ".PE")).read_bytes()
            diff = [i for i, v in enumerate(good)
                    if v != bad[i]]
            assert diff and min(diff) >= off, name
            assert max(diff) < off + 4, name
    print("pe gen OK")


if __name__ == "__main__":
    sys.exit(main())
