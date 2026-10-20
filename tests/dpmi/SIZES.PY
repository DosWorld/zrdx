"""Measure source-built section/layout budgets; convert to docs/D09SIZE.TSV."""
import argparse
import hashlib
from pathlib import Path
import re
import runpy
import shutil
import struct
import subprocess

ROOT = Path(__file__).resolve().parents[2]
CFG = runpy.run_path(str(ROOT / "tests/dpmi/ZRCFG.PY"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work", required=True, type=Path)
    parser.add_argument("--native", required=True, type=Path)
    parser.add_argument("--ordinary", required=True, type=Path)
    args = parser.parse_args()
    work = args.work.resolve()
    CFG["check_scratch"](work)
    work.mkdir(parents=True, exist_ok=True)
    src = work / "src"
    shutil.copytree(ROOT / "src", src, dirs_exist_ok=True)
    digest = hashlib.sha256()
    for path in sorted((ROOT / "src").rglob("*")):
        if path.is_file():
            digest.update(str(path.relative_to(ROOT)).encode())
            digest.update(path.read_bytes())
    # Instrument only the scratch macro, before padding is emitted.
    path = src / "prothand.asm"
    text = path.read_text().replace("%assign _csz zcat2(SegSize,%1)",
        "%assign _csz zcat2(SegSize,%1)\n%assign ProbeUsed ($ - $$)\n%warning ZRSIZE %1 ProbeUsed _csz")
    path.write_bytes(text.replace("\n", "\r\n").encode("ascii"))
    path = src / "nathost.asm"
    text = path.read_text().replace("NC_Size equ NC_Frames + 4096", """NC_Size equ NC_Frames + 4096
%assign ProbeContext NC_Size
%warning ZRCTX context ProbeContext
%assign ProbeContext NC_Stack
%warning ZRCTX locked-offset ProbeContext
%assign ProbeContext NC_MemBytes
%warning ZRCTX memory-bitmap ProbeContext""")
    path.write_bytes(text.replace("\n", "\r\n").encode("ascii"))
    rows = ["profile\titem\tused\tbudget\tnote"]
    for name, root in (("native", "zrxdpmi"), ("le", "zrdx"), ("pe", "zrdxpe"), ("rdf", "zrdxrdf")):
        result = subprocess.run([shutil.which("nasm"), "-O9", "-f", "obj", "-l", str(work / (name + ".LST")),
            root + ".asm", "-o", str(work / (name + ".OBJ"))], cwd=src, text=True, capture_output=True, check=True)
        (work / (name + ".LOG")).write_text(result.stderr)
        for section, used, budget in re.findall(r"warning: ZRSIZE (\w+) (\d+) (\d+)", result.stderr):
            rows.append("\t".join((name, section, used, budget, "section bytes before/after padding")))
        for field, value in re.findall(r"warning: ZRCTX ([\w-]+) (\d+)", result.stderr):
            rows.append("\t".join((name, field, value, "", "bytes")))
        if name in ("native", "le"):
            listing = work / (name + "LAY.LST")
            subprocess.run([shutil.which("nasm"), "-f", "obj", *([] if name == "native" else ["-DBOUND=1"]),
                "-l", str(listing), str(ROOT / "tests/dpmi/KLAYOUT.ASM"), "-o", str(work / (name + "LAY.OBJ"))], cwd=src, check=True)
            lines = listing.read_text().splitlines()
            for index, line in enumerate(lines):
                if "dd OffPageDir," not in line:
                    continue
                number = line.split()[0]
                data = b""
                for piece in lines[index:]:
                    match = re.match(r"\s*" + number + r"\s+[0-9A-F]{8} ([0-9A-F]+)-?", piece)
                    if not match:
                        break
                    data += bytes.fromhex(match[1])
                for field, value in zip(("page-directory", "kernel-page-table", "last-initialized", "mcb-vector", "client-linear-base"), struct.unpack("<5I", data)):
                    rows.append("\t".join((name, field, str(value), "", "linear address")))
    for profile, folder in (("native", args.native), ("ordinary", args.ordinary)):
        names = ("ZRXDPMI.EXE",) if profile == "native" else ("src/zrdx0.exe", "src/zrdxpe0.exe", "src/zrdxrdf0.exe", "bin/stub-le.exe", "bin/stub-pe.exe", "bin/stub-rdf.exe", "src/zrdxu.exe", "src/zrdxpu.exe", "src/zrdxru.exe")
        for name in names:
            path = folder / name
            if not path.exists():
                continue
            data = path.read_bytes()
            values = struct.unpack_from("<14H", data)
            image = values[2] * 512 - ((512 - values[1]) if values[1] else 0)
            header = values[4] * 16
            note = "file=" + str(len(data)) + "; header=" + str(header) + "; minalloc=" + str(values[5] * 16) + "; sha256=" + hashlib.sha256(data).hexdigest()
            rows.append("\t".join((profile, name, str(image - header), str(image), note)))
            minimum = 256 + ((image - header + 15) & ~15) + values[5] * 16
            rows.append("\t".join((profile, name + ":dos-load", str(minimum), "", "PSP + paragraph-rounded MZ payload + minimum extra; not steady-state residency")))
    rows.append("\t".join(("source", "sha256", digest.hexdigest(), "", "before measurement-only instrumentation")))
    target = ROOT / "docs/D09SIZE.TSV"
    target.write_bytes(("\r\n".join(rows) + "\r\n").encode("ascii"))
    print(target)


if __name__ == "__main__":
    main()
