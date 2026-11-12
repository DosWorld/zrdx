"""Run the PE/DLL fixtures in a private FreeDOS/QEMU disk image.

--fixtures is the tests/pe folder of a CHECK.PY scratch run (it
holds the bound NAME.P1/NAME.P2 programs and the DLL files).
"""
import argparse
import os
from pathlib import Path
import runpy
import shutil
import socket
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
CFG = runpy.run_path(str(ROOT / "tests/dpmi/ZRCFG.PY"))
Fat16 = runpy.run_path(str(ROOT
                           / "tests/dpmi/FAT16.PY"))["Fat16"]


def watch(port, addr, log, limit=400):
    """Debug aid: log every write to a guest linear address (QEMU
    gdbstub).
    """
    sock = None
    for _ in range(100):
        try:
            sock = socket.create_connection(("127.0.0.1",
                port), timeout=5)
            break
        except OSError:
            time.sleep(0.2)
    if sock is None:
        return
    sock.settimeout(120)

    def send(text):
        sock.sendall(("$" + text + "#%02x"
                      % (sum(text.encode()) & 255)).encode())

    def recv():
        data = b""
        while True:
            chunk = sock.recv(4096)
            if not chunk:
                raise EOFError
            data += chunk
            end = data.rfind(b"#")
            if b"$" in data and end >= 0 and len(
                data) >= end + 3:
                sock.sendall(b"+")
                return data.decode(errors="replace")

    try:
        recv_ack = lambda: None
        send("Z2,%x,4" % addr)
        recv()
        hits = 0
        while hits < limit:
            send("c")
            reply = recv()
            send("g")
            regs = recv()
            body = regs[regs.find("$") + 1:regs.rfind("#")]
            names = (
                "eax ecx edx ebx esp ebp esi edi eip eflags cs "
                "ss ds es fs gs".split())
            vals = {n: int.from_bytes(bytes.fromhex(body[i
                * 8:i * 8 + 8]), "little")
                    for i,
                    n in enumerate(names) if len(body) >= i
                    * 8 + 8}
            send("m%x,4" % addr)
            mem = recv()
            data = mem[mem.find("$") + 1:mem.rfind("#")]
            log.write(
                "hit %d cs:eip=%04x:%08x ds=%04x es=%04x "
                "value=%s regs=%s\n" % (
                hits, vals.get("cs", 0), vals.get("eip", 0),
                    vals.get("ds", 0), vals.get("es", 0),
                data,
                    " ".join("%s=%x" % (k, v) for k,
                             v in vals.items())))
            log.flush()
            hits += 1
    except (EOFError, OSError, socket.timeout):
        pass


def inspect(port, addr, commands, log, monitor, steps=0,
            pokes=()):
    """Debug aid: stop at a guest linear address, run monitor
    commands.
    """
    sock = None
    for _ in range(100):
        try:
            sock = socket.create_connection(("127.0.0.1",
                port), timeout=5)
            break
        except OSError:
            time.sleep(0.2)
    if sock is None:
        return
    sock.settimeout(120)
    buf = [b""]

    def send(text):
        sock.sendall(("$" + text + "#%02x"
                      % (sum(text.encode()) & 255)).encode())

    def packet():
        while True:
            data = buf[0]
            start = data.find(b"$")
            end = data.find(b"#", start)
            if start >= 0 and end >= 0 and len(data) >= end + 3:
                buf[0] = data[end + 3:]
                sock.sendall(b"+")
                return data[start
                            + 1:end].decode(errors="replace")
            chunk = sock.recv(4096)
            if not chunk:
                raise EOFError
            buf[0] += chunk

    try:
        send("Z0,%x,1" % addr)
        packet()
        send("c")
        log.write("stop: " + packet() + "\n")
        send("g")
        regs = packet()
        names = (
            "eax ecx edx ebx esp ebp esi edi eip eflags cs ss "
            "ds es fs gs".split())
        log.write(" ".join("%s=%x" % (n,
                                      int.from_bytes(
                                          bytes.fromhex(regs[i
                                              * 8:i * 8 + 8]),
                                                  "little"))
                           for i, n in enumerate(names)) + "\n")
        for poke in pokes:
            where, data = poke.split("=")
            send("M%x,%x:%s" % (int(where, 0), len(data) // 2,
                                data))
            log.write("poke %s: %s\n" % (poke, packet()))
        for number in range(steps):
            send("s")
            log.write("step %d: %s\n" % (number, packet()))
            send("g")
            regs = packet()
            log.write(" ".join("%s=%x" % (n,
                int.from_bytes(bytes.fromhex(regs[i * 8:i * 8
                    + 8]), "little"))
                               for i,
                               n in enumerate(names)) + "\n")
            log.flush()
        for command in commands:
            monitor.write((command + "\n").encode())
            monitor.flush()
            time.sleep(0.5)
        log.flush()
    except (EOFError, OSError, socket.timeout) as exc:
        log.write("ended: " + repr(exc) + "\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--qemu", required=True, type=Path)
    parser.add_argument("--image", type=Path,
                        default=os.environ.get("ZRDX_DISK"))
    parser.add_argument("--fixtures", required=True, type=Path)
    parser.add_argument("--work", required=True, type=Path)
    parser.add_argument("--profile",
                        choices=("raw", "xms", "vcpi"),
                        default="vcpi")
    parser.add_argument("--host", type=Path,
                        help="run NAME.EXE through this host "
                             "image (unbound PE files)")
    parser.add_argument("--leak", type=int, default=0,
                        help="run each case N times and "
                             "compare free XMS (task 161)")
    parser.add_argument("--cpu", default="pentium")
    parser.add_argument("--mem", type=int, default=32)
    parser.add_argument("--case", action="append",
                        help="run only these cases")
    parser.add_argument("--dump", type=int,
                        help="seconds to wait, then dump 8 "
                             "MB..+64K to MEM.BIN and quit")
    parser.add_argument("--watch", type=lambda s: int(s, 0),
                        help="log writes to this guest linear "
                             "address")
    parser.add_argument("--stop", type=lambda s: int(s, 0),
                        help="stop at this linear address and "
                             "run monitor commands "
                             "(INSPECT.LOG)")
    parser.add_argument("--poke", action="append", default=[],
                        help="ADDR=HEXBYTES written after "
                             "--stop")
    parser.add_argument("--step", type=int, default=0,
                        help="single steps after --stop")
    parser.add_argument("--mon", action="append", default=[],
                        help="monitor command for --stop")
    parser.add_argument("--trace", action="store_true",
                        help="QEMU -d int into TRACE.LOG")
    parser.add_argument("--dflag", default="int",
                        help="QEMU -d flags for --trace")
    parser.add_argument("--timeout", type=int, default=600)
    args = parser.parse_args()
    if args.image is None:
        raise ValueError("give --image or set ZRDX_DISK")
    gen = runpy.run_path(str(args.fixtures / "GEN.PY"))
    valid, pmxonly, expect = gen["VALID"], gen["PMXONLY"], gen[
        "EXPECT"]
    work = args.work.resolve()
    CFG["check_scratch"](work)
    work.mkdir(parents=True, exist_ok=True)
    disk = work / "TEST.IMG"
    shutil.copyfile(args.image, disk)
    volume = Fat16(disk, writable=True)
    cases = sorted(
        p.stem for p in args.fixtures.glob("*.P1")) + sorted(
        n for n in pmxonly if (args.fixtures
                               / (n + ".P2")).exists())
    if args.case:
        cases = [n for n in cases if n in args.case]
    if args.host:
        volume.put_root("ZRDXDPMI.EXE", args.host.read_bytes())
    for name in cases:
        suffix = ".PE" if args.host else (
            ".P2" if name in pmxonly else ".P1")
        volume.put_root(name + ".EXE",
                        (args.fixtures
                         / (name + suffix)).read_bytes())
    for dll in sorted(args.fixtures.glob("*.DLL")):
        volume.put_root(dll.name, dll.read_bytes())
    subprocess.run([shutil.which("nasm"), "-f", "bin",
                    str(ROOT / "tests/dpmi/QEXIT.ASM"),
                    "-o", str(work / "QEXIT.COM")], check=True)
    volume.put_root("QEXIT.COM",
                    (work / "QEXIT.COM").read_bytes())
    subprocess.run([shutil.which("nasm"), "-f", "bin",
                    str(ROOT / "tests/pe/XMSFREE.ASM"),
                    "-o", str(work / "XMSFREE.COM")], check=True)
    volume.put_root("XMSFREE.COM",
                    (work / "XMSFREE.COM").read_bytes())
    config = ["DOS=LOW", "FILES=40", "BUFFERS=20"]
    driver = {"raw": None, "xms": "HIMEMX.EXE",
              "vcpi": "JEMMEX.EXE"}[args.profile]
    if driver:
        config += ["DEVICE=C:\\FREEDOS\\BIN\\" + driver +
                   (" NOEMS X=TEST NOVME NOINVLPG" if args.
                    profile == "vcpi" else "")]
    config += [
        r"SHELL=C:\COMMAND.COM C:\ /E:1024 /P=C:\FDAUTO.BAT"]
    volume.put_root("FDCONFIG.SYS",
                    ("\r\n".join(config)
                     + "\r\n").encode("ascii"))
    batch = ["@echo off", "C:", "cd \\"]
    for n, name in enumerate(cases if args.leak else ()):
        batch += ["XMSFREE.COM > " + name + ".X0"]
        batch += [("ZRDXDPMI " if args.host else "") + name
                  + ".EXE > " + name + ".LOG"] * args.leak
        batch += ["XMSFREE.COM > " + name + ".X1"]
    for n, name in enumerate(() if args.leak else cases):
        batch += [("ZRDXDPMI " if args.host else "") + name
                  + ".EXE > " + name + ".LOG",
                  "if errorlevel 255 goto e" + str(n),
                  "if errorlevel 1 goto f" + str(n),
                  "echo 0 > " + name + ".STS", "goto d" + str(n),
                  ":e" + str(n), "echo 255 > " + name + ".STS",
                  "goto d" + str(n),
                  ":f" + str(n), "echo 1 > " + name + ".STS",
                  ":d" + str(n)]
    batch += ["echo OK > QDONE.LOG", ":spin",
              "goto spin"] if args.dump else [
                  "echo OK > QDONE.LOG", "QEXIT.COM"]
    volume.put_root("FDAUTO.BAT",
                    ("\r\n".join(batch)
                     + "\r\n").encode("ascii"))
    volume.file.close()
    command = [str(args.qemu), "-machine", "pc", "-accel",
               "tcg", "-cpu", args.cpu,
               "-m", str(args.mem), "-display", "none",
               "-serial", "none", "-monitor", "stdio",
               "-drive",
               "file=" + str(disk) + ",format=raw,if=ide",
               "-boot", "c",
               "-no-reboot", "-device",
               "isa-debug-exit,iobase=0xf4,iosize=0x04"]
    if args.watch or args.stop:
        command += ["-gdb", "tcp::12345", "-S"]
    if args.trace:
        command += ["-d", args.dflag, "-D",
                    str(work / "TRACE.LOG")]
    timed_out = False
    with (work / "QEMU.LOG").open("w") as log:
        proc = subprocess.Popen(command, stdin=subprocess.PIPE,
                                stdout=log,
                                stderr=subprocess.STDOUT)
        if args.stop:
            import threading
            threading.Thread(target=inspect,
                             args=(12345, args.stop, args.mon,
                                   (work / "INSPECT.LOG").open(
                                       "w"), proc.stdin,
                                   args.step, args.poke),
                             daemon=True).start()
        if args.watch:
            import threading
            wlog = (work / "WATCH.LOG").open("w")
            threading.Thread(target=watch,
                             args=(12345, args.watch, wlog),
                             daemon=True).start()
        try:
            if args.dump:
                time.sleep(args.dump)
                proc.stdin.write(
                    ('pmemsave 0x800000 0x10000 "'
                     + str(work / "MEM.BIN")
                     + '"\nquit\n').encode())
                proc.stdin.flush()
            status = proc.wait(timeout=args.timeout)
        except subprocess.TimeoutExpired:
            timed_out = True
            proc.kill()
            proc.wait()
            status = proc.returncode
    volume = Fat16(disk)
    failures = []
    if timed_out:
        failures.append("QEMU watchdog expired")
    elif status != 1:
        failures.append("unexpected QEMU exit status "
                        + str(status))
    try:
        done = volume.get("QDONE.LOG").strip() == b"OK"
    except FileNotFoundError:
        done = False
    if not done:
        failures.append("FreeDOS campaign incomplete")
    for name in (cases if args.leak else ()):
        try:
            free = [int(volume.get(name
                                   + x).split()[1]) for x in (
                                       ".X0", ".X1")]
        except (FileNotFoundError, IndexError, ValueError):
            failures.append(name
                            + " no XMS figures (need profile "
                              "xms or vcpi)")
            continue
        print(name, "XMS free KB before", free[0], "after",
              free[1])
        if free[0] != free[1]:
            failures.append(name + " leaks "
                            + str(free[0] - free[1]) + " KB")
    for name in (() if args.leak else cases):
        try:
            text = volume.get(name + ".LOG").decode("ascii",
                errors="replace")
            code = volume.get(name + ".STS").strip().decode()
        except FileNotFoundError:
            failures.append(name + " no result")
            continue
        (work / (name + ".LOG")).write_text(text)
        want = "0" if name in valid else "255"
        parts = expect.get(name, ())
        parts = (parts,) if isinstance(parts, str) else parts
        if code != want or any(p not in text for p in parts) or (
                name not in valid
                    and "ZRDX loader error:" not in text):
            failures.append(name + " failed (status " + code
                            + ")")
    for failure in failures:
        print(failure)
    print("FAIL" if failures else "PASS", args.profile,
          len(cases), "cases", work)
    return bool(failures)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError, RuntimeError,
            subprocess.SubprocessError) as exc:
        print("ERROR:", exc, file=sys.stderr)
        sys.exit(1)
