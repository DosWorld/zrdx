r"""Host settings shared by the test runners (environment variables).

ZRDX_SCRATCH  directory that may hold scratch trees (default /tmp)
ZRDX_DOSBOX   dosbox-x executable (default: dosbox-x from PATH)
ZRDX_DOSCONF  dosbox-x configuration used as the private template
ZRDX_DRIVE_C  DOS tools: BIN\NASM.EXE, RLINK.EXE, UPX.EXE, HX\BIN
ZRDX_DRIVE_D  Open Watcom tree (WATCOM\BINW, WATCOM\H)
ZRDX_DISK     FreeDOS FAT16 seed image for the QEMU runs
"""
import os
from pathlib import Path
import shutil


def scratch():
    return Path(os.environ.get("ZRDX_SCRATCH", "/tmp")).resolve()


def require(name):
    value = os.environ.get(name)
    if not value or not Path(value).exists():
        raise RuntimeError("set " + name + " to an existing path (see docs/BUILD.MD)")
    return Path(value)


def dosbox():
    return os.environ.get("ZRDX_DOSBOX") or shutil.which("dosbox-x") or require("ZRDX_DOSBOX")


def check_scratch(path):
    if not path.resolve().is_relative_to(scratch()):
        raise ValueError(str(path) + " must be inside ZRDX_SCRATCH=" + str(scratch()))
