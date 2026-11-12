# ZRDX PE SDK

Programs that run on stub-pmx.exe (a PE with imports or DLLs,
chosen by zrdxbind) may load DLLs at run time. The API is the
vendor entry "ZRDX" (INT 2Fh AX=168Ah); sdk/ZRDXAPI.INC wraps it.

## Use from NASM

```
        jmp start
%include "ZRDXAPI.INC"      ; nasm -I sdk/
start:  call ZrdxInit       ; once, before any other call
```

The include file holds cdecl functions at fixed offsets from the
start of the code section: LoadLibrary 20h, GetModuleHandle 40h,
GetProcAddress 60h, FreeLibrary 80h, GetLastError A0h, ZrdxInit
C0h. Export the first five from the EXE (export RVA = section RVA
plus the offset) and DLLs import them from the running EXE by its
file name. tests/pe/RT.ASM, PLUGX.ASM and GEN.PY show a complete
example including the export and import tables.

## Use from Open Watcom C

```c
#include "zrdxapi.h"        /* wcc386 -s -ecc -bt=nt -i=sdk */
if (ZrdxInit()) fail();     /* 0 = the service answers */
lib = LoadLibrary("PLUGC.DLL");
fn = (int (*)(void))GetProcAddress(lib, "_PlugVal");
```

ZRDXAPI.H holds the same wrappers as macros over `#pragma aux`
far calls. Build without a C runtime: `-s` (no stack check call)
and `-ecc` (cdecl) for the compiler, `option nodefaultlibs` and
`option start=_mainCRTStartup` for wlink. A C name has an
underscore prefix in the export table (`_PlugVal`). wlink cuts a
module name at the first dot, so a DLL that imports from the EXE
is linked against a 9 character placeholder that the converter
patches to the EXE name (tests/pe/c/BUILD.BAT, GEN.PY). Example:
tests/pe/c/HOSTC.C and PLUGC.C.

## Functions

| Function | Arguments | Result |
|---|---|---|
| LoadLibrary | path | handle, 0 on error |
| GetModuleHandle | name | handle, 0 on error |
| GetProcAddress | handle, name or ordinal (< 10000h) | address, 0 |
| FreeLibrary | handle | 0 or an error code (unloads) |
| GetLastError | - | last error code |

Names compare without case for modules and with case for
symbols. Errors: 1 bad function, 2 file not found, 3 bad image,
4 no memory, 5 module not found, 6 symbol not found, 8 main
module, 9 name conflict, 10 forwarder (not yet), 11 busy, 12
unresolved import, 13 DllMain refused, 14 the DPMI host
lacks a function (it answered 8001h to an allocation).

ZrdxInit returns 0 if the service answers INT 2Fh 168Ah
"ZRDX"; any other value (the INT 2Fh code, 8Ah if no handler)
means the program does not run on stub-pmx.exe or zrdxdpmi.exe:
check it and print a message before calling the other wrappers.
Host requirements: DPMI 0.9 with 0501h and 0502h (memory) for
the DLL service, and DOS calls through protected mode.

## Limits

LoadLibrary needs about 2 KB of stack. It fails with 11 while
another load or unload runs, also when called from DllMain.
FreeLibrary unloads every module that is no longer reachable
from the main module or an explicit handle; a DLL must not free
itself from its own code. Return from the entry point (not
INT 21h/4Ch) detaches the remaining DLLs. DLL files are
searched as given, in the directory of the program and in the
current directory.

## Vendor functions 6 and 7

Function 6 (ProgramInfo) fills DS:EDX (128 bytes) with the path
of the main program and returns its image base in EAX and its
module handle in EBX. Function 7 (Exit) takes the exit code in
EDX, detaches all DLLs like the exit stub and ends the program.
INT 2Fh 168Ah returns the entry in ES:EDI; ES is the selector of
the entry, so a program that needs ES = DS afterwards restores
it (ZRDXAPI.INC and ZRDXAPI.H do).
