<div align="center">

```
 ______ ____  ______  __
|___  /|  _ \|  _ \ \/ /
   / / | |_) | | | \  /
  / /__|  _ <| |_| /  \
 /_____|_| \_\____/_/\_\
```

# 🔥 ZRDX — Zurenava DOS Extender 🔥

**A DOS4G/W-compatible 32-bit DOS extender — rebuilt with free tools, and still tiny.**

[![License: CC0](https://img.shields.io/badge/license-CC0%201.0-brightgreen?style=for-the-badge)](LICENSE)
[![Platform](https://img.shields.io/badge/platform-MS--DOS-blue?style=for-the-badge&logo=windowsterminal&logoColor=white)](#-quick-start)
[![Release](https://img.shields.io/badge/release-fix39-orange?style=for-the-badge)](docs/HISTORY.MD)
[![DPMI](https://img.shields.io/badge/DPMI-0.9%20%E2%80%94%2049%2F49-red?style=for-the-badge)](docs/D09COV.MD)
[![Assembler](https://img.shields.io/badge/core-NASM%20assembly-purple?style=for-the-badge)](#-building-from-source)

**⚡ stub 14 KB ⚡ DPMI 0.9: 49 of 49 ⚡ VCPI / XMS / RAW ⚡ up to 4 GB ⚡ LE · PE · RDOFF2 ⚡**

*Original author: Sergey Belyakov (1998-1999)
Open source edition: DosWorld (2026)*

</div>

---

## 🚀 What is it?

ZRDX is a **DOS extender**: a tiny stub that is *bound* to the start of your 32-bit
executable. The program starts, the stub switches the PC to protected mode, loads
the main module and hands it a flat 32-bit world — exactly what DOS/4G(W) does,
but small, free and hackable. No `DOS4GW.EXE` next to your program. 🎉

It is also a **standalone DPMI 0.9 host** (`zrxdpmi.exe`) for 16-bit and 32-bit clients.

| | |
|---|---|
| 🎯 **Targets** | programs built for **DOS4G/W**, **PMODE/W**, **DOS32A**, **WDOSX** with Watcom C/C++, TMT Pascal, LadSoft C, … |
| 📦 **Formats** | **LE** (DOS4G/W style), flat 32-bit **PE32** EXE (no imports, relocations supported), **RDOFF2** (`.rdf`, NASM + RLINK) |
| 🧠 **Memory** | all physical memory (up to **4 GB**); in a test with 3000 MB RAM one `malloc` returned a 2995 MB block (DOS/4GW: 62 MB) |
| 🔌 **DPMI** | **DPMI 0.9, all 49 specified INT 31h functions** — 42 full, 6 paging no-ops (memory is nonpageable), vendor lookup 0A00h without vendors. [Coverage report](docs/D09COV.MD) |
| 🖥️ **Hosts** | runs under **DPMI**, **VCPI**, **XMS** and **RAW**; it picks what is available |
| 🔁 **DOS calls** | translates everything PMODE/W translates (except identification) and almost all of DOS4G/W |
| 📁 **Long names** | **LFN API** (int 21h, AX=71xxh) with buffer translation |
| 📋 **Clipboard** | **Windows clipboard API** (int 2Fh, AX=17xxh), text and bitmaps translated both ways |
| 📏 **Size** | every packed stub is **14 336 bytes** |
| 🛠️ **Tools** | built with **NASM + Open Watcom + UPX** — no proprietary compilers |

> 💡 The list of all translated DOS functions is in [`src/extender.asm`](src/extender.asm).

---

## ⚡ Quick start

Everything you need is in [`bin/`](bin):

| File | What it is |
|---|---|
| `zrxbind.com` | binds a ZRDX stub to your program (a 2 KB `.COM`) |
| `stub-le.exe` | the stub for **LE** programs (DOS4G/W style) |
| `stub-pe.exe` | the stub for **PE** programs |
| `stub-rdf.exe` | the stub for **RDOFF2** (`.rdf`) programs |
| `zrxsetup.exe` | changes the settings of a bound program |
| `zrxdpmi.exe` | standalone **DPMI 0.9 host** (RAW / XMS / VCPI): `zrxdpmi PROGRAM.COM` |

> ⚠️ `zrxbind.com` takes the stubs **only from its own directory** (found via the DOS
> environment block, DOS 3.0+) — keep `zrxbind.com` and the `stub-*.exe` files together.
> The stub is chosen by the format of the input file; you do not pick it.

```bat
:: 1. build a 32-bit DOS4G/W-style program (Watcom example)
wcl386 -l=dos4g -bt=dos hello.c

:: 2. bind ZRDX to it
zrxbind -c hello.exe hellz.exe

:: 3. run it - no DOS4GW.EXE needed
hellz.exe
```

```
Zurenava DOS extender, version 0.52OSE. Copyright(C) 1998-1999, Sergey Belyakov
Copyright(C) 2026, Viacheslav Komenda
hello from 32-bit
```

Want a DPMI host for a separate DPMI client instead of binding? Run it through the host:

```bat
zrxdpmi myclient.com      :: the host owns discovery (1687h) for this command only
```

The first client fixes the bitness of the VM — do not mix 16-bit and 32-bit clients in one run.

---

## 🔗 Binding: `zrxbind`

The stub is **bound to the start of the executable**. `zrxbind` looks at the
input file and picks the stub: `LE` → `stub-le.exe`, `PE` → `stub-pe.exe`,
`RDOFF2` → `stub-rdf.exe`. To put it on an already linked program use `-r` or `-c`:

| Command | What it does |
|---|---|
| `zrxbind -r prog.exe` | **r**eplaces the old stub with ZRDX and makes a backup copy |
| `zrxbind -r -n prog.exe` | the same, **n**o backup copy |
| `zrxbind -c prog.exe out.exe` | **c**opies the program to a new file and binds ZRDX to it; the original stays untouched |
| `zrxbind -w zrdx.exe` | **w**rites the **LE** stub itself to a file (a copy of `stub-le.exe`) |

```bat
zrxbind -r -n doom.exe              :: bind to doom.exe, no backup
zrxbind -c doom.exe doomz.exe       :: doomz.exe = doom.exe + ZRDX
```

### 📚 The formats

| Format | What the loader does | Limits |
|---|---|---|
| **LE** | loads objects, applies fixups, creates selectors (16/32-bit, flat) | as in DOS4G/W |
| **PE** | allocates the image at any free address, loads sections, applies **base relocations** when the address differs from `ImageBase`, gives flat 4 GB code/data selectors, starts at the entry point | PE32/i386 EXE only; **no imports, DLLs, delay imports or TLS** (clear diagnostic); a `.reloc` directory is required unless the image lands on its `ImageBase`; stack = `SizeOfStackReserve` (64 KB … 4 MB) |
| **RDOFF2** | loads `code`/`data`/`bss`, applies relocations, starts at the exported symbol `start` (or at the code start); a return stub is on the stack, so `RET` ends the program | single module: no `import`, `library`, `segrelo`, `farimport`; `EAX`/`EBX` = linear address of the code, `EDX` = linear address of the PSP |

```bat
nasm -f rdf hello.asm -o hello.o        :: 32-bit code, global start
rlink rdf /o=hello.rdf hello.o          :: see the RTools project
zrxbind -c hello.rdf hello.exe          :: stub-rdf.exe is used
```

Want ZRDX bound at **link time**? Extract the stub and give it to `wlink`:

```bat
zrxbind -w zrdx.exe
wlink ... option stub=zrdx.exe
```

---

## 🔌 DPMI 0.9 coverage

The built-in host was checked against the DPMI 0.9 specification (in [`docs/`](docs)):

| Class | Calls | Notes |
|---|---|---|
| ✅ Full | **42** | descriptors, DOS memory, interrupts and exceptions, real-mode translation, callbacks, memory, physical mapping, virtual IF, **debug watchpoints 0B00h-0B03h** |
| 🟡 No-op | **6** | 0600h-0603h, 0702h, 0703h: page lock / demand paging succeed, because memory is nonpageable |
| 🔎 Lookup | **1** | 0A00h: optional vendor API lookup; no vendors, so every name returns CF=1 |

**49 of 49** functions are in the dispatch table, for 16-bit and 32-bit clients. Current
qualification: **55 native cases** and **17 bound API cases** on each of RAW, XMS and VCPI,
99 loader results and 11 tooling tests. Details, limits and evidence: [`docs/D09COV.MD`](docs/D09COV.MD),
[`docs/D09WORK.MD`](docs/D09WORK.MD), [`docs/D09ABI.MD`](docs/D09ABI.MD).

> Honest note: memory is **not swapped to disk**. A deferred plan exists
> ([`docs/SWAPPLAN.MD`](docs/SWAPPLAN.MD)); the dormant `src/vmm.asm` is not qualified.

---

## 🎛️ Settings: `zrxsetup`

```bat
zrxsetup [options] <filename>
```

`<filename>` is a program bound with `zrxbind`. Without switches `zrxsetup`
prints the current configuration; with switches it prints what changes and
writes the new values.

| Switch | Meaning | Default |
|---|---|---|
| `/X<hex>` | max. XMS/RAW memory to allocate, in **KB** | `FFFFFFFF` |
| `/L<hex>` | conventional memory to **leave free**, in paragraphs | `F000` |
| `/T<hex>` | size of the DOS **transfer buffer**, in paragraphs (max `1000`) | `400` |
| `/M<0/1>` | detection order: `0` = DPMI → VCPI, `1` = VCPI → DPMI | `0` |
| `/B<0/1>` | startup copyright banner: `0` = off, `1` = on | `1` |
| `/H`, `/?` | help | |

<details>
<summary><b>📖 What do these switches really do?</b> (click)</summary>

**`/X`** — RAW/XMS can't change the amount of memory on the fly, so the extender
takes *all* free memory at start. If your program then runs another one
(`exec`), the child gets no extended memory. If you plan to use `exec`, estimate
what your program needs and set this value. In VCPI and DPMI modes there is no
such problem: they allocate smartly.

**`/L`** — when the DPMI server is initialised, all DOS memory minus this value
joins the DPMI memory pool. Lowering it gives the client more memory. If you are
going to `exec`, keep the default (`0F000h`). Close to `0` lets you run small
programs even without extended memory.

**`/T`** — DOS works only with low memory, but extended-mode parameters can be
anywhere, so there is a buffer to carry data between the two worlds. A big read
is split into chunks of this size. A bigger buffer means fewer calls and faster
I/O, but less free low memory.

**`/M`** — order of probing protected-mode interfaces.
`0` = dpmi / vcpi / xms / raw (default), `1` = vcpi / dpmi / xms / raw. The first one that
works wins. The standard recommends DPMI first, but some DPMI servers are buggy
or provide VMM when it is not needed and only slow things down — that is why
most extenders (DOS4G/W included) prefer VCPI when it is present.

**`/B`** — no comment. *Banners must die!* 😄

</details>

### 💾 Where the settings live

The settings are the **last 14 bytes of the bound program**:

```
 offset  size  field
 ------  ----  -----------------------------------------------
  -14      4   signature "ZRXC"
  -10      2   flags: bit 0 = banner off, bit 1 = VCPI before DPMI
   -8      2   transfer buffer size (paragraphs)
   -6      4   max. XMS/RAW memory (KB)
   -2      2   low memory to leave free (paragraphs)
```

`zrxbind` writes the default block; the stub reads it at start (it opens its own
file anyway). Because it does not depend on the stub code, **the stub can be
UPX-packed** and still be configured. Programs bound by an older `zrxbind` have
no such block — bind them again.

### 🧩 Why so few switches?

You may notice there are far fewer settings than in PMODE/W or DOS32A. The ZRDX
kernel configures itself dynamically:

- **Page tables** (and low memory) are allocated in VCPI mode on demand, always
  from the top of memory — the addressable space stays up to 4 GB.
- **Physical-to-linear mappings** are allocated on demand, also up to 4 GB.
- **LDT** starts with one page (511 selectors) and grows up to 8191.
- **Stacks** are managed by an exclusive system and need no tuning. The only
  thing you may want to touch is the common DOS stack (you need just one); the
  default 512 bytes is plenty.
- **Real-mode callbacks**: more than 20 will not work under DPMI, because most
  DPMI servers limit them to 20.

---

## 🛠️ Building from source

Only **free tools**. No Borland C, no TASM, no TLINK, no MS link.

| Tool | Version | Used for |
|---|---|---|
| [**NASM**](https://nasm.us/) | 3.01 on the host, 0.98.39 in DOS | the whole stub and the binder |
| [**Open Watcom**](https://github.com/open-watcom/open-watcom-v2) | `wmake`, `wlink`, `wcl` | linking and the C tools |
| [**UPX**](https://upx.github.io/) | 5.x (tested with 5.1.0) | packing the stubs, `zrxbind.com` and `zrxsetup` |

Set the `WATCOM` environment variable, put the tools in `PATH` and run, in `src/`:

```bat
wmake                 :: zrxbind.com, zrxsetup.exe and the stubs (stub-le/pe/rdf.exe) in binder\
wmake STUB=zrdx.exe PESTUB=zrdxpe.exe RDFSTUB=zrdxrdf.exe   :: unpacked stubs
wmake install         :: copy zrxbind.com, zrxsetup.exe and the stubs to ..\bin
wmake clean
```

`makefile` assembles with `NASMOPT = -O9` (accepted by NASM 0.98.39 and 3.x). The full
reproducible build and check, which runs everything under DOSBox-X, is one command:

```sh
python3 tests/CHECK.PY --work /tmp/ZRXWORK
```

The runner needs `ZRDX_DOSCONF` (a dosbox-x config), `ZRDX_DRIVE_C` (DOS tools) and
`ZRDX_DRIVE_D` (Open Watcom); the scratch root `ZRDX_SCRATCH` defaults to `/tmp`.
All variables, the native host, QEMU qualification, installation and reports:
[`docs/BUILD.MD`](docs/BUILD.MD). Do not mix stubs from different builds.

### 🧪 Tests

| Where | What |
|---|---|
| `tests/CHECK.PY` | the full runner: builds in scratch, runs all suites, writes `RESULT.JSN` |
| `tests/rdf/` | about 50 self-checking programs for the RDF and LE loaders (each prints `ok` or `FAIL`) |
| `tests/dpmi/` | native DPMI host fixtures, 55 cases, RAW / XMS / VCPI, QEMU for CPU-fault checks |
| `tests/pe/`, `tests/mz/`, `tests/bind/` | generated PE, MZ-aligner and binder inputs, valid and damaged |
| `tests/clip/` | clipboard mock |
| `tests/mem/` | `malloc` size comparison with DOS/4GW and CauseWay |

Covered: DPMI selectors, memory stress (random allocate/resize/free with patterns), out-of-memory,
exception handlers, relocations, `start` lookup, PSP and registers at entry, 1 MB BSS, deep recursion,
DOS file I/O larger than the transfer buffer, LFN and clipboard translation.

### 📏 Memory available to `malloc`

`tests/mem` has a small Watcom C program (it prints the largest block one `malloc` can return) and batch files that build `memd.exe` (DOS/4GW), `memc.exe` (CauseWay) and `memz.exe` (ZRDX); see [`docs/MEMTEST.MD`](docs/MEMTEST.MD). In DOSBox-X with 3000 MB of RAM ZRDX gave one block of 2995 MB, DOS/4GW only 62 MB; CauseWay gave about 1 GB through its swap file (old measurement, recorded in [`docs/HISTORY.MD`](docs/HISTORY.MD)).

### 🔬 Experimental: VMM

`src/vmm.asm` is a dormant virtual memory manager. It is **not** part of any release build
and is not qualified. To try it, comment out `%assign Release 1` and uncomment
`%assign VMM 1` in `segdefs.asm`. Known problems: pages are not freed under VCPI (use RAW/XMS),
page unlocking is missing, and the swap file is reopened after each resize and not deleted on exit.
Plan: [`docs/SWAPPLAN.MD`](docs/SWAPPLAN.MD).

---

## 🗂️ Repository

```
bin/        prebuilt: zrxbind.com, zrxsetup.exe, stub-le.exe, stub-pe.exe,
            stub-rdf.exe, zrxdpmi.exe (standalone DPMI host)
src/        the sources (NASM) and the makefile
  zrdx.asm      main file, includes everything (builds stub-le.exe)
  zrdxpe.asm    PE stub root     zrdxrdf.asm  RDOFF2 stub root
  zrxdpmi.asm   standalone DPMI host (with nathost.asm)
  gmacros.asm   macros of virtual segments and labels
  segdefs.asm   segment layout and sizes
  extender.asm  DOS/BIOS call translation tables and handlers
  dpmifunc.asm  DPMI functions (int 31h)
  realinit.asm, protinit.asm  backend detection, entering PM
  prothand.asm, realhand.asm  interrupts and mode switches
  mem.asm, smm.asm            memory services (smm = active backend)
  leldr.asm / peldr.asm / rdfldr.asm   LE, PE32, RDOFF2 loaders
  binder/       zrxbind.com source (bnd.asm, a .COM program)
  aligner/      stub aligner
  zrxsetup.c    the settings utility
tests/      CHECK.PY and the suites above
docs/       BUILD.MD, D09COV.MD (DPMI coverage), backlog.txt, HISTORY.MD, plans
history.txt changes by version
LICENSE     CC0 1.0
```

Start with [`docs/README.MD`](docs/README.MD) — the project guide.

---

## 📜 History

Version **0.52OSE**: the project moved to NASM + Open Watcom + UPX, the settings were moved
to the end of the bound file, `zrxsetup` was rewritten, `zrxbind` became a `.COM` that reads the
stubs from its directory, PE and RDOFF2 loaders were added. Current release **fix39**: DPMI 0.9
hardening and the native `zrxdpmi.exe` host, 14 336-byte packed stubs. See
[`docs/HISTORY.MD`](docs/HISTORY.MD), the plans in [`docs/backlog.txt`](docs/backlog.txt) and
[`history.txt`](history.txt).

---

## ⚠️ Notes and limitations

- **Memory is not swapped to disk.** Swap-file paging is only a deferred plan ([`docs/SWAPPLAN.MD`](docs/SWAPPLAN.MD)).
- **DPMI:** 0.9 only; DPMI 1.0 is not implemented and not planned. There are no vendor extensions (0A00h answers `CF=1`).
- **PE:** one EXE, no imports, DLLs, delay imports or TLS. DLL loading is a deferred plan ([`docs/PEPLAN.MD`](docs/PEPLAN.MD)).
- **How it was verified:** the full campaign (55 native, 17 bound, 99 loader results) was recorded on the
  reference machine on 2026-10-18 under DOSBox-X and QEMU. CPU-fault checks and the VCPI debug-watch checks
  need QEMU with a FreeDOS image, which is not shipped. DOSBox-X alone cannot run them. On 2026-10-20 the PE
  loader (including relocations, 24 cases) and a fresh rebuild of all six binaries were re-checked; the QEMU
  campaign was not repeated.
- **Test setup is local:** the runners take DOSBox-X, the DOS tool tree, Open Watcom and the disk image from
  environment variables (see above); none of them is shipped.
- **Recorded source fingerprint:** `docs/TOOLS.JSN` holds the source hash from 2026-10-18; sources changed
  afterwards but a rebuild gives byte-identical binaries.
- **Specifications in `docs/`** (`DPMI09.HTM`, `DPMI10.TXT`, `VCPI.TXT`) are third-party documents kept for
  reference. Their copyright stays with the original owners; the CC0 license applies to the ZRDX sources only.
- **`src/vmm.asm` is experimental** and not part of any release build.

---

## ⚖️ License

[CC0 1.0](LICENSE). ZRDX was distributed by its author as **freeware**: it can be
freely copied and used for any purpose, commercial or not, and there are no
restrictions on software built by modifying or extending the ZRDX sources or
binaries. ZRDX is distributed **"AS IS"** — the author does not guarantee fitness
for any particular purpose and is not responsible for any consequences of using
or not using this product.

## 🙏 Credits

- **Sergey Belyakov** — author of ZRDX (1998-1999), homepage `http://www.zrdx.da.ru`
- **DosWorld** — open source edition (2026)

<div align="center">

**Made with 🔥 for the DOS people.** · Banners must die! · Long live protected mode!

</div>
