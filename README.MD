<div align="center">
```
 ______ ____  ______  __
|___  /|  _ \|  _ \ \/ /
   / / | |_) | | | \  /
  / /__|  _ <| |_| /  \
 /_____|_| \_\____/_/\_\
```
</div>

# 🔥 ZRDX — Zurenava DOS Extender 🔥

**A DOS4G/W-compatible 32-bit DOS extender, rebuilt with free tools.**

[![License: CC0](https://img.shields.io/badge/license-CC0%201.0-brightgreen?style=for-the-badge)](LICENSE)
[![Platform](https://img.shields.io/badge/platform-MS--DOS-blue?style=for-the-badge&logo=windowsterminal&logoColor=white)](#-quick-start)
[![Version](https://img.shields.io/badge/version-0.50OSE-orange?style=for-the-badge)](history.txt)

**⚡ stub 12 KB ⚡ DPMI 0.9 ⚡ VCPI / XMS / RAW ⚡ up to 4 GB ⚡**

*Original author: Sergey Belyakov (1998-1999) · Open source edition: Viacheslav Komenda (2026)*


---

## 🚀 What is it?

ZRDX is a **DOS extender**: a tiny stub that is *bound* to the start of your 32-bit
executable. When the program starts, the stub switches the PC to protected mode,
loads the main module and gives it a flat 32-bit world — exactly what DOS/4G(W)
does, but small, fast and free.

| | |
|---|---|
| **Targets** | programs built for **DOS4G/W**, **PMODE/W**, **DOS32A**, **WDOSX** with Watcom C/C++, TMT Pascal, LadSoft C, … |
| **Memory** | access to all physical memory (up to **4 GB**) |
| **DPMI** | DPMI 0.9 compatible (int 31h), except functions 0A00h-0B03h |
| **DOS calls** | translates all functions translated by PMODE/W (except identification) and almost all of DOS4G/W's |
| **Hosts** | runs under **DPMI**, **VCPI**, **XMS** and **RAW** (it picks what is available) |
| **Size** | the packed stub is only **12 800 bytes** |
| **Tools** | built with **NASM + Open Watcom + UPX** — no proprietary compilers |

> 💡 If you call DOS directly, the list of all translated functions is in
> [`src/extender.asm`](src/extender.asm).

---

## ⚡ Quick start

Everything you need is in [`bin/`](bin):

| File | What it is |
|---|---|
| `zrxbind.exe` | binds the ZRDX stub to your program (the stub is built in, UPX-packed) |
| `zrxsetup.exe` | changes the settings of a bound program |
| `stub-le.exe` | the same stub as a standalone file (for `wlink`) |

```bat
:: 1. build a 32-bit DOS4G/W-style program (Watcom example)
wcl386 -l=dos4g -bt=dos hello.c

:: 2. bind ZRDX to it
zrxbind -c hello.exe hellz.exe

:: 3. run it - no DOS4GW.EXE needed
hellz.exe
```

```
Zurenava DOS extender, version 0.50OSE. Copyright(C) 1998-1999, Sergey Belyakov
Copyright(C) 2026, Viacheslav Komenda
hello from 32-bit
```

---

## 🔗 Binding: `zrxbind`

The stub is **bound to the start of the executable**. To put it on an already
linked program use the commands `-r` or `-c`:

| Command | What it does |
|---|---|
| `zrxbind -r prog.exe` | **r**eplaces the old stub with ZRDX and makes a backup copy |
| `zrxbind -r -n prog.exe` | the same, **n**o backup copy |
| `zrxbind -c prog.exe out.exe` | **c**opies the program to a new file and binds ZRDX to it; the original stays untouched |
| `zrxbind -w zrdx.exe` | **w**rites the stub itself to a file |

```bat
zrxbind -r -n doom.exe              :: bind to doom.exe, no backup
zrxbind -c doom.exe doomz.exe       :: doomz.exe = doom.exe + ZRDX
```

Want ZRDX bound at **link time**? Extract the stub and give it to `wlink`:

```bat
zrxbind -w zrdx.exe
wlink ... option stub=zrdx.exe
```

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
| `/M<0/1>` | detection order: `0` = DPMI → VCPI, `1` = VCPI → DPMI | `1` |
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
`0` = dpmi / vcpi / xms / raw, `1` = vcpi / dpmi / xms / raw. The first one that
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
| [**NASM**](https://nasm.us/) | **0.98.39** (or newest) | the whole stub and the binder |
| [**Open Watcom**](https://github.com/open-watcom/open-watcom-v2) | `wmake`, `wlink`, `wcl` | linking and the C tools |
| [**UPX**](https://upx.github.io/) | 5.x (tested with 5.1.0) | packing the stub and `zrxsetup` |

Set the `WATCOM` environment variable, put the tools in `PATH` and run, in `src/`:

```bat
wmake                 :: zrxbind.exe + zrxsetup.exe (with the UPX-packed stub)
wmake STUB=zrdx.exe   :: the same, but embed the unpacked stub
wmake zrdxu.exe       :: only the UPX-packed stub
wmake install         :: copy zrxbind.exe, zrxsetup.exe, stub-le.exe to ..\bin
wmake clean
```

### 🔬 Experimental: VMM

`src/vmm.asm` is an experimental virtual memory manager. To try it, comment out
`%assign Release 1` and uncomment `%assign VMM 1` in `segdefs.asm`. It is very
incomplete: pages are not freed under VCPI (so use it with RAW/XMS only), page
unlocking is not implemented, and the swap file handling is not quite right (it
has to be reopened after each resize and deleted on exit). Its speed is no worse
than DOS/4G(W).

---

## 🗂️ Repository

```
bin/        prebuilt: zrxbind.exe, zrxsetup.exe, stub-le.exe
src/        the sources (NASM) and the makefile
  zrdx.asm      main file, includes everything
  gmacros.asm   macros of virtual segments and labels
  segdefs.asm   segment layout and sizes
  extender.asm  DOS/BIOS call translation tables and handlers
  dpmifunc.asm  DPMI functions (int 31h)
  loader.asm    LE loader
  binder/       zrxbind (bnd.asm)
  aligner/      stub aligner
  zrxsetup.c    the settings utility
history.txt  changes by version
LICENSE      CC0 1.0
```

---

## 📜 History

Version **0.50OSE**: the project moved to NASM 0.98.39 + Open Watcom + UPX, the
settings were moved to the end of the bound file, `zrxsetup` rewritten. Full
history in [`history.txt`](history.txt).

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
