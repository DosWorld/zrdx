# Build, run and install ZRDX

Toolchain recorded on 2026-10-17; current release verified on 2026-10-18. ZRDX uses NASM assembly and DOS Open Watcom
C/link tools. TP7, FPC and emu2 do not build this project.

## Dependencies and environment

- Host: NASM 3.01 and Python 3.9+ (tests/CHECK.PY and the other runners).
- DOSBox-X (tested with 2024.03.01) for the DOS build and fixtures.
- A DOS tool tree (drive C:) with BIN\NASM.EXE (0.98.39), UPX.EXE (5.1.0),
  RLINK.EXE and, for the HDPMI comparison, HX\BIN\HDPMI16/32.EXE.
- Open Watcom (drive D:): WATCOM\BINW with WMAKE, WLINK, WCL/WCL386
  (tested with version 2.0 beta dated 2025-03-21); the directory is
  WATCOM, not WATCOME.
- Optional: QEMU (tested with 10.1.2) and a FAT16 FreeDOS seed image.

The runners read the host settings from environment variables; nothing
is hard-coded. Missing required settings stop the run with a message.

| Variable | Meaning | Default |
|---|---|---|
| ZRDX_DOSCONF | dosbox-x.conf copied as the private template | required |
| ZRDX_DRIVE_C | folder mounted as C: (DOS tools) | required |
| ZRDX_DRIVE_D | folder mounted as D: (Open Watcom) | required |
| ZRDX_DOSBOX | dosbox-x executable | dosbox-x from PATH |
| ZRDX_SCRATCH | root of all scratch trees; --work must be inside | /tmp |
| ZRDX_DISK | FreeDOS FAT16 seed image for QEMU (QCHECK.PY --image) | none |
| ZRDX_CCDIR | folder with CC32W.EXE, LINK32W.EXE, LIB32W.EXE, include\, lib\ (task 184) | cases skipped |

```sh
export ZRDX_DOSCONF=$HOME/dosbox-x/dosbox.conf
export ZRDX_DRIVE_C=$HOME/dosbox-x/C
export ZRDX_DRIVE_D=$HOME/dosbox-x/D
```

A recursive search of C: found one NASM.EXE on the reference machine.
Host NASM produces OMF objects accepted by the installed DOS WLINK.
Inherited host warnings concern two 16-bit segment relocations emitted
inside 32-bit data and the old .8086 directive; no assembler errors occur.

## Reproducible build and checks

From the checkout on the host:

```sh
python3 tests/CHECK.PY --work /tmp/ZRDXWORK
```

The runner copies src/, bin/ and tests/ into scratch. It preserves the
user config as USER.CON and writes DOSBOX.CON with a private R: mount.
It assembles three OMF objects and the binder with host NASM -O9, runs
DOS wmake all, packs the binder with UPX and runs wmake install inside
scratch. The user's configuration and C:/D: files are unchanged.

DOS startup sets WATCOM=D:\WATCOM, INCLUDE=D:\WATCOM\H,
EDPATH=D:\WATCOM\EDDAT and PATH=C:\BIN;D:\WATCOM\BINW;Z:\;Z:\SYSTEM;Z:\BIN.
It replaces the actual [autoexec] section, avoiding C:\AUTOEXEC.BAT,
which starts interactive tools/HX and deletes temporary files.
SDL dummy video/audio permit unattended use; core=auto, cycles=max,
lfn=true and the copied 32 MB memory setting are used. --core selects
auto/normal/dynamic/full; --lfn selects true/false/auto in the private
configuration. The original config uses lfn=auto with no explicit DOS
version; its comments enable LFN automatically only with DOS >= 7.0.
Earlier campaigns skipped T40 twice. Those runs do not qualify LFN.
The default true setting requires both T40 formats to report success;
a skip now fails acceptance. ZRPACKREG and ZRPACKFIN both pass the full
99-result campaign with two T40 successes and no LFN skip. DOS clipboard integration
is disabled; tests/clip/MOCK.ASM supplies deterministic binary storage.

The runner checks targeted DPMI/PE/MZ/binder fixtures first, then builds
and runs the existing RDF/LE regression campaign. A per-phase watchdog
(default 900 seconds), a 4 MB fault-log limit and STOP.TXT in scratch
stop hung emulation. STATUS.LOG and RESULT.JSN propagate failures to
Python's exit status. A failed or incomplete run is not a passing result.
Source fingerprint BUILD.ID prevents --skip-build after source changes.
The runner leaves build products and logs in scratch for inspection.

```sh
python3 tests/CHECK.PY --work /tmp/ZRDXWORK --skip-build --bugs-only
python3 tests/CHECK.PY --work /tmp/ZRDXRAW --profile raw --bugs-only
python3 tests/CHECK.PY --work /tmp/ZRDXVCPI --profile vcpi --bugs-only
```

--bugs-only omits the long legacy campaign. XMS disables EMS; RAW also
disables XMS; VCPI enables EMS emm386 and active startup. HOST.LOG records
1687h, DE00h and 4300h discovery rather than inferring a provider from a
setting alone. HDPMI16/32 native comparisons and HDPMI32 bound LE/PE/RDF qualification
are recorded in D09RUNS.JSN; this does not substitute for built-in tests.
DOSBox-X 2024.03.01 fails the VCPI profile (PROBE, UNSUP, CBACK,
PHYSMAP, MAPEXIT, CBEXIT) with old and new tools alike; the same
cases pass under QEMU with JEMMEX (QCHECK.PY --profile vcpi), so
use QEMU for VCPI.
--supplied diagnoses existing bin artifacts without rebuilding; it is
not evidence that current sources build. --real-clip uses the DOSBox
clipboard and has different format/text semantics; see HISTORY.MD (archive STAGE1).

## Explicit native DPMI candidate

The native host is outside ordinary wmake all/install. Build and test it
in a private copy with the same NASM and DOS WLINK dependencies:

```sh
python3 tests/dpmi/NATIVE.PY --work /tmp/ZRDXNAT --build-only
```

NATIVE.PY defaults to DOSBox core=normal; --core auto/dynamic selects a
separate qualification profile (task 068). It copies the user's config
and records the actual core. --build-only creates ZRDXDPMI.EXE and COM
fixtures without claiming a test pass. Invoke ZRDXDPMI PROGRAM.COM in DOS
to run a native client; the host owns discovery only for that command.
The first successful client fixes VM bitness. Do not mix 16/32-bit
clients within one host invocation. A new invocation is a new VM.

--profile raw/xms/vcpi selects backend setup. With --qemu, QCHECK.PY uses
a private FreeDOS disk image, with HIMEMX for XMS and JEMMEX for VCPI.
The QEMU executable is an explicit dependency, not shipped by the repo.
Current evidence uses source-built QEMU 10.1.2, -cpu pentium and 32 MB.
The disk seed is $ZRDX_DISK (or --image); it is copied,
not modified. QCHECK.PY accepts --image for another compatible FAT16 seed.
NATIVE.JSN/RESULT.JSN record source/input hashes, provider and failures.

The native suite now has 55 cases, including IRQ16/32 suspended-kernel
EXEC, register sentinels, raw/state transitions and private-stack checks.
The default DOSBox selection has 51 cases (NMX and KST are
skipped without --qemu). QEMU adds NMX16/32
actual-#NM cases. Explicit --case NMX16/--case NMX32 is available on
DOSBox but the observed emulator profile does not deliver the TS trap;
this is a recorded failed qualification there, not a claimed pass.
The observed DOSBox profile also does not trap private-stack reads in
KST16/32; QEMU supplies these CPU-fault checks. The earlier host passed NEST16/32 on
normal, auto and dynamic. Current tasks 106-110 qualification uses QEMU for
all 55 cases on RAW/XMS/VCPI, plus 17 bound cases per backend and the
99-result DOSBox packed-loader suite. See D09R6.MD and fix39-* manifests.
The earlier 39-case DOSBox subset is historical, not a current full-suite
qualification. Current required CPU-fault evidence comes from QEMU.
See D09WORK.MD, D09RUNS.JSN and D09ABI.MD for current exact evidence.
No native artifact is installed by NATIVE.PY itself. The qualified
release now includes bin/zrdxdpmi.exe; see the installation section.

## DOS-only build

In a private source copy with the above DOS environment:

```bat
cd src
wmake all
wmake install
```

The makefile now defaults to numeric -O9, accepted by DOS NASM 0.98.39.
The former -Ox failed before assembly. Host NASM is the verified full
build route; the installed DOS NASM also builds the regression fixtures.
A separate DOS-NASM full-root timing probe was stopped before completion;
a complete DOS-only rebuild is not claimed by this stage.
No assertion of byte-identical output between assembler versions is made.

Assembly roots zrdx.asm, zrdxpe.asm and zrdxrdf.asm include the shared
runtime and select LE, PE and RDF loaders. NASM -> OMF -> DOS WLINK ->
UPX -9 --8086 -> aligner yields the default 14336-byte packed stubs.
Watcom WCL builds zrdxconf.exe and aligner.exe; binder/bnd.asm -> NASM
binary -> UPX yields zrdxbind.com. The user explicitly approved increasing
PSTUBSZ from 13312 to 14336 on 2026-10-18 (HISTORY.MD task 094).
stub-pmx.exe alone uses PMXSZ = 18944 (18432 approved 2026-11-05,
18944 on 2026-11-11). Runtime
resource capacities and initialized code totals are unchanged.

The unpacked 29184-byte forms remain available. Select them consistently
for both build and install:

```bat
del binder\stub-*.exe
wmake STUB=zrdx.exe PESTUB=zrdxpe.exe RDFSTUB=zrdxrdf.exe all install
```

Remove the generated binder stubs whenever switching variants because
make does not track a changed variable value. The make-variable selection
is verified by dry run; the unpacked binaries
retain their prior 99-result qualification. Do not mix format stubs from
different builds. The installed bin/ set uses the qualified packed forms;
D09WORK.MD/TOOLS.JSN identify hashes, tests and retained CPU-profile limits.

Source files over 1300 lines are split into two parts that
assemble as one text: nathost.asm, extender.asm, prothand.asm,
realinit.asm and dpmifunc.asm %include nathost2.asm,
extende2.asm, prothan2.asm, realini2.asm and dpmifun2.asm at
their end (task 150). Docs name the first part; the objects are
byte-identical to the unsplit ones.

## Installation and use

wmake install copies seven matching files into ../bin: zrdxbind.com,
zrdxconf.exe, zrdxdpmi.exe, stub-le.exe, stub-pe.exe,
stub-pmx.exe and stub-rdf.exe.
Keep them together: the binder loads its stubs relative to its own
executable. Back up the previous set before replacing it. Install only
after source build and regression checks pass. wmake also builds the
product zrdxdpmi.exe from src/zrdxnp.asm (host + PE loader);
NATIVE.PY --build-only builds the host-only zrdxdpmi.asm, and
--image zrdxnp the product, for the fixtures. TOOLS.JSN records the prior-set backup. There is
no remote upload/deployment target.

```sh
python3 tests/le/LECHK.PY --self
python3 tests/le/LECHK.PY --work /tmp/ZRDXLE
```

The LE check builds BADOBJ.ASM into an LE with a selector fixup,
clears the flags of the fixup target in a copy and expects exit
code 255 (not a hang) while the original exits with 42. --self
runs only the host part; the DOS part needs the ZRDX_* variables.

```bat
wcl386 -l=dos4g -bt=dos HELLO.C
R:\BIN\zrdxbind -c HELLO.EXE HELLZ.EXE
HELLZ.EXE
R:\BIN\zrdxconf HELLZ.EXE
```

-c creates a bound copy; -r replaces a stub in-place; -n suppresses the
backup. Existing PE support remains single-module PE32 without DLLs,
imports, TLS or delay imports. Proposed DLL APIs in PEPLAN.MD do not exist.

## Test input, converter and output relationships

- tests/rdf/*.ASM plus T.INC -> BUILD/ONE/ONER/ONELE/ONEN.BAT using
  DOS NASM, RLINK/WLINK and zrdxbind -> EXE/COM -> RUN.BAT -> RUN.LOG.
  Expected success/error statuses are in CHK/CHKN and the fixture code.
- tests/dpmi/*.ASM -> PROBE.BAT and ../rdf/ONE.BAT -> EXE/COM ->
  PROBE, UNSUP, CBACK, PHYSMAP, MAPEXIT, CBEXIT and HOST .LOG files.
  MAPEXIT/CBEXIT reuse the parameterized real-mode T59X launcher.
- tests/clip/MOCK.ASM -> DOS NASM -f bin -> MOCK.COM. It retains raw
  clipboard bytes for translation tests and chains unrelated INT 2Fh.
- tests/pe/HELLO.ASM, RELOC.ASM (linked at three bases) -> host NASM
  -> .BIN; GEN.PY -> 24 .PE inputs (3 valid, 21 invalid) and RUN.BAT
  -> zrdxbind -> EXE -> RESULT.LOG.
- tests/mz/GEN.PY -> six EXE inputs and RUN.BAT -> aligner -> RESULT.LOG;
  verify() checks exact payload preservation and unchanged rejected files.
- tests/bind/GEN.PY -> four BAD inputs and RUN.BAT -> binder -> RESULT.LOG.
- tests/CHECK.PY orchestrates these steps, verifies outputs and generates
  RESULT.JSN with provider identification and all five tool SHA256 values.
- tests/mem/MEM.C -> BUILD.BAT with Watcom -> MEMD/MEMC/MEMZ.EXE ->
  RUN.BAT. Its old stored numbers are not new qualification evidence.

Generated fixtures/binaries stay in scratch. Saved release evidence and
its provenance are described in D09R6.MD and the portable-record section below. Text changes use ASCII/CRLF
and DOS 8.3 names; modified C was formatted with astyle. wmake clean
removes build outputs only in the selected source directory.

The native example above only builds. Use the complete matrix below to
qualify all 55 native fixtures, including the per-backend auxiliary cases. A plain DOSBox default run still includes
KST16/32 and fails their required private-page fault assertions on the
observed CPU profiles; it is not a full release qualification command.

To reproduce independent-provider comparisons, build native fixtures,
assemble PORTABLE=1 LIM16/32 for valid-input comparison as documented in
D09WORK.MD, then run REFRUN.PY --work /tmp/ZRDXREF --fixtures
/tmp/PORTABLE --bound /tmp/ZRDXWORK. The --bound argument
names a qualified CHECK work tree; LE/RDF status 42 and quiet PE/PROBE
output are checked under HDPMI32. RUNS.TSV -> REPORT.PY -> D09RUNS.JSN
retains the actual final input hashes; SIZES.PY and PERF.PY generate the
size and latency records. No generated fixture is an input source.

## Portable qualification records

CATALOG.PY is the shared native/bound/auxiliary expectation source.
Run REPORTUT.PY for host-side negative validation tests. The checked-in
RUNS.TSV points to small source manifests under docs/EVID using relative
paths and SHA256 hashes, never to scratch directories. To regenerate:

```sh
python3 tests/dpmi/REPORTUT.PY
python3 tests/dpmi/REPORT.PY
python3 tests/dpmi/LEDGER.PY
python3 tests/dpmi/PERF.PY
```

For a new package, archive the exact completed runner manifests under new
DOS8.3 EVID names, mark prior gates historical and add the seven current
native/bound/legacy gates with matching source/tool hashes in TOOLS.JSN.
REPORT.PY rejects absent/partial/stale gates before writing output. It does
not rebuild executables or manufacture missing success fields. Preserve
archived sources byte-for-byte; update the index hash only for a newly
reviewed source record. Temporary FreeDOS images need not be retained.
ASSERT.TSV is manually authored clause-to-assertion input; LEDGER.PY checks
its anchors and fixture evidence and writes D09ASRT.TSV. MANUAL-CONTRACTS
labels are reviewed links, not an automated proof of full conformance.
ASSERT.TSV distinguishes standard, host-policy and cpu-contract entries;
there are 28 current assertions. EVID also retains three unindexed early
bound-run manifests, moved from the docs root during cleanup.

## Hardening and matched timing (tasks 106-110)

D09R6.MD distinguishes normative clauses from host policies and records the
bounded exhaustion/rollback/nested-event tests. Use --timeout 600 for the
expanded native QEMU campaign on a busy host; this is a runner watchdog,
not a change to DOS memory, callback or stack limits. For matched timings:

```sh
python3 tests/dpmi/QCHECK.PY --qemu /tmp/QEMU09/BUILD/qemu-system-i386 \
  --fixtures /tmp/ZRDXNAT --native-host /path/to/previous/ZRDXDPMI.EXE \
  --work /tmp/ZRTPRE --profile raw --case LATENC --icount
python3 tests/dpmi/QCHECK.PY --qemu /tmp/QEMU09/BUILD/qemu-system-i386 \
  --fixtures /tmp/ZRDXNAT --native-host /tmp/ZRDXNAT/ZRDXDPMI.EXE \
  --work /tmp/ZRTPOST --profile raw --case LATENC --icount
python3 tests/dpmi/PERF.PY --before /tmp/ZRTPRE --after /tmp/ZRTPOST
```

Save the previous installed host before replacement. Use the same freshly
built LATENC.COM for both runs. The checked-in EVID/PERF39.JSN contains
samples and manifests; `python3 tests/dpmi/PERF.PY` regenerates the table
without disk images. Do not interpret icount virtual ticks as hardware
cycles. Old nine-sample timing inputs do not match the new five-path schema.
CHECK.PY records the source ID captured for its build, not a new ID read
from potentially edited sources after a long run.

## Complete current release matrix

One command (task 202): `python3 tests/dpmi/MATRIX.PY --qemu
PATH --image SEED.IMG --work DIR` runs everything below with its
own work dirs; the commands that follow are the same steps by hand.

Run from the checkout with fresh private work directories. Build the native
host once with --build-only before invoking QCHECK.PY: this makes its build
manifest available for source identity checks. Merely selecting --native-host
in QCHECK.PY does not select the native case catalog; pass every case below.
VERIFY.PY must build CONTROL for the matching real/VCPI profile.
The commands use the existing recorded environment, not an independent host.

```sh
python3 tests/CHECK.PY --work /tmp/ZRLOAD
python3 tests/dpmi/NATIVE.PY --work /tmp/ZRNAT --build-only
python3 - <<'PYRUN'
from pathlib import Path
import runpy
import subprocess
import sys

qemu = '/tmp/QEMU09/BUILD/qemu-system-i386'
native = Path('/tmp/ZRNAT')
cases = runpy.run_path('tests/dpmi/CATALOG.PY')['NATIVE']
for profile in ('raw', 'xms', 'vcpi'):
    command = [sys.executable, 'tests/dpmi/QCHECK.PY', '--qemu', qemu,
               '--fixtures', str(native), '--native-host', str(native / 'ZRDXDPMI.EXE'),
               '--work', '/tmp/ZRN' + profile.upper(), '--profile', profile,
               '--timeout', '600']
    extra = [] if profile == 'vcpi' else ['DRSAVE', 'A20R' if profile == 'raw' else 'A20X']
    for name in list(cases) + extra:
        command += ['--case', name]
    subprocess.run(command, check=True)
    bound = Path('/tmp/ZRB' + profile.upper())
    subprocess.run([sys.executable, 'tests/dpmi/VERIFY.PY', '--work', str(bound),
                    '--tools', '/tmp/ZRLOAD/bin', '--profile', profile,
                    '--build-only', '--timeout', '600'], check=True)
    subprocess.run([sys.executable, 'tests/dpmi/QCHECK.PY', '--qemu', qemu,
                    '--fixtures', str(bound / 'tests/dpmi'), '--work', str(bound / 'QEMU'),
                    '--profile', profile, '--timeout', '600'], check=True)
PYRUN
python3 tests/dpmi/SIZES.PY --work /tmp/ZRSIZE \
  --native /tmp/ZRNAT --ordinary /tmp/ZRLOAD
```

QCHECK counts include HOST: native RAW/XMS have 58 cases (55 plus discovery
and two auxiliaries); native VCPI has 56. Each bound campaign has 18
(17 plus discovery). The ordinary campaign has 99 successful results,
including two LFN passes. Build-only results never count as passing gates.
See the portable-record section before archiving a new release. Do not
replace current qualification records merely because a build completed.

## Workspace hygiene

Keep generated COM/EXE/OBJ files, logs, traces and emulator images in the
private working copies. bin/ is the installed release, not disposable
build scratch. tests/mem retains sources and batch scripts; its three
EXEs must be rebuilt as described in MEMTEST.MD. Closed review documents
are consolidated in HISTORY.MD. EVID manifests and converter inputs are
retained because report regeneration and historical comparisons use them.

## VENDOR test (experimental)

The vendor entry (task 149) is built only with %assign VENDOR 1
in src/segdefs.asm. To test it on QEMU (RAW, XMS, VCPI):

    python3 tests/dpmi/VENDRUN.PY --qemu <qemu-system-i386>
        --image <FreeDOS image> --work <dir in ZRDX_SCRATCH>

It copies the tree, enables VENDOR, runs CHECK.PY and then the
VENDOR fixture on each profile. The native host is not built
with the entry.

## SWAP test (experimental)

SWAP builds are not part of the ordinary build. To check the
demand-paging code on QEMU (RAW and XMS, 8 MB guest):

    python3 tests/swap/SWAPRUN.PY --qemu <qemu-system-i386>
        --image <FreeDOS image> --work <dir in ZRDX_SCRATCH>

It copies the tree, enables SWAP, runs CHECK.PY, builds the
SWAPT fixture with the SWAP stub and runs it with QCHECK.PY
(--mem sets guest RAM). The same fixture fails on ordinary
stubs and under VCPI, which is the control.

SWAPRUN.PY runs SWAPT (demand paging, locks), SWAPF (store
full: allocation fails, stack stays locked), SWAPC (crash:
the swap file must not be left) and SWAPK (long TEMP path
with an LFN driver) on RAW and XMS, for bound programs and
for the native host. A full cycle takes over ten minutes.
The swap file lives in the directory named by TEMP (else
TMP, else the current one); long names need an LFN driver.
SWAP-only ceilings: Text 10340 (native 17956), Text16 912
(native 1200), IText16 2160 (native 1488), Data 400, packed
stubs 15872. A SWAP build does not change ordinary builds.

SWAPRUN.PY --fail builds a tree with SWAPFAIL as well and runs
SWAPW (write error) and SWAPR (read error) on RAW; see
SWAPDES.MD.

The swap size of the native host is in its command line:

    zrdxdpmi /S20 PROG.EXE

The swap size of bound programs is set with zrdxconf:

    zrdxconf /S20 PROG.EXE

/S takes hex megabytes (0-80, 0 = off, the default). The
fixtures use /S10 (16 MB).

If the file has no ZRDX block, zrdxconf appends one with the
defaults (flags 0, swap 0, transfer buffer 400h, XMS FFFFFFFFh,
low memory F000h), then applies the options. A rejected option
leaves the file unchanged.

## Win32 DLLs (tasks 178, 179)

KERNEL32.DLL and USER32.DLL are built in DOS with Open Watcom:
`cd win32` and `BUILD.BAT` (BUILD.LOG lists compiler errors and
"NOT BUILT" if a DLL is missing). CHECK.PY builds them for its
tests. For a release copy them to bin/: a Win32 program finds
them next to itself or in the current directory. The same
build makes MSVCRT.DLL and UCRTBASE.DLL (docs/MSVCRT.MD; both
link the objects listed in win32/CRTCOM.LNK; the math part is
NASM, CRTM.ASM and CRTJMP.ASM). They are not
part of the stub set, so TOOLS.JSN does not hash them.
