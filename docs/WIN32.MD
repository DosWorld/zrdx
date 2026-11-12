# Win32 console layer for ZRDX (investigation, task 174)

Status: stages 1 and 2 (tasks 178-188) done; stage 4 (MSVCRT.DLL,
docs/MSVCRT.MD) started with task 189.

## Goal

Run console Win32 .exe files (PE32, subsystem console) under
zrdxdpmi.exe and stub-pmx.exe. The PE loader, imports by name,
forwarders and DllMain exist (docs/PEABI.MD). Missing are the
DLLs the programs import.

## Clean room rule

KERNEL32.DLL, USER32.DLL and MSVCRT.DLL are written from the
public API documentation (function names, arguments, results,
error codes) and from black-box tests of our own. Not allowed:
reading or copying code, headers or tables of Windows, ReactOS,
Wine, MinGW or other implementations; disassembly of Microsoft
binaries. Allowed sources are listed in docs/CLEANRM.MD (to be
created with the first stage) and every export gets a test.

## Measured need (Open Watcom hello world)

`wcl386 -zq -bt=nt -l=nt W.C` (puts("hello")) gives a 23 KB PE
that imports 32 functions:

- USER32: CharUpperA.
- KERNEL32 (31): CloseHandle CreateEventA CreateFileA
  ExitProcess FlushFileBuffers GetACP GetCPInfo GetCommandLineA
  GetCommandLineW GetCurrentThreadId GetFileType GetLastError
  GetModuleFileNameA GetModuleFileNameW GetModuleHandleA GetOEMCP
  GetProcAddress GetStdHandle GetVersion LoadLibraryA
  MultiByteToWideChar SetConsoleCtrlHandler
  SetEnvironmentVariableA SetFilePointer SetStdHandle
  SetUnhandledExceptionFilter UnhandledExceptionFilter
  VirtualAlloc VirtualFree VirtualQuery WriteFile.

The Watcom start-up does not use FS:[0] (no SEH chain), no TLS
directory and no threads. MSVC and MinGW programs need more: the
TEB at FS:[0..30h], SEH frames, TLS, MSVCRT.DLL or the UCRT
forwarders (api-ms-win-crt-*).

## Design decisions

1. Language: C89 compiled by Open Watcom (-s, nodefaultlibs, no
   C runtime), a little NASM. Recipe: docs/SDK.MD, tests/pe/c.
2. Calling convention: Win32 is stdcall (callee pops). DLL
   functions use __stdcall and are exported under the plain
   name (wlink: export ExitProcess=_ExitProcess@4).
3. DOS under the DLL: INT 21h from the flat client (the extender
   translates buffers); file calls use the LFN functions first
   (716Ch) like the loader. A HANDLE is (DOS handle + 1) * 4;
   GetStdHandle gives 4, 8, 12 for input, output, error.
4. Memory: VirtualAlloc/Free/Query and Heap* sit on DPMI 0501h/
   0502h blocks plus a chunked small-record allocator (no big
   static arrays, no leaks at exit, PEExit detaches the DLL).
5. Process: ExitProcess = DLL detach is done by PEExit, then INT
   21h 4Ch. GetProcAddress/LoadLibraryA map to the vendor API
   (ZrdxInit in DllMain).
6. Threads: one thread; GetCurrentThreadId returns 1,
   CreateThread fails with a documented error.
7. TEB/PEB (task 204): K32TEB.C builds one page behind a DPMI
   selector: TEB at +0 (chain 0, stack base 4, limit 8, Self
   18h, PID 20h, TID 24h, static TLS 2Ch, PEB 30h, last error
   34h, TlsAlloc slots 0-63 at E10h, the others via F94h),
   PEB at +1000h (image base 8, process heap 18h, OS version
   A4h-ACh). FS reaches the program through vendor function 8
   (docs/PEABI.MD). Stack base: page-aligned top near DllMain;
   limit: SizeOfStackReserve of the image.
8. Exceptions (task 205): K32EXC.C and K32EXA.ASM. KERNEL32
   hooks DPMI exceptions 0 4 5 6 12 13 14 16 17 (0203h; the old
   handlers return at exit). A stub runs on the host stack and
   builds
   a CONTEXT (x86 layout, 2CCh bytes) on the faulting stack,
   points the frame at w32_dispatch and returns, so SEH handlers
   run on the program stack (like KiUserExceptionDispatcher).
   w32_dispatch fills an EXCEPTION_RECORD (NT status of the
   exception), walks the FS:[0] chain (frames must be aligned,
   ascending, below the stack base) and calls each handler
   (cdecl: record, frame, context, dispatcher context). Result 0
   resumes from the possibly edited CONTEXT (w32_resume); a
   noncontinuable record raises C0000025h. No frame handles it:
   UnhandledExceptionFilter (the user filter, else 1); -1
   resumes, else the process ends with the status (low byte)
   and a message.
   RaiseException walks the chain without a CPU exception.
   Limits: access violation parameters are 0 (the DPMI frame has
   no CR2), the stubs assume DS and ES are flat, INT 1 and 3
   stay with the debugger, a stack overflow cannot be handled.
   The x87 state is saved with FNSAVE into FloatSave and
   restored by the resume (control word, status, registers).
10. Static TLS (tasks 207-209): the loader builds the block of
    every module with a TLS directory (EXE and DLLs) and calls the
    TLS callbacks (docs/PEABI.MD); KERNEL32 only stores the
    loader table (vendor function 9) in FS:[2Ch]. Tests TTLS
    (EXE) and TTLD (DLL) add the directory with ADDTLS.C.
11. Compatibility (task 184): the Win32 tools of the supplied
    compiler (CC32W, LINK32W, LIB32W, only KERNEL32 imports) run
    unchanged. With ZRDX_CCDIR set CHECK.PY compiles and links
    CCRET.C (exit code 7) and CCPRN.C (printf) with them, runs
    both results and lists libraries (tests/win32/CCRUN.BAT).
    Other programs measured but not targeted: see the history.
12. USER32 (task 210): wsprintf and wvsprintf A/W (U32FMT.C;
    1024 character limit, wide to ANSI is truncation), MessageBox
    and MessageBoxEx A/W print "[caption] text" on stdout and
    return the first button, GetKeyState/GetAsyncKeyState return
    0, GetSystemMetrics knows SM_CXSCREEN 640 and SM_CYSCREEN
    400, CharToOem/OemToChar convert through the code pages.
13. KERNELBASE.DLL (task 211): wlink cannot write PE
    forwarders, so win32/GENFWD.PY writes a code-less DLL whose
    exports forward "KERNEL32.name" (source: the export list of
    KERNEL32.LNK; output: win32/KERNELBA.DLL, installed as
    KERNELBASE.DLL, a long name that needs the LFN API). Test:
    tests/win32/TKBS.C.
14. NTDLL.DLL (task 212, NTDLL.C): RtlUnwind, Rtl(Get|Set)
    LastWin32Error, RtlFill/Zero/MoveMemory, Rtl(Allocate|Free|
    ReAllocate)Heap over KERNEL32, RtlGetVersion (10.0.10240, the
    EXW form too) and NtTerminateProcess (own process only).
    Test: tests/win32/TNTD.C (imports by TNTD.LNK).
15. ADVAPI32.DLL (task 213, ADVAPI32.C): GetUserName A/W
    (USERNAME or "user"), RegOpenKeyEx and RegQueryValueEx A/W
    (no registry: error 2), RegCloseKey, OpenProcessToken and
    GetTokenInformation (access denied) and SystemFunction036
    (RtlGenRandom, xorshift from the tick count). Test: TADV.C.
16. SHELL32.DLL (task 214, SHELL32.C): CommandLineToArgvW
    (documented quote and backslash rules, empty line gives the
    program path, one LocalAlloc block), SHGetFolderPath A/W and
    SHGetSpecialFolderPathA (the usual CSIDLs give the current
    directory, others E_INVALIDARG), ShellExecute A/W (always 2).
    Test: TSHL.C.
9. RtlUnwind (task 206, KERNEL32 export): calls the handler of
   every frame below the target (all for NULL) with EH_UNWINDING
   (and EH_EXIT_UNWIND for NULL), record NULL -> status
   C0000027h, unlinks each frame from FS:[0]; the target stays
   the head. Disposition 3 (collided) continues at the frame the
   handler stored in the dispatcher context. A target that is
   not in the chain raises C0000029h. A handler that handles an
   exception calls RtlUnwind and then jumps into its code, as
   MSVC does; nothing else in the dispatcher is needed.

## Stages

1. KERNEL32 skeleton and the 31 functions above, USER32
   CharUpperA; own Win32 test exes built with Open Watcom
   (-bt=nt, no CRT first). Acceptance: W.EXE prints hello.
2. KERNEL32 breadth: files and directories (LFN), environment,
   time, code pages, console modes, heap, TLS API; tests per
   export.
3. USER32 subset (CharUpper/Lower, wsprintfA, MessageBoxA as
   text, GetKeyState, GetSystemMetrics) and the small DLLs
   (NTDLL, KERNELBASE forwarders, ADVAPI32, SHELL32 subset).
4. MSVCRT.DLL: own C runtime subset (stdio, string, memory,
   process start-up exports such as __getmainargs, _initterm,
   __set_app_type), then the api-ms-win-crt-* forwarders to a
   UCRTBASE.DLL. Tests: Watcom exes that import MSVCRT through
   own import directives.
5. TEB/PEB and SEH: an FS selector with a TEB, exception frames
   from DPMI exceptions to EXCEPTION_RECORD, RtlUnwind. Needed
   for MSVC/MinGW binaries only.
6. Compatibility: third-party console programs, measured and
   listed in docs/WIN32.MD.

## Risks and open points

- No MSVC or MinGW compiler here: stages 4-6 are tested with
  Watcom-built programs and, if the user supplies them, real
  third-party binaries.
- Size: each DLL image takes extended memory only; DOS memory
  is unchanged. The loader paths (EText, IEText) may need
  small fixes found by real binaries.
- Clean room discipline: the author works from documentation
  only; tests encode documented behaviour.

## Stage 1 result (tasks 178 and 179)

KERNEL32.DLL (win32/KERNEL32.C, K32FILE.C, K32MEM.C, K32OBJ.C,
K32STR.C) exports 34 functions and USER32.DLL one. The plain
Open Watcom hello world (tests/win32/W.C, with the C runtime)
prints hello under zrdxdpmi.exe and bound to stub-pmx.exe;
tests/win32/TAPI.C checks every export (about 90 checks), T1
and T2 are smaller probes.

Decisions found on the way (problem, choice):

- Entry state. The DOS extender convention ES = PSP selector
  crashed every string instruction of the C runtime. The PE
  start-up now sets ES = DS (Starter, PEMAX only), as Win32 does.
- INT 2Fh 168Ah returns the entry selector in ES. The ZRDXAPI
  wrappers (H and INC) now save and restore ES.
- DllMain is stdcall in Win32. The loader called it through a
  trampoline that assumed cdecl. The trampoline is now a far jump
  to PEDllRet, which reloads ESP; both conventions work.
- Program information. The vendor entry got function 6
  (ProgramInfo: main program path, image base, module handle)
  and 7 (Exit: detach all DLLs, then end the program). KERNEL32
  cannot free its own image while it runs, so ExitProcess asks
  the extender to do the detach and the DOS exit.
- HMODULE: the main program is its image base (GetModuleHandle
  (NULL) points to the MZ header); other modules are the ZRDX
  module handles. DllMain gets the module handle as hinstDLL.
- A name without extension in LoadLibrary/GetModuleHandle gets
  ".DLL", as the API does.
- Handles: file handle = 4 * (DOS handle + 1), kernel objects
  (events) 10000h and up; ReadFile/WriteFile work in pieces of
  4 KB over INT 21h.
- VirtualAlloc: DPMI blocks, zero filled, tracked in chunks of
  records, freed at detach; addresses outside our blocks are
  reported as committed read/write memory (stack, images).
- Code pages: GetACP is 1252, GetOEMCP is the DOS code page; all
  single byte pages map bytes to the same code point (Latin-1),
  UTF-8 is decoded. A real mapping table is a stage 2 item.
- GetModuleFileName: the path as the loader got it, completed by
  the DOS true name when DOS can do it; modules other than the
  main program are not supported yet.
- Build: wlink drops the relocation table of a DLL with no fixup
  (the loader cannot place it): W32_ANCHOR. A function called
  DllMain makes Watcom reference __DLLstart_; the entry point is
  called W32Main. Errors of wcc386 go to stderr: BUILD.BAT uses
  -fr= files and reports a missing DLL. -dW32TRACE builds a DLL
  that prints each function name (W32T) for debugging.

## Environment, time, process (task 185)

KERNEL32.DLL has 58 exports now (K32ENV.C, K32TIME.C, K32PROC.C
added); tests/win32/TPROC.C checks each new one.

- Environment: the DOS block (PSP:2Ch) is copied at first use
  into one block "A=B\0C=D\0\0" of any size (limit 64 KB); every
  Set builds a new block. GetEnvironmentStrings returns a copy
  (VirtualAlloc), Free releases it. The W functions narrow to
  the low byte of each character.
- Time: DOS date and time (centiseconds -> milliseconds); system
  time = local time (no zone). FILETIME conversions use 16-bit
  limb arithmetic, because the 64-bit helpers of a C runtime are
  not linked. GetTickCount counts BIOS ticks (55 ms steps) with
  midnight wraps; QueryPerformanceFrequency is 1193182, the
  counter is ticks * 65536 (the PIT is not read: ring 3 port
  access is not promised); Sleep waits on the tick count and
  yields with INT 2Fh 1680h.
- Process: GetCurrentProcess is -1; GetCurrentProcessId is the
  PSP selector; TerminateProcess ends only the own process.
  GlobalMemoryStatus reports the DPMI largest free block in all
  fields.

## Heap, memory, TLS, sync (task 186)

KERNEL32.DLL has 96 exports (K32HEAP.C, K32TLS.C, events in
K32OBJ.C); tests/win32/TMEM.C checks each new one.

- Heaps: a heap is a list of segments (DPMI blocks, at least
  64 KB); blocks have a 16 byte header (size, used or free
  mark) and are split and merged when needed. The heap handle
  is the address of the heap record. Everything is released at
  detach. HeapReAlloc grows in place when the next block is free.
- Global and Local memory: fixed blocks of the process heap
  (GMEM_MOVEABLE gives a fixed block, the handle is the address;
  GlobalLock returns it). Local* are the same functions under
  another export name.
- TLS: 1088 slots in one block, one thread. Critical sections
  and Interlocked functions are the plain single thread forms.
- Events: SetEvent, ResetEvent and WaitForSingleObject on events.
  A wait that no other thread could end returns WAIT_TIMEOUT
  after the time-out; with INFINITE it fails at once with
  WAIT_FAILED (error 1359) instead of hanging.

## Files, directories, search (task 187)

KERNEL32.DLL has 116 exports (K32FS.C, K32FIND.C); TFS.C checks
each new one, long names included (DOSBox-X with lfn on).

- Every name call tries the LFN function (71xxh) and falls back
  to the plain DOS function when the LFN API answers 7100h
  (w32_dos2); FindFirstFile does the same with 714Eh and 4Eh.
- The LFN find record has the layout of WIN32_FIND_DATAA (318
  bytes), so the DOS answer is the Win32 structure. A find handle
  is a pointer to a record (magic, LFN flag, DOS handle, DTA copy)
  allocated on the process heap; FindClose frees it.
- DOS errors map to the same Win32 numbers (2, 3, 5, 6, 18 ...),
  except: CreateDirectory on an existing name gives 183, MoveFile
  onto an existing name gives 183.
- File times are DOS times (2 seconds). DOS stores the time set
  with SetFileTime when the handle is closed, so a reader sees it
  after the close. The creation time is the write time and the
  access time has the date only (the plain DOS has no more).
- Sizes are 32 bit: the high word of a size or of a distance must
  be the sign extension, else ERROR_INVALID_PARAMETER.
- GetCurrentDirectory returns "X:\dir" (the drive from INT 21h
  19h); SetCurrentDirectory also changes the drive for "X:..".

## Console and text (task 188)

KERNEL32.DLL has 149 exports and USER32.DLL 37 (K32CP.C, K32TXT.C,
K32CON.C added; TTXT.C checks each new one).

- Code pages: the upper halves of 14 single byte pages (437, 850,
  852, 858, 866, 1250-1258) are generated into K32CP.C by GENCP.PY
  from the Unicode Consortium mappings (source: the codec tables
  of the Python library; bytes the mapping leaves undefined map to
  the code point of the same value). 28591 is Latin-1, 65001 is
  UTF-8. CP_ACP is 1252, CP_OEMCP the DOS code page.
- WideCharToMultiByte: a character with no mapping becomes the
  default character ('?') and sets *used; a surrogate pair counts
  as one character in a single byte page (one '?').
- Case mapping (w32_upper/w32_lower): ASCII, Latin-1, Latin
  Extended-A, Greek and Cyrillic. Character classes (GetStringType
  CT_CTYPE1) use the same rules. CompareString compares code
  points after folding (no locale collation). USER32 imports its
  mapping from KERNEL32 (linked with `system nt_dll`, which gives
  the import table of a real Win32 DLL).
- Console: a console handle is a DOS handle of a character device;
  GetConsoleMode on a redirected handle fails with 6, as a program
  expects when it tests for redirection. The screen size and the
  cursor come from the BIOS data area (0040:004A, 0050, 0084).
  Console modes and attributes are stored, not applied; the code
  pages of the console are stored.
- INT 21h AH=60h (true name) fails in this environment; the LFN
  form 7160h works, so file name calls try it first. The true name
  call may write 260 characters: buffers need 270 bytes (a 132
  byte buffer corrupted the stack of GetModuleFileName).
