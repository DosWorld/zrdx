# Native DPMI 0.9 ABI

Review update 2026-10-18: tasks 106-110 are qualified and installed.
D09R6.MD records normative expectation auditing, bounded failure/nesting
stress, the stable build-identity repair and 29 bytes of native Text saved.
Current fix39-* gates pass 55 native and 17 bound API cases on each
RAW/XMS/VCPI backend, plus 99 legacy results and 11 tooling tests.
Matched five-path QEMU icount timing medians are unchanged. This package
adds no independent-environment or external-provider qualification.

Baseline: previously qualified runtime, installed with the matching bound
loaders. D09WORK.MD and D09RUNS.JSN identify exact passing evidence.
Packed-file task 094 is complete with the explicitly approved 14 KB ceiling.

## Shared tables and entry

The VM freezes its first successful client bitness. NativeMode is not set
by rejected entry attempts. 0.9 clients share one LDT/IDT; this does not
implement the separate contexts of DPMI 1.0. Native ownership bitmaps
preserve ancestors' resources during nested DOS EXEC and child cleanup.
NATMODE/NATM32 cover opposite-mode rejection and subsequent same-mode
entry with a real-mode parent. NEST16/NEST32 cover four leaking children.

Native 16-bit software vectors use a private read-only LDT code selector
and 16-bit IDT trap gates. CALL32 instructions in the table overlap at a
four-byte stride, like the original Trap3 scheme: the following slot
contains the current call's selector inside its ignored offset. There
are 257 slots, including the last instruction's operand bytes. INT 31h
uses the DPMI gate; others use the default-reflection gate. The separate
state stub ends with a word FAR return. Public software-vector changes
retain 16-bit gate width; CPU/kernel gates stay 32-bit.

Use an operand-size prefix for the 32-bit call gates. A word CALL to the
same gate works on QEMU but DOSBox normal truncates the kernel target to
16 bits. The explicit CALL32 is valid on 386 and both emulators. The
thunk selector is immutable and lives for the VM, not the first child.
Restoring its default vector also removes the corresponding RM passup
hook. No LDT capacity or image-size ceiling is increased.

ENTRY now tests PUSHF/CALL FAR through the saved INT 31h vector, with
word/dword frames according to client mode. Both variants also pass on
the independent HDPMI16/HDPMI32 hosts. Pointer width follows client mode;
stack offset width follows SS.B. A word stack's high ESP is undefined,
so tests using 32-bit local addressing normalize it after API returns.

## Private frames and saved state

NC_Frames is the already reserved private 4 KB page. NativeAllocFrame
uses 128-byte LIFO records shared by public-call and bridge frames.
NativeFrameNext is mirrored beside RMStack/PMStack in permanent RM data.
Native 0305h advertises 16 bytes; both RM and PM state routines copy that
size. Entry snapshots preserve the parent's pointer; cleanup restores
it with the parent's stack descriptor before freeing the child context.

Public 16-bit calls use a canonical 12-byte IRET at record offset 80.
The header retains the original SS/SP and six-byte input frame length.
030xh workspace fits below the canonical IRET. CFrSS=Data1 identifies a
private shadow; CFrCS stays RPL 3 for ownership accounting. Private host
calls keep their ring-1 frame. NativeFinishIret restores the original
SS/SP and releases the record for DPMI, RM reflection and bridge returns.
Copied 030xh parameters come from the original stack above its word
IRET frame, rather than from the private canonical record. Active-SS
validation also reads the original stack identity.

Each client context is 45056 bytes (11 pages): ownership metadata,
existing alignment padding, one 4096-byte locked user stack and one
4096-byte private frame pool. The padding holds the parent's suspended
512-byte CPL1 stack during nested DOS EXEC. Save it before the child
reuses kernel stacks; restore it from the separate real-mode exit stack.
IRQ16/32 specifically interrupt an active allocator before executing a
child; sequential NEST alone does not prove this continuation.

The permanent CPL0/CPL1 stacks share one supervisor-only page. A 4096-byte
owned root IRQ anchor permits interrupts between native clients without
exposing that kernel page. Context metadata/frame pages are supervisor-
only while active; publication/free pair the U/S transitions because the
physical free pool retains PTE flags. Unpublished entry failures skip
that paired restoration. Ordinary allocation loops gain no extra walk.

Bridge records retain the prior PMStack and, for 16-bit clients, the
previous real-stack alias base. These records survive recursive callback
entry; return restores both values. RAW16/32 exercise both raw-switch directions, FS/GS clearing, wrapping
word stack offsets, state save/restore, flags and a size canary. Explicit
state switching between different callback aliases is covered by NATL16: save
outer/inner states, restore each without an intervening host allocation,
check distinct real-stack markers and restore the inner continuation.
NATLOCK/NATL16 also check the 16-byte state size and output canary.

## Locked events and callbacks

The existing LockedStack GDT slot maps the current NC_Stack page, limit
4095, DPL 3 and B according to client mode. Configure it before setup can
admit IRQs and restore the parent before the child's private free.
NativePickLockedStack recognizes locked aliases, flat stack addresses
and original stacks behind canonical frames. Nested events continue
below the current SP. The final locked offset is truncated for a word
stack because high ESP is undefined after word IRET. With no active client, IRQs use the host's original
32-bit default path, not the exited client's word vector table.

Exception frames occupy 32 bytes for 32-bit clients and 28 bytes for
16-bit clients. The latter include the documented word frame plus high
EIP/EFLAGS/ESP carriers. The return gate copies 24 parameter bytes;
NativeExceptionReturn16 reconstructs the hardware IRET while preserving
handler registers. IRQs use 12/6-byte IRET trampolines before the same
context. Ring-1 IRQs retain the asynchronous kernel continuation and
receive a mode-correct trampoline. Exception entry clears IF/TF/DF/NT;
the saved client flags are restored on return. CPU defaults first use a
hooked protected interrupt vector; otherwise nonfatal defaults reflect
through real mode and unhandled fatal exceptions terminate the client.
Software INT still uses the original client stack. A separate native
IRET return gate propagates interrupt-handler flag edits; RETF exception
handlers edit their documented frame. Capture flags before disabling
IRQs and updating the canonical return frame. Return CS/SS are forced to
RPL3 and NT/VM are cleared. Internal copy/TLB/LLDT/VCPI gates have DPL1;
optional CR emulation allows only EAX CR0/CR2 reads and MP/EM/TS changes.

Callbacks and real-mode passups use the locked stack and preserve their
prior PM bridge. A per-client immutable 64 KB data alias supplies real
SS:SP as DS:SI for 16-bit callbacks. Nested entry saves/restores its base.
32-bit callbacks continue to receive flat DS:ESI. ES:(E)DI supplies the
register packet; FS/GS are zero. Packet pointers on 16-bit return ignore
high EDI. NATLOCK/NATL16 exercise actual timer IRQs, exceptions, nested
exceptions, recursive callbacks and INT 31h inside handlers/callbacks.

## Descriptor, memory and real-mode policy

0002h aliases and DOS allocation descriptors use separate immutable-class
bitmaps. An alias always spans 64 KB and survives DOS-block release.
Native DOS blocks use adjacent descriptor arrays. On this 32-bit host,
both word-client and dword-client heads span the complete block; only
followers are capped at 64 KB, with a partial final descriptor.
DOS MCB size supplies the full allocation extent for resize/free. Bound
32-bit clients retain a single full-block descriptor with class checks
and failed-resize rollback. Neither path accepts a segment alias as a
DOS block. 000Ah also accepts data selectors per the supplied correction
to DPMI 0.9; absent descriptors retain the specified relaxed type checks.

0500h returns the exact supported largest allocation and marks unavailable
nonpageable statistics FFFFFFFFh. It walks allocation gaps and crossed
page directories, charging only missing page tables and a needed MCB
page. It does not allocate probe blocks. Memory and physical mappings
share an existing boundary page table without replacing normal pages.

0300h-0302h preserve packet CS/IP/SS/SP and copy the documented registers.
BH bit 0 requests normal real-mode A20 on RAW/XMS; unflagged calls add no
A20 I/O. A single owned XMS local-enable reference is released/reacquired
across flagged calls, callbacks and termination. RAW retains its working
port-92/KBC method and leaves an already-enabled line alone; VCPI ignores
the flag. The prior policy fits an unused high byte of the saved CS slot.
A20X independently proxies the XMS entry to check paired calls and an
upstream reference; RAW A20R and XMS A20X pass final qualification.

## Qualification and budgets

D09SIZE.TSV is generated by tests/dpmi/SIZES.PY from measured listings and
MZ artifacts. The initialized section-byte totals and all fixed resource
limits are unchanged. Existing padding was redistributed: native Text
15456 bytes, ordinary Text 7840 bytes; see D09SIZE.MD. The native MZ file
is 19088 bytes, including a 16-byte relocation-header increase over the
previous candidate. Default packed bound stubs are 14336 bytes; unpacked forms remain 29184.

D09WORK.MD and generated D09RUNS.JSN distinguish current fix39 campaigns
from historical DOSBox native-core, HDPMI comparison and timing evidence.
Current QEMU campaigns cover actual #NM/private-stack #PF, A20 ownership,
callback-state aliases, nested/fatal cleanup and successful-call sentinels.
D09PERF.TSV compares three common calls and two software-INT paths with
identical clients under QEMU icount. All medians are unchanged after the
29-byte compaction; these virtual ticks are not hardware timings. No
emulator-specific production path was added.

Software INT frame stores now preflight complete segment bounds and
user-write access on every touched page. A failing second page cannot
leave the first page partially written. #SS/#PF is reflected through the
existing locked exception stack, with the original instruction pointer.
See D09R6.MD and STKEDGE.ASM for exact positive/negative coverage.
