; Emit the actual native stack addresses for KSTACK.ASM's build parameters.
%ifndef BOUND
%define NativeHost 1
%endif
%include "autolbl.inc"
%include "prot.inc"
%include "gmacros.asm"
%include "segdefs.asm"
%include "layout.inc"
; Reuse the production trap list to expose offsets without emitting gates.
%unmacro DefTrap3 3
%assign ProbeTrap 0
%macro DefTrap3 3
Off %+ %1 %+ Trap3 equ ProbeTrap * 4
%assign ProbeTrap ProbeTrap + %2
%endmacro
%include "traps.inc"
SEGM Data
dd OffKernelStackStart, OffKernelStack1Start
dd OffIret2KernelAsincTrap3
dd OffPageDir, OffPage2, OffLastInit, OffMCBVector, OffClientPages
ESEG Data
