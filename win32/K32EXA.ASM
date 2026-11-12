; This file is part of the ZRDX OSE project
; (C) 2026, Viacheslav Komenda
;
; KERNEL32.DLL: DPMI exception stubs and context resume (task
; 205). A stub runs on the host stack: it builds a CONTEXT and
; the arguments of w32_dispatch on the faulting stack, changes
; the exception frame to enter w32_dispatch there and returns,
; like KiUserExceptionDispatcher.

bits 32

CTXSZ equ 2CCh
RECSZ equ 80

segment _TEXT use32 class=CODE

global _w32_excstubs, _w32_resume
extern _w32_dispatch

; stub n is at _w32_excstubs + 16 * n
%macro STUB 1
        align 16
        push dword %1
        jmp excom
%endmacro

_w32_excstubs:
%assign n 0
%rep 18
        STUB n
%assign n n + 1
%endrep

; (the x87 state is saved with FNSAVE and restored by resume)
; copies the saved dword [ebp + %1] to the CONTEXT field %2
%macro SAVE 2
        mov eax, [ebp + %1]
        mov [ebx + %2], eax
%endmacro

; frame after pushad: edi esi ebp esp ebx edx ecx eax (0..28),
; n 32, return eip/cs 36/40, error code 44, eip 48, cs 52,
; eflags 56, esp 60, ss 64
excom:
        pushad
        mov ebp, esp
        mov ebx, [ebp + 60]
        sub ebx, 16 + CTXSZ
        mov edi, ebx
        mov ecx, CTXSZ / 4
        xor eax, eax
        cld
        rep stosd
        mov dword [ebx], 1000Fh
        fnsave [ebx + 1Ch]              ;FloatSave: 108 bytes
        cmp dword [ebp + 32], 16
        je .fpclean                     ;pending x87 error: init
        frstor [ebx + 1Ch]
.fpclean:
        mov ax, gs
        mov [ebx + 8Ch], eax
        mov ax, fs
        mov [ebx + 90h], eax
        mov ax, es
        mov [ebx + 94h], eax
        mov ax, ds
        mov [ebx + 98h], eax
        SAVE 0, 9Ch
        SAVE 4, 0A0h
        SAVE 16, 0A4h
        SAVE 20, 0A8h
        SAVE 24, 0ACh
        SAVE 28, 0B0h
        SAVE 8, 0B4h
        SAVE 48, 0B8h
        SAVE 52, 0BCh
        SAVE 56, 0C0h
        SAVE 60, 0C4h
        SAVE 64, 0C8h
        lea edx, [ebx - RECSZ]
        lea ecx, [edx - 16]
        mov dword [ecx], 0
        mov [ecx + 4], edx
        mov [ecx + 8], ebx
        mov eax, [ebp + 32]
        mov [ecx + 12], eax
        mov [ebp + 60], ecx
        mov dword [ebp + 48], _w32_dispatch
        and dword [ebp + 56], ~(100h | 400h | 4000h)
        popad
        add esp, 4
        retf

; void __cdecl w32_resume(CONTEXT *ctx): continues at ctx->Eip
; with the registers of ctx. Everything is copied above the new
; ESP first: the CONTEXT lies below it and an IRQ may reuse it.
_w32_resume:
        mov eax, [esp + 4]
        mov edx, [eax + 0C4h]
        sub edx, 16
        mov ecx, [eax + 0A8h]
        mov [edx], ecx
        mov ecx, [eax + 0B0h]
        mov [edx + 4], ecx
        mov ecx, [eax + 0C0h]
        mov [edx + 8], ecx
        mov ecx, [eax + 0B8h]
        mov [edx + 12], ecx
        frstor [eax + 1Ch]
        mov edi, [eax + 9Ch]
        mov esi, [eax + 0A0h]
        mov ebx, [eax + 0A4h]
        mov ebp, [eax + 0B4h]
        mov ecx, [eax + 0ACh]
        mov esp, edx
        pop edx
        pop eax
        popfd
        ret
