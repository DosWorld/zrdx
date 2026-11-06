; Unload scenario (task 013). IAT: 0 LoadLibrary, 1 GetModuleHandle,
; 2 FreeLibrary (imported from RF.EXE). PlugRun returns 0 or a step.
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
IATVA equ (BASE - 1000h) + 2400h
        mov eax, 1
        ret
        align 16
%macro CALL1 2
        push %2
        call [IATVA + 4 * %1]
        add esp, 4
%endmacro
%macro FREE 0
        push eax
        call [IATVA + 8]
        add esp, 4
%endmacro
plugrun:
        push ebx
        push esi
        push edi
        mov ebx, 1
        CALL1 0, nExtra
        or eax, eax
        jz fail
        FREE
        or eax, eax
        jnz fail
        call meminfo
        mov eax, [mbuf]
        mov [m0], eax
        mov eax, [mbuf + 14h]
        mov [m1], eax
        mov esi, 20
again:  mov ebx, 2
        CALL1 0, nExtra
        or eax, eax
        jz fail
        FREE
        or eax, eax
        jnz fail
        dec esi
        jnz again
        call meminfo
        mov ebx, 3
        mov eax, [mbuf]
        cmp eax, [m0]
        jne fail
        mov eax, [mbuf + 14h]
        cmp eax, [m1]
        jne fail
        mov ebx, 4
        CALL1 0, nIA
        or eax, eax
        jz fail
        FREE
        or eax, eax
        jnz fail
        mov ebx, 5
        CALL1 1, nIB
        or eax, eax
        jnz fail
        mov ebx, 6
        CALL1 1, nIC
        or eax, eax
        jnz fail
        mov ebx, 7
        CALL1 0, nIB
        mov esi, eax
        or eax, eax
        jz fail
        mov ebx, 8
        CALL1 0, nIA
        or eax, eax
        jz fail
        FREE
        or eax, eax
        jnz fail
        mov ebx, 9
        CALL1 1, nIB
        or eax, eax
        jz fail
        mov ebx, 10
        mov eax, esi
        FREE
        or eax, eax
        jnz fail
        mov ebx, 11
        CALL1 1, nIC
        or eax, eax
        jnz fail
        mov ebx, 12
        CALL1 0, nIP
        mov esi, eax
        or eax, eax
        jz fail
        FREE
        or eax, eax
        jnz fail
        mov ebx, 13
        CALL1 1, nIQ
        or eax, eax
        jnz fail
        mov ebx, 14
        mov eax, esi
        FREE
        or eax, eax
        jz fail
        mov ebx, 15
        CALL1 0, nExit
        or eax, eax
        jz fail
        xor eax, eax
        jmp done
fail:   mov eax, ebx
done:   pop edi
        pop esi
        pop ebx
        ret
meminfo:
        push ds
        pop es
        mov edi, mbuf
        mov ax, 500h
        int 31h
        ret
m0: dd 0
m1: dd 0
mbuf: times 12 dd 0
nExtra: db 'EXTRA.DLL', 0
nIA: db 'IA.DLL', 0
nIB: db 'IB.DLL', 0
nIC: db 'IC.DLL', 0
nIP: db 'IP.DLL', 0
nIQ: db 'IQ.DLL', 0
nExit: db 'EXITD.DLL', 0
