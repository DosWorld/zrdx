; Runtime loading scenario (task 012), imports the EXE wrappers
; LoadLibrary, GetProcAddress, GetModuleHandle, FreeLibrary and
; GetLastError (IAT slots 0..4). PlugRun returns 0 or a step.
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
plugrun:
        push ebx
        push esi
        mov ebx, 1
        CALL1 0, nExtra
        mov esi, eax
        or esi, esi
        jz fail
        push nExtraFn
        push esi
        call [IATVA + 4]
        add esp, 8
        mov ebx, 2
        or eax, eax
        jz fail
        call eax
        mov ebx, 3
        cmp eax, 99
        jne fail
        CALL1 0, nExtra
        mov ebx, 4
        cmp eax, esi
        jne fail
        CALL1 2, nExtra
        mov ebx, 5
        cmp eax, esi
        jne fail
        push esi
        call [IATVA + 12]
        add esp, 4
        mov ebx, 6
        or eax, eax
        jnz fail
        CALL1 0, nNo
        mov ebx, 7
        or eax, eax
        jnz fail
        call [IATVA + 16]
        mov ebx, 8
        cmp eax, 2
        jne fail
        CALL1 0, nBad
        mov ebx, 9
        or eax, eax
        jnz fail
        call [IATVA + 16]
        mov ebx, 10
        cmp eax, 13
        jne fail
        CALL1 2, nBad
        mov ebx, 11
        or eax, eax
        jnz fail
        call [IATVA + 16]
        mov ebx, 12
        cmp eax, 5
        jne fail
        CALL1 2, nMain
        mov ebx, 13
        or eax, eax
        jz fail
        push eax
        call [IATVA + 12]
        add esp, 4
        mov ebx, 14
        cmp eax, 8
        jne fail
        CALL1 0, nReent
        mov ebx, 15
        or eax, eax
        jz fail
        xor eax, eax
        jmp done
fail:   mov eax, ebx
done:   pop esi
        pop ebx
        ret
nExtra: db 'EXTRA.DLL', 0
nExtraFn: db 'Extra', 0
nNo: db 'NOEXIST.DLL', 0
nBad: db 'EXTRAF.DLL', 0
nMain: db 'RT.EXE', 0
nReent: db 'REENT.DLL', 0
