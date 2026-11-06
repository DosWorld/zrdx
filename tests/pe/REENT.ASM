; Reentrancy fixture (task 012): DllMain calls LoadLibrary (IAT
; slot 0) and GetLastError (slot 1); prints R if it was refused
; with error 11, N otherwise.
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
IATVA equ (BASE - 1000h) + 2400h
        mov eax, [esp + 8]
        cmp eax, 1
        jne ok
        push nX
        call [IATVA]
        add esp, 4
        or eax, eax
        jnz bad
        call [IATVA + 4]
        cmp eax, 11
        jne bad
        mov dl, 'R'
        jmp show
bad:    mov dl, 'N'
show:   mov ah, 2
        int 21h
ok:     mov eax, 1
        ret
nX: db 'X.DLL', 0
