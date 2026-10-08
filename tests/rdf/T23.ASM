; selectors: DS = SS, code is readable through CS and sees the same flat memory
%include "t.inc"
bits 32
global start
TNAME 'T23 selectors'
section .data
v:      dd 0DEADBEEFh
section .text
start:  BEGIN
        mov ax, ds
        mov bx, ss
        cmp ax, bx
        jne bad
        mov ax, cs
        cmp ax, bx
        je bad
        mov eax, [cs:v]
        cmp eax, 0DEADBEEFh
        jne bad
        mov dword [v], 12345h
        mov eax, [cs:v]
        cmp eax, 12345h
        jne bad
        PASS
bad:    FAIL
