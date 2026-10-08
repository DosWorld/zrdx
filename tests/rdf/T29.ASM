; write 40000 bytes to NUL in one DOS call (bigger than the transfer buffer)
%include "t.inc"
bits 32
global start
TNAME 'T29 big write'
LEN     equ 40000
section .data
nul:    db 'NUL', 0
blk:    times LEN db 'x'
section .text
start:  BEGIN
        mov edx, nul
        mov eax, 3D01h
        int 21h
        jc bad
        mov ebx, eax
        mov edx, blk
        mov ecx, LEN
        mov ah, 40h
        int 21h
        jc bad
        cmp eax, LEN
        jne bad
        mov ah, 3Eh
        int 21h
        PASS
bad:    FAIL
