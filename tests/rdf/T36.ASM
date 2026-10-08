; clipboard API: bitmap header pointer is converted between protected and real mode
%include "t.inc"
bits 32
global start
TNAME 'T36 clipboard bitmap'
section .data
bm:     dw 0, 8, 2, 1
        db 1, 1
        dd 0
        dw 20, 10
        db 0AAh, 55h
BLEN    equ $ - bm
skip:   db 'skipped (no clipboard driver)', 13, 10, '$'
section .bss
rb:     resb 64
section .text
start:  BEGIN
        push ds
        pop es
        mov eax, 1700h
        int 2Fh
        cmp ax, 1700h
        je near .skip
        mov eax, 1702h
        int 2Fh
        mov eax, 1703h
        mov edx, 2
        mov ebx, bm
        xor esi, esi
        mov ecx, BLEN
        int 2Fh
        or ax, ax
        jz near bad
        mov eax, 1704h
        mov edx, 2
        int 2Fh
        cmp ax, BLEN
        jne near bad
        mov eax, 1705h
        mov edx, 2
        mov ebx, rb
        int 2Fh
        or ax, ax
        jz near bad
        mov eax, rb + 12h
        cmp [rb + 0Ah], ax
        jne near bad
        mov ax, es
        cmp [rb + 0Ch], ax
        jne near bad
        cmp dword [rb], 80000h
        jne near bad
        cmp word [rb + 4], 2
        jne near bad
        cmp byte [rb + 12h], 0AAh
        jne near bad
        cmp byte [rb + 13h], 55h
        jne near bad
        cmp word [rb + 0Eh], 20
        jne near bad
        PASS
.skip:  PRINT skip
        EXITC 0
bad:    FAIL
