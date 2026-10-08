; clipboard API: 40001 bytes (larger than the transfer buffer), set and get back
%include "t.inc"
bits 32
global start
TNAME 'T35 clipboard big'
LEN     equ 40001
section .data
skip:   db 'skipped (no clipboard driver)', 13, 10, '$'
section .bss
wb:     resb LEN
rb:     resb LEN + 16
section .text
start:  BEGIN
        push ds
        pop es
        mov eax, 1700h
        int 2Fh
        cmp ax, 1700h
        je near .skip
        mov ecx, LEN
        mov edi, wb
        xor eax, eax
.f:     mov [edi], al
        inc edi
        add eax, 13
        loop .f
        mov eax, 1702h
        int 2Fh
        mov eax, 1703h
        mov edx, 1
        mov ebx, wb
        xor esi, esi
        mov ecx, LEN
        int 2Fh
        or ax, ax
        jz near bad
        mov eax, 1704h
        mov edx, 1
        int 2Fh
        cmp dx, 0
        jne near bad
        cmp ax, LEN
        jne near bad
        mov eax, 1705h
        mov edx, 1
        mov ebx, rb
        int 2Fh
        or ax, ax
        jz near bad
        mov esi, wb
        mov edi, rb
        mov ecx, LEN
        repe cmpsb
        jne near bad
        mov ecx, 16
        mov edi, rb + LEN
        xor eax, eax
        repe scasb
        jne near bad
        PASS
.skip:  PRINT skip
        EXITC 0
bad:    FAIL
