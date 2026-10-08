; BSS is zeroed and writable
%include "t.inc"
bits 32
global start
TNAME 'T04 bss'
section .bss
b1:     resd 1
b2:     resb 4096
section .text
start:  BEGIN
        cmp dword [b1], 0
        jne bad
        mov esi, b2
        mov ecx, 4096
.l:     cmp byte [esi + ecx - 1], 0
        jne bad
        loop .l
        mov dword [b1], 12345678h
        cmp dword [b1], 12345678h
        jne bad
        mov byte [b2 + 4095], 77h
        cmp byte [b2 + 4095], 77h
        jne bad
        PASS
bad:    FAIL
