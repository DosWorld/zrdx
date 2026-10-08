; relocations with addends
%include "t.inc"
bits 32
global start
TNAME 'T25 reloc addends'
section .data
sx:    db 'ABCDEFGH'
p1:     dd sx + 3
p2:     dd sx + 7
p3:     dd bv + 8
section .bss
bv:     resd 4
section .text
start:  BEGIN
        mov esi, [p1]
        cmp byte [esi], 'D'
        jne bad
        mov esi, [p2]
        cmp byte [esi], 'H'
        jne bad
        mov edi, [p3]
        mov dword [edi], 1234h
        cmp dword [bv + 8], 1234h
        jne bad
        mov esi, sx + 1
        cmp byte [esi], 'B'
        jne bad
        PASS
bad:    FAIL
