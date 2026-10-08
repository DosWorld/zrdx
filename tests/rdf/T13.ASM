; EDX = linear address of the PSP, command tail is read from it (run with: T13 ABC)
%include "t.inc"
bits 32
global start
TNAME 'T13 psp command tail'
section .text
start:  mov esi, edx
        BEGIN
        cmp word [esi], 20CDh
        jne bad
        cmp byte [esi + 80h], 4
        jne bad
        cmp dword [esi + 81h], 43424120h
        jne bad
        cmp byte [esi + 85h], 13
        jne bad
        PASS
bad:    FAIL
