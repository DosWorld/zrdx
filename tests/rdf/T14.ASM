; empty command tail (run without arguments)
%include "t.inc"
bits 32
global start
TNAME 'T14 empty command tail'
section .text
start:  mov esi, edx
        BEGIN
        cmp byte [esi + 80h], 0
        jne bad
        cmp byte [esi + 81h], 13
        jne bad
        PASS
bad:    FAIL
