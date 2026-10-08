; DPMI 0400h (get version) from the program
%include "t.inc"
bits 32
global start
TNAME 'T19 dpmi version'
section .text
start:  BEGIN
        mov eax, 400h
        int 31h
        jc bad
        cmp ah, 0
        jne bad
        PASS
bad:    FAIL
