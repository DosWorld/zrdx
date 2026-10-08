; many small sections of code and data, alignment
%include "t.inc"
bits 32
global start
TNAME 'T33 alignment'
section .data
        align 16
a1:     dd 1
        align 16
a2:     dd 2
        align 16
a3:     dd 3
section .bss
        alignb 16
b1:     resd 1
        alignb 16
b2:     resd 1
section .text
        align 16
start:  BEGIN
        mov eax, a2
        sub eax, a1
        cmp eax, 16
        jne bad
        mov eax, a3
        sub eax, a2
        cmp eax, 16
        jne bad
        mov eax, b2
        sub eax, b1
        cmp eax, 16
        jne bad
        PASS
bad:    FAIL
