; deep recursion on the program stack (7000 levels)
%include "t.inc"
bits 32
global start
TNAME 'T17 deep recursion'
section .text
sum:    or eax, eax
        jz .z
        push eax
        dec eax
        call sum
        pop ecx
        add eax, ecx
        ret
.z:     ret
start:  BEGIN
        mov eax, 7000
        call sum
        cmp eax, 7000 * 7001 / 2
        jne bad
        PASS
bad:    FAIL
