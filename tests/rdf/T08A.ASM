; two modules: code, data and bss shared between them
%include "t.inc"
bits 32
global start
extern addtwo, shared_d, shared_b
TNAME 'T08 two modules'
section .text
start:  BEGIN
        mov eax, 5
        mov ebx, 6
        call addtwo
        cmp eax, 11
        jne bad
        cmp dword [shared_d], 0CAFEh
        jne bad
        mov dword [shared_b], 99
        call addtwo
        cmp dword [shared_b], 99
        jne bad
        PASS
bad:    FAIL
