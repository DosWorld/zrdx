; start registers: EAX = EBX = code base
%include "t.inc"
bits 32
global start
TNAME 'T12 start registers'
section .text
start:  cmp eax, ebx
        jne bad
        mov esi, ebx
        BEGIN
        call .n
.n:     pop eax
        sub eax, .n - $$
        cmp eax, esi
        jne bad
        PASS
bad:    FAIL
