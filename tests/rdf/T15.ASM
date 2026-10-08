; 1 MB of BSS: zeroed, first and last bytes writable
%include "t.inc"
bits 32
global start
TNAME 'T15 big bss'
section .bss
big:    resb 100000h
section .text
start:  BEGIN
        push ds
        pop es
        mov edi, big
        mov ecx, 100000h / 4
        xor eax, eax
        repe scasd
        jne bad
        mov byte [big], 1
        mov byte [big + 0FFFFFh], 2
        cmp byte [big], 1
        jne bad
        cmp byte [big + 0FFFFFh], 2
        jne bad
        PASS
bad:    FAIL
