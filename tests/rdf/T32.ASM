; flags and string ops, 32-bit operations in a loop (arithmetic sanity)
%include "t.inc"
bits 32
global start
TNAME 'T32 cpu sanity'
section .bss
tmp:    resb 32
section .text
start:  BEGIN
        push ds
        pop es
        mov edi, tmp
        mov ecx, 32
        mov al, 0AAh
        rep stosb
        mov esi, tmp
        mov ecx, 8
        xor ebx, ebx
.l:     lodsd
        xor ebx, eax
        loop .l
        or ebx, ebx
        jnz bad
        mov eax, 0FFFFFFFFh
        mov edx, 0FFFFFFFFh
        mul edx
        cmp edx, 0FFFFFFFEh
        jne bad
        cmp eax, 1
        jne bad
        mov eax, -100
        cdq
        mov ecx, 7
        idiv ecx
        cmp eax, -14
        jne bad
        PASS
bad:    FAIL
