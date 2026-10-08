; bubble sort of 200 dwords in bss
%include "t.inc"
bits 32
global start
TNAME 'T28 sort'
N       equ 200
section .bss
arr:    resd N
section .text
start:  BEGIN
        mov ecx, N
        mov eax, 12345
        mov edi, arr
.f:     imul eax, 1103515245
        add eax, 12345
        mov edx, eax
        shr edx, 8
        mov [edi], edx
        add edi, 4
        loop .f
        mov ecx, N - 1
.o:     push ecx
        mov esi, arr
.i:     mov eax, [esi]
        cmp eax, [esi + 4]
        jbe .n
        xchg eax, [esi + 4]
        mov [esi], eax
.n:     add esi, 4
        loop .i
        pop ecx
        loop .o
        mov esi, arr
        mov ecx, N - 1
.c:     mov eax, [esi]
        cmp eax, [esi + 4]
        ja bad
        add esi, 4
        loop .c
        PASS
bad:    FAIL
