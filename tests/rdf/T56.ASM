; DPMI 0501h/0503h: zero size and sizes that overflow after rounding to pages must fail
%include "t.inc"
bits 32
global start
TNAME 'T56 dpmi bad sizes'
section .bss
h:      resd 1
section .text
start:  BEGIN
        mov eax, 501h
        xor ebx, ebx
        xor ecx, ecx
        int 31h
        jnc near bad
        mov eax, 501h
        mov ebx, 0FFFFh
        mov ecx, 0FFFFh
        int 31h
        jnc near bad
        mov eax, 501h
        mov ebx, 0FFFFh
        mov ecx, 0F001h
        int 31h
        jnc near bad
        mov eax, 501h
        mov ebx, 0
        mov ecx, 100h
        int 31h
        jc near bad
        shl esi, 16
        mov si, di
        mov [h], esi
        mov esi, [h]
        mov edi, esi
        shr esi, 16
        mov eax, 503h
        mov ebx, 0FFFFh
        mov ecx, 0FFFFh
        int 31h
        jnc near bad
        mov esi, [h]
        mov edi, esi
        shr esi, 16
        mov eax, 503h
        xor ebx, ebx
        xor ecx, ecx
        int 31h
        jnc near bad
        mov esi, [h]
        mov edi, esi
        shr esi, 16
        mov eax, 502h
        int 31h
        jc near bad
        PASS
bad:    FAIL
