; 40000 byte file: written and read back with single DOS calls (larger than the transfer buffer)
%include "t.inc"
bits 32
global start
TNAME 'T30 big file'
LEN     equ 40000
section .data
fname:  db 'T30.TMP', 0
section .bss
wb:     resb LEN
rb:     resb LEN
section .text
start:  BEGIN
        push ds
        pop es
        mov ecx, LEN
        mov edi, wb
        xor eax, eax
.f:     mov [edi], al
        inc edi
        add eax, 7
        loop .f
        mov edx, fname
        xor ecx, ecx
        mov ah, 3Ch
        int 21h
        jc bad
        mov ebx, eax
        mov edx, wb
        mov ecx, LEN
        mov ah, 40h
        int 21h
        jc bad
        cmp eax, LEN
        jne bad
        mov ah, 3Eh
        int 21h
        mov edx, fname
        mov eax, 3D00h
        int 21h
        jc bad
        mov ebx, eax
        mov edx, rb
        mov ecx, LEN
        mov ah, 3Fh
        int 21h
        jc bad
        cmp eax, LEN
        jne bad
        mov ah, 3Eh
        int 21h
        mov esi, wb
        mov edi, rb
        mov ecx, LEN
        repe cmpsb
        jne bad
        mov edx, fname
        mov ah, 41h
        int 21h
        PASS
bad:    FAIL
