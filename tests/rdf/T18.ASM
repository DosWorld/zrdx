; DOS file I/O: create, write, close, open, read, compare, delete
%include "t.inc"
bits 32
global start
TNAME 'T18 file io'
section .data
fname:  db 'T18.TMP', 0
text:   db 'hello, zrdx'
TLEN    equ $ - text
section .bss
buf:    resb 64
section .text
start:  BEGIN
        push ds
        pop es
        mov edx, fname
        xor ecx, ecx
        mov ah, 3Ch
        int 21h
        jc bad
        mov ebx, eax
        mov edx, text
        mov ecx, TLEN
        mov ah, 40h
        int 21h
        jc bad
        cmp eax, TLEN
        jne bad
        mov ah, 3Eh
        int 21h
        mov edx, fname
        mov eax, 3D00h
        int 21h
        jc bad
        mov ebx, eax
        mov edx, buf
        mov ecx, 64
        mov ah, 3Fh
        int 21h
        jc bad
        cmp eax, TLEN
        jne bad
        mov ah, 3Eh
        int 21h
        mov esi, buf
        mov edi, text
        mov ecx, TLEN
        repe cmpsb
        jne bad
        mov edx, fname
        mov ah, 41h
        int 21h
        jc bad
        PASS
bad:    FAIL
