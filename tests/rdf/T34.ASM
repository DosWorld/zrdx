; clipboard API (INT 2Fh, AX=17xxh): text round trip. Needs a clipboard driver (clipdrv), otherwise skipped
%include "t.inc"
bits 32
global start
TNAME 'T34 clipboard text'
section .data
txt:    db 'Hello, clipboard!'
TLEN    equ $ - txt
skip:   db 'skipped (no clipboard driver)', 13, 10, '$'
section .bss
buf:    resb 64
section .text
start:  BEGIN
        push ds
        pop es
        mov eax, 1700h
        int 2Fh
        cmp ax, 1700h
        je near .skip
        mov eax, 1701h
        int 2Fh
        or ax, ax
        jz near bad
        mov eax, 1702h
        int 2Fh
        or ax, ax
        jz near bad
        mov eax, 1703h
        mov edx, 1
        mov ebx, txt
        xor esi, esi
        mov ecx, TLEN
        int 2Fh
        or ax, ax
        jz near bad
        mov eax, 1704h
        mov edx, 1
        int 2Fh
        cmp dx, 0
        jne near bad
        cmp ax, TLEN
        jne near bad
        mov eax, 1705h
        mov edx, 1
        mov ebx, buf
        int 2Fh
        or ax, ax
        jz near bad
        mov esi, buf
        mov edi, txt
        mov ecx, TLEN
        repe cmpsb
        jne near bad
        cmp byte [buf + TLEN], 0
        jne near bad
        mov eax, 1708h
        int 2Fh
        or ax, ax
        jz near bad
        PASS
.skip:  PRINT skip
        EXITC 0
bad:    FAIL
