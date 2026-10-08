; 70000 bytes of initialised data (more than 64K), checksum
%include "t.inc"
bits 32
global start
TNAME 'T16 big data'
section .data
blk:    times 70000 db 5
section .text
start:  BEGIN
        mov esi, blk
        mov ecx, 70000
        xor ebx, ebx
.l:     movzx eax, byte [esi]
        add ebx, eax
        inc esi
        loop .l
        cmp ebx, 350000
        jne bad
        PASS
bad:    FAIL
