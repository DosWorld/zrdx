; 16-bit relocation: low word of the address
%include "t.inc"
bits 32
global start
TNAME 'T26 word reloc'
section .data
tgt:    db 0
w16:    dw tgt
section .text
start:  BEGIN
        mov ax, [w16]
        mov ebx, tgt
        cmp ax, bx
        jne bad
        PASS
bad:    FAIL
