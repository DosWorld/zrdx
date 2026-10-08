; entry point is "start", not the first byte of the code
%include "t.inc"
bits 32
global start
TNAME 'T09 entry in the middle'
section .text
skip:   FAIL
        nop
        nop
start:  BEGIN
        PASS
