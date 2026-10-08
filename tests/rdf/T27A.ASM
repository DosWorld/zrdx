; three modules: a -> b -> c, shared bss counter
%include "t.inc"
bits 32
global start
extern fb, counter
TNAME 'T27 three modules'
section .text
start:  BEGIN
        call fb
        call fb
        cmp dword [counter], 4
        jne bad
        PASS
bad:    FAIL
