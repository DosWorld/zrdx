; string in the data section, address fixed up by a relocation
%include "t.inc"
bits 32
global start
TNAME 'T03 data string'
section .text
start:  BEGIN
        PASS
