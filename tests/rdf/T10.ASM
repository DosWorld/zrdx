; no "start" symbol: execution begins at the first byte of the code
%include "t.inc"
bits 32
TNAME 'T10 no start symbol'
global first
section .text
first:  BEGIN
        PASS
