; the program returns with RET, the loader return stub ends the process
%include "t.inc"
bits 32
global start
TNAME 'T11 ret to loader'
section .text
start:  BEGIN
        PRINT t_ok
        xor eax, eax
        ret
