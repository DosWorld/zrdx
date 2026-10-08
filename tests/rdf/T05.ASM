; initialised data items, read and modify
%include "t.inc"
bits 32
global start
TNAME 'T05 data'
section .data
d1:     dd 11111111h
d2:     dw 2222h
d3:     db 33h
d4:     db 'abc', 0
section .text
start:  BEGIN
        cmp dword [d1], 11111111h
        jne bad
        cmp word [d2], 2222h
        jne bad
        cmp byte [d3], 33h
        jne bad
        cmp dword [d4], 'abc'
        jne bad
        add dword [d1], 1
        mov word [d2], 0BEEFh
        mov byte [d3], 0FFh
        cmp dword [d1], 11111112h
        jne bad
        cmp word [d2], 0BEEFh
        jne bad
        cmp byte [d3], 0FFh
        jne bad
        PASS
bad:    FAIL
