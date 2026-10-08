; BIOS int 10h teletype output from protected mode
%include "t.inc"
bits 32
global start
TNAME 'T22 int 10h'
section .text
start:  BEGIN
        mov ah, 0Eh
        mov al, '*'
        mov bx, 7
        int 10h
        mov ah, 0Eh
        mov al, 8
        int 10h
        PASS
