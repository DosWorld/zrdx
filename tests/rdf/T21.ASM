; DOS services: version, date, time
%include "t.inc"
bits 32
global start
TNAME 'T21 dos services'
section .text
start:  BEGIN
        mov ah, 30h
        int 21h
        cmp al, 3
        jb bad
        mov ah, 2Ah
        int 21h
        cmp cx, 1980
        jb bad
        cmp dh, 1
        jb bad
        cmp dh, 12
        ja bad
        mov ah, 2Ch
        int 21h
        cmp ch, 24
        jae bad
        cmp cl, 60
        jae bad
        PASS
bad:    FAIL
