; Import graph driver (task 010): HostVal export for reverse
; imports, calls the first imported function and checks EXPECT.
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
IATVA equ (BASE - 1000h) + 2400h
%ifndef EXPECT
%define EXPECT 8
%endif
        jmp start
        align 16
hostval:
        mov eax, 7
        ret
        align 16
start:  call [IATVA]
        cmp eax, EXPECT
        jne bad
        mov edx, okmsg
        mov eax, 4c00h
        jmp fin
bad:    mov edx, bm
        mov eax, 4c01h
fin:    push eax
        mov ah, 9
        int 21h
        pop eax
        int 21h
okmsg: db 'PE import graph: ok', 13, 10, '$'
bm: db 'PE import graph: bad value', 13, 10, '$'
