; table of string pointers in the data section
%include "t.inc"
bits 32
global start
TNAME 'T07 data pointers'
section .data
s1:     db 'Alpha', 0
s2:     db 'Bravo', 0
s3:     db 'Charlie', 0
s4:     db 'Delta', 0
ptrs:   dd s1, s2, s3, s4
chars:  db 'ABCD'
section .text
start:  BEGIN
        xor ecx, ecx
.l:     mov esi, [ptrs + ecx * 4]
        mov al, [chars + ecx]
        cmp [esi], al
        jne bad
        inc ecx
        cmp ecx, 4
        jb .l
        mov esi, [ptrs + 8]
        cmp dword [esi], 'Char'
        jne bad
        PASS
bad:    FAIL
