; table of code addresses in the data section, indirect calls
%include "t.inc"
bits 32
global start
TNAME 'T06 call table'
section .data
tbl:    dd f1, f2, f3, f4
section .text
f1:     mov eax, 1
        ret
f2:     mov eax, 20
        ret
f3:     mov eax, 300
        ret
f4:     mov eax, 4000
        ret
start:  BEGIN
        xor ebx, ebx
        xor ecx, ecx
.l:     call [tbl + ecx * 4]
        add ebx, eax
        inc ecx
        cmp ecx, 4
        jb .l
        cmp ebx, 4321
        jne bad
        PASS
bad:    FAIL
