%include "t.inc"
bits 32
global start
TNAME 'T59 dpmi mapping lifetime'
section .bss
sels: resw 1024
section .text
start: BEGIN
       xor esi, esi
.map:  mov ebx, esi
       mov eax, 2
       int 31h
       jc bad
       mov [sels + esi * 2], ax
       movzx ebx, ax
       mov eax, 6
       xor ecx, ecx
       xor edx, edx
       int 31h
       jc bad
       shl ecx, 16
       mov cx, dx
       mov eax, esi
       shl eax, 4
       cmp ecx, eax
       jne bad
       inc esi
       cmp esi, 1024
       jb .map
       xor esi, esi
.again:
       mov ebx, esi
       mov eax, 2
       int 31h
       jc bad
       cmp ax, [sels + esi * 2]
       jne bad
       inc esi
       cmp esi, 1024
       jb .again
       PASS
bad:   FAIL
