%include "../rdf/T.INC"
bits 32
global start
TNAME 'DESCR specific selectors'
section .bss
step: resb 1
sels: resw 16
section .text
start: BEGIN
       mov byte [step], 1
       xor esi, esi
.alloc:
       mov ebx, esi
       shl ebx, 3
       or bl, 4
       mov eax, esi
       and al, 3
       or bl, al
       mov eax, 0dh
       int 31h
       jc near bad
       mov [sels+esi*2], bx
       mov eax, 0dh
       int 31h
       jnc near bad
       mov byte [step], 2
       mov ecx, 1234h
       mov edx, esi
       shl edx, 12
       mov eax, 7
       int 31h
       jc near bad
       mov eax, 6
       int 31h
       jc near bad
       cmp cx, 1234h
       jne near bad
       mov eax, esi
       shl eax, 12
       cmp dx, ax
       jne near bad
       xor ecx, ecx
       mov edx, 0ffffh
       mov eax, 8
       int 31h
       jc near bad
       mov ecx, 0f2h
       mov eax, 9
       int 31h
       jc near bad
       lsl eax, ebx
       jnz near bad
       cmp eax, 0ffffh
       jne near bad
       inc esi
       cmp esi, 16
       jb .alloc
       mov byte [step], 3
       xor ecx, ecx
       xor eax, eax
       int 31h
       jnc near bad
       mov ecx, 1
       xor eax, eax
       int 31h
       jc near bad
       cmp ax, 128
       jb near bad
       mov bx, ax
       mov eax, 1
       int 31h
       jc near bad
.free: dec esi
       mov bx, [sels+esi*2]
       mov eax, 1
       int 31h
       jc near bad
       test esi, esi
       jnz .free
       xor ebx, ebx
       mov eax, 0dh
       int 31h
       jnc near bad
       mov ebx, 0ffffh
       mov eax, 0dh
       int 31h
       jnc near bad
       PASS
%include "FAIL.INC"
