%include "../rdf/T.INC"
bits 32
global start
TNAME 'CONTROL version and virtual IF'
section .bss
step: resb 1
flags: resd 1
section .text
start: BEGIN
       mov byte [step], 1
       mov eax, 0400h
       mov ebx, 12345678h
       mov ecx, 23456789h
       mov edx, 3456789ah
       int 31h
       jc near bad
       cmp ax, 005ah
       jne near bad
       test bx, 1
       jz near bad
       test bx, 0fffch
       jnz near bad
%ifdef REALMODE
       mov ax, bx
       and ax, 2
       cmp ax, REALMODE * 2
       jne near bad
%endif
       cmp cl, 3
       jb near bad
       cmp dx, 0870h
       jne near bad
       cmp ebx, 12340000h
       jb near bad
       shr ebx, 16
       cmp bx, 1234h
       jne near bad
       shr ecx, 8
       cmp ecx, 234567h
       jne near bad
       shr edx, 16
       cmp dx, 3456h
       jne near bad
       mov byte [step], 2
       mov eax, 0604h
       int 31h
       jc near bad
       test bx, bx
       jnz near bad
       cmp cx, 1000h
       jne near bad
       mov byte [step], 3
       pushfd
       pop dword [flags]
       mov eax, 0900h
       int 31h
       jc near bad
       mov ebx, [flags]
       shr ebx, 9
       and ebx, 1
       cmp al, bl
       jne near bad
       mov eax, 0902h
       int 31h
       jc near bad
       test al, al
       jnz near bad
       pushfd
       pop edx
       test edx, 0200h
       jnz near bad
       mov eax, 0901h
       int 31h
       jc near bad
       test al, al
       jnz near bad
       mov eax, 0902h
       int 31h
       jc near bad
       cmp al, 1
       jne near bad
       pushfd
       pop edx
       test edx, 0200h
       jz near bad
       push dword [flags]
       popfd
       PASS
%include "FAIL.INC"
