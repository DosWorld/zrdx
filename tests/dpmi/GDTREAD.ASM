; Built-in descriptor contracts, including its public flat GDT selector.
%include "../rdf/T.INC"
bits 32
global start
TNAME 'GDTREAD read and protection'
section .bss
step: resb 1
desc: resb 8
section .text
start: BEGIN
       push ds
       pop es
       mov byte [step], 1
       mov ebx, 76540018h
.rpl:  mov eax, 6
       int 31h
       jc near bad
       test cx, cx
       jnz near bad
       test dx, dx
       jnz near bad
       mov edi, desc
       mov eax, 0bh
       int 31h
       jc near bad
       cmp word [desc], 0ffffh
       jne near bad
       test byte [desc+5], 10h
       jz near bad
       inc ebx
       cmp bl, 1ch
       jb .rpl
       mov byte [step], 2
       mov bx, 1bh
       mov eax, 7
       int 31h
       jnc near bad
       mov eax, 9
       int 31h
       jnc near bad
       mov bx, 20h              ;kernel code is not a readable client GDT entry
       mov eax, 6
       int 31h
       jnc near bad
       mov bx, 3bh              ;a DPL-3 call gate is not a code/data descriptor
       mov eax, 6
       int 31h
       jnc near bad
       mov byte [step], 3
       mov bx, 1bh
       mov eax, 0ah
       int 31h
       jc near bad
       mov es, ax
       mov bx, ax
       mov eax, 1
       int 31h
       jc near bad
       mov ax, es
       test ax, ax
       jnz near bad
       push ds
       pop es
       mov byte [step], 4
       mov bx, ss
       and bx, 0fffch
       mov eax, 1
       int 31h
       jnc near bad
       PASS
%include "FAIL.INC"
