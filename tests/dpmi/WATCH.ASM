%include "../rdf/T.INC"
bits 32
global start
TNAME 'WATCH debug services'
section .bss
step: resb 1
handles: resw 4
resb (4 - (($ - $$) & 3)) & 3
watched: resd 1
section .text
start: BEGIN
       mov byte [step], 1
       xor esi, esi
.alloc:
       mov ebx, watched
       mov ecx, ebx
       shr ebx, 16
       mov edx, 0101h
       mov eax, 0b00h
       int 31h
       jc near bad
       mov [handles + esi*2], bx
       mov byte [step], 2
       mov eax, 0b02h
       int 31h
       jc near bad
       test ax, ax
       jnz near bad
       mov byte [step], 3
       mov eax, 0b03h
       int 31h
       jc near bad
       mov byte [step], 4
       inc esi
       cmp esi, 4
       jb .alloc
       mov ebx, watched
       mov ecx, ebx
       shr ebx, 16
       mov edx, 0101h
       mov eax, 0b00h
       int 31h
       jnc near bad
.free: mov byte [step], 5
       dec esi
       mov bx, [handles + esi*2]
       mov eax, 0b01h
       int 31h
       jc near bad
       mov eax, 0b01h
       int 31h
       jnc near bad
       test esi, esi
       jnz .free
       mov ebx, watched
       mov ecx, ebx
       shr ebx, 16
       mov edx, 0103h
       mov eax, 0b00h
       int 31h
       jnc near bad
       mov edx, 0002h
       mov eax, 0b00h
       int 31h
       jnc near bad
       mov edx, 0301h
       mov eax, 0b00h
       int 31h
       jnc near bad
       inc ecx
       mov edx, 0104h
       mov eax, 0b00h
       int 31h
       jnc near bad
       PASS
%include "FAIL.INC"
