%include "../rdf/T.INC"
bits 32
global start
TNAME 'DBHIT hardware watch'
section .bss
step: resb 1
hits: resd 1
resb (4 - (($ - $$) & 3)) & 3
watched: resd 1
handle: resw 1
oldexc: resd 1
        resw 1
section .text
handler:
       inc dword [hits]
       or dword [ss:esp+20], 10000h ;resume an execution breakpoint once
       retf
start: BEGIN
       mov byte [step], 1
       mov eax, 202h
       mov bl, 1
       int 31h
       jc near bad
       mov [oldexc], edx
       mov [oldexc+4], cx
       mov eax, 203h
       mov bl, 1
       mov cx, cs
       mov edx, handler
       int 31h
       jc near bad
       mov byte [step], 2
       mov ebx, watched
       mov ecx, ebx
       shr ebx, 16
       mov edx, 0104h
       mov eax, 0b00h
       int 31h
       jc near bad
       mov [handle], bx
       mov byte [step], 3
       pushad
       mov ah, 30h             ;watch must survive a real-mode DOS transition
       int 21h
       popad
       mov dword [watched], 12345678h
       cmp dword [hits], 1
       jne near bad
       mov eax, 0b02h
       int 31h
       jc near bad
       cmp ax, 1
       jne near bad
       mov eax, 0b02h
       int 31h
       jc near bad
       cmp ax, 1
       jne near bad
       mov eax, 0b03h
       int 31h
       jc near bad
       mov eax, 0b02h
       int 31h
       jc near bad
       test ax, ax
       jnz near bad
       mov eax, 0b01h
       int 31h
       jc near bad
       mov dword [watched], 0
       cmp dword [hits], 1
       jne near bad
       mov byte [step], 4
       mov edx, 0102h
       call arm
       mov word [watched], 4321h
       cmp dword [hits], 2
       jne near bad
       call disarm
       mov byte [step], 5
       mov edx, 0201h
       call arm
       mov al, [watched]
       cmp dword [hits], 3
       jne near bad
       call disarm
       mov byte [step], 6
       mov ebx, execute
       mov ecx, ebx
       shr ebx, 16
       mov edx, 0001h
       mov eax, 0b00h
       int 31h
       jc near bad
       mov [handle], bx
       call execute
       cmp dword [hits], 4
       jne near bad
       call disarm
       call execute
       cmp dword [hits], 4
       jne near bad
       mov edx, [oldexc]
       mov cx, [oldexc+4]
       mov bl, 1
       mov eax, 203h
       int 31h
       jc near bad
       PASS
execute:
       nop
       ret
arm:   mov ebx, watched
       mov ecx, ebx
       shr ebx, 16
       mov eax, 0b00h
       int 31h
       jc near bad
       mov [handle], bx
       ret
disarm:
       mov bx, [handle]
       mov eax, 0b02h
       int 31h
       jc near bad
       cmp ax, 1
       jne near bad
       mov eax, 0b01h
       int 31h
       jc near bad
       ret
%include "FAIL.INC"
