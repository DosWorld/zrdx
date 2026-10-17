%include "../rdf/T.INC"
bits 32
global start
TNAME 'DOSSTK active stack'
section .bss
step: resb 1
oldss: resw 1
oldsp: resd 1
sel: resw 1
result: resd 1
section .text
start: BEGIN
       mov byte [step], 1
       mov ebx, 100h
       mov eax, 100h
       int 31h
       jc near bad
       mov [sel], dx
       mov [oldss], ss
       mov [oldsp], esp
       mov ss, dx
       mov esp, 1000h
       mov byte [step], 2
       mov eax, 101h
       int 31h
       jnc .broken
       cmp ax, 9
       jne .broken
       mov byte [step], 3
       and dx, ~3
       mov eax, 101h
       int 31h
       jnc .broken
       cmp ax, 9
       jne .broken
       xor eax, eax
       jmp short .restore
.broken:
       or eax, 10000h
.restore:
       mov [result], eax
       mov ss, [oldss]
       mov esp, [oldsp]
       cmp dword [result], 0
       jne near bad
       mov byte [step], 4
       mov dx, [sel]
       lsl eax, edx
       jnz near bad
       cmp eax, 0fffh
       jne near bad
       mov eax, 101h
       int 31h
       jc near bad
       PASS
%include "FAIL.INC"
