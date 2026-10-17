; Exercise a device mapping that shares a page table with ordinary RAM.
%include "../rdf/T.INC"
bits 32
global start
TNAME 'MAPBND shared directory'
section .bss
step: resb 1
block: resd 1
handle: resd 1
mapped: resd 1
info: resb 48
section .text
start: BEGIN
       mov byte [step], 1
       mov ebx, 40h
       xor ecx, ecx
       mov eax, 501h
       int 31h
       jc near bad
       shl ebx, 16
       mov bx, cx
       mov [block], ebx
       shl esi, 16
       mov si, di
       mov [handle], esi
       mov edi, ebx
       mov ecx, 1024
.fill: mov dword [edi], 12345678h
       add edi, 1000h
       loop .fill
       mov dword [ebx+400000h-4], 76543210h
       mov byte [step], 2
       ; The existing allocator chooses the top of the linear arena.
       ; Leave one page above the normal block, in its last directory.
       lea eax, [ebx+400000h+1000h]
       mov edi, 0ffc00000h
       sub edi, eax
       mov esi, edi
       shr esi, 16
       mov ebx, 10h
       xor ecx, ecx
       mov eax, 800h
       std                       ;the host must not inherit a client copy direction
       int 31h
       jc near bad
       pushfd
       pop eax
       test ah, 4
       jz near bad
       cld
       shl ebx, 16
       mov bx, cx
       mov [mapped], ebx
       mov ebx, [block]
       cmp dword [ebx], 12345678h
       jne near bad
       cmp dword [ebx+400000h-4], 76543210h
       jne near bad
       mov edi, ebx
       mov ecx, 1024
.check:cmp dword [edi], 12345678h
       jne near bad
       add edi, 1000h
       loop .check
       push ds
       pop es                   ;BEGIN leaves ES on the PSP, not the data buffer
       mov edi, info
       mov eax, 500h
       int 31h
       jc near bad
       mov eax, [info]
       cmp eax, 1000h
       jne near bad
       mov byte [step], 3
       mov edi, [handle]
       mov esi, edi
       shr esi, 16
       mov eax, 502h
       int 31h
       jc near bad
       mov edi, info
       mov eax, 500h
       int 31h
       jc near bad
       mov eax, [info]
       cmp eax, 401000h
       jne near bad
       ; Freeing normal RAM must leave the device PTE present.
       mov ebx, [mapped]
       mov eax, [ebx]
       PASS
%include "FAIL.INC"
