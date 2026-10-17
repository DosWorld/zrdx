%include "../rdf/T.INC"
bits 32
global start
TNAME 'UNSUP unsupported dispatch'
section .data
calls: dw 0004h,0005h,0700h,0701h,0b04h,0bffh,0c01h,0c02h
       dw 000eh,00ffh,0103h,0307h,0801h,0c00h,0ffffh
NCALLS equ ($-calls)/2
section .bss
index: resd 1
savedsp: resd 1
segments: resw 4
flags: resd 1
section .text
start: BEGIN
       mov [segments],ds
       mov [segments+2],es
       push ds
       pop fs
       push ds
       pop gs
       mov [segments+4],fs
       mov [segments+6],gs
.next: mov eax,[index]
       movzx eax,word [calls+eax*2]
       mov ebx,12345678h
       mov ecx,23456789h
       mov edx,3456789ah
       mov esi,456789abh
       mov edi,56789abch
       mov ebp,6789abcdh
       mov [savedsp],esp
       std
       int 31h
       pushfd
       pop dword [flags]
       cld
       jnc near bad
       cmp ax,8001h
       jne near bad
       cmp esp,[savedsp]
       jne near bad
       cmp ebx,12345678h
       jne near bad
       cmp ecx,23456789h
       jne near bad
       cmp edx,3456789ah
       jne near bad
       cmp esi,456789abh
       jne near bad
       cmp edi,56789abch
       jne near bad
       cmp ebp,6789abcdh
       jne near bad
       mov ax,ds
       cmp ax,[segments]
       jne near bad
       mov ax,es
       cmp ax,[segments+2]
       jne near bad
       mov ax,fs
       cmp ax,[segments+4]
       jne near bad
       mov ax,gs
       cmp ax,[segments+6]
       jne near bad
       test dword [flags],400h
       jz near bad
       inc dword [index]
       cmp dword [index],NCALLS
       jb near .next
       PASS
bad:   cld
       FAIL
