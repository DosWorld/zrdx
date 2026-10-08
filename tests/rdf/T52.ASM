; DPMI exception handler: the handler must be entered with DF clear and may skip the faulting instruction
%include "t.inc"
bits 32
global start
TNAME 'T52 exception handler'
section .data
dfseen: dd 0
hits:   dd 0
section .bss
step:   resb 1
errax:  resw 1
section .text
handler:
        push eax
        pushfd
        pop eax
        test eax, 400h
        jz .ok
        mov dword [ds:dfseen], 1
.ok:    inc dword [ds:hits]
        pop eax
        add dword [esp + 12], 5
        retf
start:  BEGIN
        push ds
        pop es
        mov eax, 203h
        mov ebx, 0Eh
        mov cx, cs
        mov edx, handler
        mov byte [step], 1
        int 31h
        jc near bad
        cld
        mov eax, [0F0000000h]
        cld
        mov eax, [0F0000000h]
        mov ah, [hits]
        mov al, [dfseen]
        cmp dword [hits], 2
        jne near bad
        cmp dword [dfseen], 0
        jne near bad
        PASS
bad:    mov [errax], ax
        mov al, [step]
        aam
        mov bx, ax
        mov dl, bh
        add dl, '0'
        mov ah, 2
        int 21h
        mov dl, bl
        add dl, '0'
        mov ah, 2
        int 21h
        mov dl, ' '
        mov ah, 2
        int 21h
        mov bx, [errax]
        mov cx, 4
.hx:    rol bx, 4
        mov dl, bl
        and dl, 0Fh
        add dl, '0'
        cmp dl, '9'
        jbe .hd
        add dl, 7
.hd:    mov ah, 2
        int 21h
        loop .hx
        mov dl, ' '
        mov ah, 2
        int 21h
        FAIL
