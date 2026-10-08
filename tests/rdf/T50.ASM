; DPMI selectors: allocate until the LDT is full, set/get bases, free everything, allocate again
%include "t.inc"
bits 32
global start
TNAME 'T50 dpmi selectors'
MAXSEL  equ 8200
section .bss
step:   resb 1
errax:  resw 1
sels:   resw MAXSEL
count:  resd 1
section .text
start:  BEGIN
        push ds
        pop es
        xor esi, esi
.a:     xor eax, eax
        mov ecx, 1
        mov byte [step], 1
        int 31h
        jc .full
        mov [sels + esi * 2], ax
        inc esi
        cmp esi, MAXSEL
        jb .a
.full:  mov [count], esi
        cmp esi, 1000
        jb near bad
        xor edi, edi
.b:     movzx ebx, word [sels + edi * 2]
        mov eax, 7
        mov ecx, edi
        shl ecx, 4
        mov edx, ecx
        shr ecx, 16
        mov byte [step], 2
        int 31h
        jc near bad
        inc edi
        cmp edi, [count]
        jb .b
        xor edi, edi
.c:     movzx ebx, word [sels + edi * 2]
        mov eax, 6
        mov byte [step], 3
        int 31h
        jc near bad
        mov eax, edi
        shl eax, 4
        shl ecx, 16
        mov cx, dx
        cmp ecx, eax
        jne near bad
        inc edi
        cmp edi, [count]
        jb .c
        xor edi, edi
.d:     movzx ebx, word [sels + edi * 2]
        mov eax, 1
        mov byte [step], 4
        int 31h
        jc near bad
        inc edi
        cmp edi, [count]
        jb .d
        mov ecx, 100
        xor eax, eax
        mov byte [step], 5
        int 31h
        jc near bad
        mov ebx, eax
        mov eax, 1
        mov byte [step], 6
        int 31h
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
