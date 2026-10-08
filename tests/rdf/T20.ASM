; DPMI 0501h / 0502h: allocate a block, write and read it through the flat data selector, free it
%include "t.inc"
bits 32
global start
TNAME 'T20 dpmi memory'
section .text
start:  BEGIN
        mov eax, 501h
        mov ebx, 1
        xor ecx, ecx
        int 31h
        jc bad
        shl ebx, 16
        mov bx, cx
        mov edi, ebx
        mov ebp, esi
        shl ebp, 16
        mov bp, di
        mov dword [edi], 0A5A5A5A5h
        mov dword [edi + 0FFFCh], 5A5A5A5Ah
        cmp dword [edi], 0A5A5A5A5h
        jne bad
        cmp dword [edi + 0FFFCh], 5A5A5A5Ah
        jne bad
        PASS
bad:    FAIL
