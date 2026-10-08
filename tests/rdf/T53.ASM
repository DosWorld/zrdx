; DPMI 0501h: handles and addresses of live blocks must be different
%include "t.inc"
bits 32
global start
TNAME 'T53 dpmi handles'
section .data
cnt:    dd 0
section .bss
h:      resd 4
a:      resd 4
section .text
start:  BEGIN
.a:     mov eax, 501h
        mov ebx, 1
        xor ecx, ecx
        int 31h
        jc near bad
        shl esi, 16
        mov si, di
        shl ebx, 16
        mov bx, cx
        mov edx, [cnt]
        mov [h + edx * 4], esi
        mov [a + edx * 4], ebx
        inc dword [cnt]
        cmp dword [cnt], 4
        jb .a
        xor ecx, ecx
.o:     lea edx, [ecx + 1]
.i:     cmp edx, 4
        jae .n
        mov eax, [h + ecx * 4]
        cmp eax, [h + edx * 4]
        je near bad
        mov eax, [a + ecx * 4]
        cmp eax, [a + edx * 4]
        je near bad
        inc edx
        jmp .i
.n:     inc ecx
        cmp ecx, 3
        jb .o
        PASS
bad:    FAIL
