; heap-like use: DPMI blocks allocated in a loop and freed
%include "t.inc"
bits 32
global start
TNAME 'T31 dpmi alloc loop'
section .bss
hnd:    resd 8
section .text
start:  BEGIN
        xor ebp, ebp
.a:     mov eax, 501h
        xor ebx, ebx
        mov ecx, 8000h
        int 31h
        jc bad
        shl esi, 16
        mov si, di
        mov [hnd + ebp * 4], esi
        shl ebx, 16
        mov bx, cx
        mov dword [ebx], 0F00DF00Dh
        cmp dword [ebx], 0F00DF00Dh
        jne bad
        inc ebp
        cmp ebp, 8
        jb .a
.fr:    dec ebp
        mov esi, [hnd + ebp * 4]
        mov edi, esi
        shr esi, 16
        mov eax, 502h
        int 31h
        jc bad
        or ebp, ebp
        jnz .fr
        PASS
bad:    FAIL
