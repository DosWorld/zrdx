; DPMI: allocate the whole free memory (the largest block reported by 0500h) as one block
%include "t.inc"
bits 32
global start
TNAME 'T57 dpmi largest block'
section .bss
info:   resb 48
hnd:    resd 1
lin:    resd 1
size:   resd 1
section .text
pr32:   pushad
        mov ebx, eax
        mov ecx, 8
.h:     rol ebx, 4
        mov dl, bl
        and dl, 0Fh
        add dl, '0'
        cmp dl, '9'
        jbe .d
        add dl, 7
.d:     mov ah, 2
        int 21h
        loop .h
        mov dl, ' '
        mov ah, 2
        int 21h
        popad
        ret
start:  BEGIN
        push ds
        pop es
        mov edi, info
        mov eax, 500h
        int 31h
        jc near bad
        mov eax, [info]
        mov [size], eax
        call pr32
        or eax, eax
        jz near bad
        mov ebx, eax
        mov ecx, eax
        shr ebx, 16
        mov eax, 501h
        int 31h
        jc near bad
        shl ebx, 16
        mov bx, cx
        mov [lin], ebx
        shl esi, 16
        mov si, di
        mov [hnd], esi
        mov edi, [lin]
        mov ecx, [size]
        shr ecx, 12
        xor edx, edx
.w:     mov [edi], edx
        add edi, 1000h
        inc edx
        loop .w
        mov edi, [lin]
        mov ecx, [size]
        shr ecx, 12
        xor edx, edx
.r:     cmp [edi], edx
        jne near bad
        add edi, 1000h
        inc edx
        loop .r
        mov esi, [hnd]
        mov edi, esi
        shr esi, 16
        mov eax, 502h
        int 31h
        jc near bad
        mov ecx, [size]
        add ecx, 40000000h
        jc .ok
        mov ebx, ecx
        shr ebx, 16
        mov eax, 501h
        int 31h
        jnc near bad
.ok:    PASS
bad:    FAIL
