; DPMI 0503h: resize in place, resize with move, shrink, resize into a freed neighbour
%include "t.inc"
bits 32
global start
TNAME 'T55 dpmi realloc'
section .bss
hA:     resd 1
hB:     resd 1
hC:     resd 1
lA:     resd 1
section .text
; eax=bytes -> esi:edi handle in ebp, linear in ebx
alloc:  push ecx
        mov ecx, eax
        mov ebx, eax
        shr ebx, 16
        mov eax, 501h
        int 31h
        jc .e
        shl ebx, 16
        mov bx, cx
        shl esi, 16
        mov si, di
        mov ebp, esi
        pop ecx
        clc
        ret
.e:     pop ecx
        stc
        ret
; ebp=handle, eax=new size -> ebx linear, ebp new handle
resize: mov ecx, eax
        mov ebx, eax
        shr ebx, 16
        mov esi, ebp
        mov edi, ebp
        shr esi, 16
        mov eax, 503h
        int 31h
        jc .e
        shl ebx, 16
        mov bx, cx
        shl esi, 16
        mov si, di
        mov ebp, esi
        clc
        ret
.e:     stc
        ret
release: mov esi, ebp
        mov edi, ebp
        shr esi, 16
        mov eax, 502h
        int 31h
        ret
touch:  mov dword [ebx], 0A5A5A5A5h
        mov edx, eax
        mov dword [ebx + edx - 4], 5A5A5A5Ah
        ret
start:  BEGIN
        mov eax, 77C8h
        call alloc
        jc near bad
        mov eax, 77C8h
        call touch
        mov eax, 0A3E8h
        call resize
        jc near bad
        mov eax, 0A3E8h
        call touch
        call release
        jc near bad
        mov eax, 1000h
        call alloc
        jc near bad
        mov [hA], ebp
        mov eax, 1000h
        call alloc
        jc near bad
        mov [hB], ebp
        mov ebp, [hA]
        mov eax, 3000h
        call resize
        jc near bad
        mov eax, 3000h
        call touch
        mov [hA], ebp
        mov ebp, [hA]
        call release
        mov ebp, [hB]
        call release
        mov eax, 20000h
        call alloc
        jc near bad
        mov eax, 1000h
        call resize
        jc near bad
        call release
        mov eax, 4000h
        call alloc
        jc near bad
        mov [hA], ebp
        mov eax, 4000h
        call alloc
        jc near bad
        mov [hB], ebp
        mov eax, 4000h
        call alloc
        jc near bad
        mov [hC], ebp
        mov ebp, [hB]
        call release
        mov ebp, [hA]
        mov eax, 6000h
        call resize
        jc near bad
        mov [hA], ebp
        mov ebp, [hA]
        mov eax, 20000h
        call resize
        jc near bad
        mov [hA], ebp
        mov ebp, [hA]
        call release
        mov ebp, [hC]
        call release
        PASS
bad:    FAIL
