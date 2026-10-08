; DPMI memory: random allocate / verify / reallocate / free with data patterns
%include "t.inc"
bits 32
global start
TNAME 'T51 dpmi memory stress'
NSLOT   equ 48
ITER    equ 4000
section .bss
hnd:    resd NSLOT
lin:    resd NSLOT
siz:    resd NSLOT
stamp:  resd NSLOT
seed:   resd 1
iter:   resd 1
newsz:  resd 1
oldsz:  resd 1
oldst:  resd 1
cur:    resd 1
section .text
rnd:    mov eax, [seed]
        imul eax, 1103515245
        add eax, 12345
        mov [seed], eax
        shr eax, 8
        ret
fill:   mov edi, [lin + ebx * 4]
        mov ecx, [siz + ebx * 4]
        mov edx, [stamp + ebx * 4]
        shr ecx, 2
.f:     mov [edi], edx
        add edi, 4
        inc edx
        loop .f
        ret
check:  mov edi, [lin + ebx * 4]
        mov ecx, [siz + ebx * 4]
        mov edx, [stamp + ebx * 4]
        shr ecx, 2
.f:     cmp [edi], edx
        jne .bad
        add edi, 4
        inc edx
        loop .f
        clc
        ret
.bad:   stc
        ret
newsize: call rnd
        xor edx, edx
        mov ecx, 60000
        div ecx
        lea ecx, [edx * 4 + 64]
        ret
store:  shl ebx, 16
        mov bx, cx
        shl esi, 16
        mov si, di
        mov [lin + ebp * 4], ebx
        mov [hnd + ebp * 4], esi
        mov ecx, [newsz]
        mov [siz + ebp * 4], ecx
        mov edx, [iter]
        shl edx, 12
        mov [stamp + ebp * 4], edx
        mov ebx, ebp
        call fill
        ret
overlap: mov esi, [lin + ebp * 4]
        mov edi, esi
        add edi, [siz + ebp * 4]
        xor edx, edx
.o:     cmp edx, ebp
        je .n
        cmp dword [hnd + edx * 4], 0
        je .n
        mov eax, [lin + edx * 4]
        mov ebx, eax
        add ebx, [siz + edx * 4]
        cmp esi, ebx
        jae .n
        cmp eax, edi
        jae .n
        stc
        ret
.n:     inc edx
        cmp edx, NSLOT
        jb .o
        mov eax, hnd
        mov ebx, newsz + 4
        cmp esi, ebx
        jae .ok
        cmp eax, edi
        jae .ok
        stc
        ret
.ok:    clc
        ret
start:  BEGIN
        mov dword [seed], 12345
        mov dword [iter], 0
.main:  call rnd
        xor edx, edx
        mov ecx, NSLOT
        div ecx
        mov ebp, edx
        mov [cur], ebp
        cmp dword [hnd + ebp * 4], 0
        jne .have
        call newsize
        mov [newsz], ecx
        mov eax, 501h
        mov ebx, ecx
        shr ebx, 16
        int 31h
        jc near .skip
        call store
        call overlap
        jc near bad
        jmp .skip
.have:  mov ebx, ebp
        call check
        jc near bad
        call rnd
        test al, 1
        jz near .free
        call newsize
        mov [newsz], ecx
        mov eax, [siz + ebp * 4]
        mov [oldsz], eax
        mov eax, [stamp + ebp * 4]
        mov [oldst], eax
        mov eax, 503h
        mov esi, [hnd + ebp * 4]
        mov edi, esi
        shr esi, 16
        mov ebx, ecx
        shr ebx, 16
        int 31h
        jc near .skip
        push ebx
        push ecx
        push esi
        push edi
        shl ebx, 16
        mov bx, cx
        mov edi, ebx
        mov ecx, [oldsz]
        cmp ecx, [newsz]
        jbe .pk
        mov ecx, [newsz]
.pk:    shr ecx, 2
        mov edx, [oldst]
.pc:    cmp [edi], edx
        jne near bad
        add edi, 4
        inc edx
        loop .pc
        pop edi
        pop esi
        pop ecx
        pop ebx
        call store
        call overlap
        jc near bad
        jmp .skip
.free:  mov esi, [hnd + ebp * 4]
        mov edi, esi
        shr esi, 16
        mov eax, 502h
        int 31h
        jc near bad
        mov dword [hnd + ebp * 4], 0
.skip:  inc dword [iter]
        cmp dword [iter], ITER
        jb near .main
        xor ebp, ebp
.fin:   cmp dword [hnd + ebp * 4], 0
        je .nx
        mov ebx, ebp
        call check
        jc near bad
        mov esi, [hnd + ebp * 4]
        mov edi, esi
        shr esi, 16
        mov eax, 502h
        int 31h
        jc near bad
.nx:    inc ebp
        cmp ebp, NSLOT
        jb .fin
        PASS
bad:    FAIL
