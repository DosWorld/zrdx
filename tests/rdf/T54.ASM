; DPMI memory exhaustion: failed 0501h/0503h must return an error and leave the host working
%include "t.inc"
bits 32
global start
TNAME 'T54 dpmi out of memory'
MAXB    equ 4096
section .data
step:   db 0
section .bss
hnd:    resd MAXB
cnt:    resd 1
last:   resd 1
section .text
start:  BEGIN
        mov dword [cnt], 0
.a:     mov eax, 501h
        mov ebx, 10h
        xor ecx, ecx
        int 31h
        jc near .full
        shl esi, 16
        mov si, di
        mov edx, [cnt]
        mov [hnd + edx * 4], esi
        inc dword [cnt]
        cmp dword [cnt], MAXB
        jb near .a
.full:  mov al, 'F'
        cmp dword [cnt], 4
        jb near bad
        mov eax, 501h
        mov ebx, 7FFFh
        mov ecx, 0F000h
        int 31h
        jnc near bad
        mov edx, [cnt]
        dec edx
        mov esi, [hnd + edx * 4]
        mov edi, esi
        shr esi, 16
        mov eax, 503h
        mov ebx, 7FFFh
        mov ecx, 0F000h
        int 31h
        jnc near bad
        mov edx, [cnt]
        dec edx
        mov esi, [hnd + edx * 4]
        mov edi, esi
        shr esi, 16
        mov eax, 503h
        mov ebx, 0A0h
        xor ecx, ecx
        int 31h
        jc near .nogrow
        shl esi, 16
        mov si, di
        mov edx, [cnt]
        dec edx
        mov [hnd + edx * 4], esi
.nogrow:
        xor ebp, ebp
.f:     mov esi, [hnd + ebp * 4]
        mov edi, esi
        shr esi, 16
        mov eax, 502h
        int 31h
        jc near bad
        inc ebp
        cmp ebp, [cnt]
        jb near .f
        mov eax, 501h
        mov ebx, 10h
        xor ecx, ecx
        int 31h
        jc near bad
        PASS
bad:    FAIL
