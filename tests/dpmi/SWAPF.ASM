; Full swap store: a request larger than free memory plus all
; swap slots (4096) must fail with CF=1 and give everything
; back; one that fits must then succeed. The client stack is
; locked first (a swapped stack faults at PL0).
%include "../rdf/T.INC"
bits 32
global start
TNAME 'SWAPF store full'
SLOTS equ 4096
section .data
buf:    times 12 dd 0
section .text
start:  BEGIN
        mov ax, 6
        mov bx, ss
        int 31h
        jc near bad
        mov ebx, ecx
        shl ebx, 16
        mov bx, dx
        add ebx, esp
        and ebx, ~0FFFh
        sub ebx, 1000h
        mov ecx, ebx
        shr ebx, 16
        xor esi, esi
        mov edi, 2000h
        mov ax, 600h
        int 31h
        jc near bad
        push ds
        pop es
        mov edi, buf
        mov ax, 500h
        int 31h
        mov esi, [buf]
        shr esi, 12
        lea eax, [esi + SLOTS + 64]
        call alloc
        jnc near bad
        lea eax, [esi + 48]
        call alloc
        jc near bad
        PASS
;in: eax = pages; out: CF as 501h
alloc:  shl eax, 12
        mov ecx, eax
        mov ebx, eax
        shr ebx, 16
        mov ax, 501h
        int 31h
        ret
bad:    FAIL
