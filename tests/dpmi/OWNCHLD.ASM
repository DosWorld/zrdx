; Leave resources owned by this native child for host termination cleanup.
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 1
%endif
%ifndef FATAL
%define FATAL 0
%endif
start:
        cld
        push cs
        pop ds
        push cs
        pop es
        mov sp, stacktop
        mov bx, (stacktop - $$ + 100h + 15) / 16
        mov ah, 4Ah
        int 21h
        jc bad
        xor eax, eax
        mov ax, cs
        shl eax, 4
        add eax, watched
        mov [linear], eax
        mov ax, 1687h
        int 2Fh
        test ax, ax
        jnz bad
        mov [entry], di
        mov [entry+2], es
        mov bx, si
        mov ah, 48h
        int 21h
        jc bad
        mov es, ax
        mov ax, CLIENT32
        call far [entry]
        jc bad
        push ds
        pop es
        mov ecx, 100
        xor eax, eax
        int 31h
        jc bad
        mov ebx, 10h
        xor ecx, ecx
        mov eax, 501h
        int 31h
        jc bad
        mov ebx, 1001h
        mov eax, 100h
        int 31h
        jc bad
        push ds
        push cs
        pop ds
        mov esi, callback
        mov edi, cbregs
        mov eax, 303h
        int 31h
        pop ds
        jc bad
        mov ebx, [linear]
        mov ecx, ebx
        shr ebx, 16
        mov edx, 0101h
        mov eax, 0B00h
        int 31h
        jc bad
        mov ebx, 10h
        xor ecx, ecx
        xor esi, esi
        mov edi, 4096
        mov eax, 800h
        int 31h
        jc bad
%if FATAL = 2
        mov bx, 3
        mov cx, cs
        mov edx, badreturn
        mov eax, 203h
        int 31h
        jc bad
        int3
        jmp bad                 ;an invalid return must never resume this child
%elif FATAL
        ud2
%else
        mov ax, 4C00h
        int 21h
%endif
callback:
%if CLIENT32
        iretd
%else
        iret
%endif
%if FATAL = 2
badreturn:
        mov bx, sp
%if CLIENT32
        mov word [ss:bx+16], 20h ;inner-ring code selector, forced RPL3 faults
        o32 retf
%else
        mov word [ss:bx+8], 20h
        retf
%endif
%endif
bad:    mov ax, 4C01h
        int 21h
linear: dd 0
entry:  dd 0
watched: db 0
cbregs: times 50 db 0
        times 1024 db 0
stacktop:
