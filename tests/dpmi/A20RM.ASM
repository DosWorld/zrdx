; Native client verifies unflagged and flagged RAW calls with normal A20 off.
bits 16
org 100h
start:  cld
        mov sp, stacktop
        push cs
        pop ds
        push cs
        pop es
        mov [packet+36], cs
        mov [packet+44], cs
        mov bx, (stacktop-$$+100h+15)/16
        mov ah, 4Ah
        int 21h
        jc bad
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
        xor ax, ax
        call far [entry]
        jc bad
        push ds
        pop es
        xor ebx, ebx
        call invoke
        jc bad
        cmp word [packet+28], 1
        jne bad
        mov ebx, 100h
        call invoke
        jc bad
        cmp word [packet+28], 0
        jne bad
        mov si, ok
        xor bx, bx
        jmp print
invoke: mov edi, packet
        xor ecx, ecx
        mov eax, 301h
        int 31h
        ret
realcall:
        call a20query
        retf
%include "A20PROBE.INC"
bad:    mov si, fail
        mov bx, 1
print:  lodsb
        test al, al
        jz done
        mov dl, al
        pushad
        mov ah, 2
        int 21h
        popad
        jmp print
done:   mov ax, 4C00h
        mov al, bl
        int 21h
entry: dd 0
packet: times 42 db 0
        dw realcall, 0, 0, 0
ok: db 'A20RM reset flag: ok',13,10,0
fail: db 'A20RM reset flag: FAIL',13,10,0
        times 1024 db 0
stacktop:
