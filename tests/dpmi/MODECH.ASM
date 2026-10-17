; A sequential client may enter only the VM's first successful bitness.
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 0
%endif
start:  cld
        push cs
        pop ds
        mov sp, stacktop
        mov al, [80h]
        mov [expect], al
        mov bx, (stacktop - $$ + 100h + 15) / 16
        mov ah, 4Ah
        int 21h
        jc bad
        mov byte [stage], 2
        mov ax, 1687h
        int 2Fh
        test ax, ax
        jnz bad
        mov [entry], di
        mov [entry+2], es
        mov byte [stage], 3
        mov bx, si
        mov ah, 48h
        int 21h
        jc bad
        mov [private], ax
        mov es, ax
        mov byte [stage], 4
        mov ax, CLIENT32
        call far [entry]
        jnc entered
        cmp byte [expect], 0
        je bad
        mov es, [private]
        mov ah, 49h
        int 21h
        jc bad
        jmp short good
entered:
        mov byte [stage], 5
        cmp byte [expect], 0
        jne bad
 good:  mov ax, 4C00h
        int 21h
bad:    mov ah, 4Ch
        mov al, [stage]
        int 21h
stage: db 1
expect: db 0
entry: dd 0
private: dw 0
        times 512 db 0
stacktop:
