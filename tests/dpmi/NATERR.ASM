; Native-host input errors must return to the real-mode FAR caller.
bits 16
org 100h
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
        jc badrm
        mov ax, 1687h
        int 2Fh
        test ax, ax
        jnz badrm
        mov [entry], di
        mov [entry+2], es
        mov bx, si
        mov ah, 48h
        int 21h
        jc badrm
        mov [private], ax
        mov es, ax
        push cs
        pop fs
        push cs
        pop gs
        mov ebx, 12345678h
        mov ecx, 23456789h
        mov edx, 3456789Ah
        mov esi, 456789ABh
        mov edi, 56789ABCh
        mov ebp, 6789ABCDh
        mov [savedsp], sp
        mov eax, 55550002h
        call far [entry]
        jnc badrm
        call preserved
        mov ax, es
        cmp ax, [private]
        jne badrm
        xor ax, ax
        mov es, ax
        mov eax, 55550001h
        call far [entry]
        jnc badrm
        call preserved
        mov ax, es
        test ax, ax
        jnz badrm
        mov es, [private]
        mov ax, 1
        call far [entry]
        jc badrm
        mov si, ok
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
        int 21h
preserved:
        cmp ebx, 12345678h
        jne badrm
        cmp ecx, 23456789h
        jne badrm
        cmp edx, 3456789Ah
        jne badrm
        cmp esi, 456789ABh
        jne badrm
        cmp edi, 56789ABCh
        jne badrm
        cmp ebp, 6789ABCDh
        jne badrm
        mov ax, cs
        mov [cs:savedcs], ax
        mov ax, ds
        cmp ax, [cs:savedcs]
        jne badrm
        mov ax, ss
        cmp ax, [savedcs]
        jne badrm
        mov ax, fs
        cmp ax, [savedcs]
        jne badrm
        mov ax, gs
        cmp ax, [savedcs]
        jne badrm
        mov ax, sp
        add ax, 2
        cmp ax, [savedsp]
        jne badrm
        ret
badrm:  push cs
        pop ds
        mov dx, fail
        mov ah, 9
        int 21h
        mov ax, 4C01h
        int 21h
entry:  dd 0
private: dw 0
savedsp: dw 0
savedcs: dw 0
ok:     db 'NATERR rejected entries: ok', 13, 10, 0
fail:   db 'NATERR rejected entries: FAIL', 13, 10, '$'
        times 1024 db 0
stacktop:
