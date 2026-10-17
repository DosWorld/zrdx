%ifndef CLIENT32
%define CLIENT32 0
%endif
%ifdef BOUND
bits 32
global start
section .text
start: cld
%else
bits 16
org 100h
start:  cld
        mov sp, stacktop
        push cs
        pop ds
        push cs
        pop es
        mov bx, (stacktop - $$ + 100h + 15) / 16
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
        mov ax, CLIENT32
        call far [entry]
        jc bad
        push ds
        pop es
%endif
.cycle: mov ecx, 1
        xor eax, eax
        int 31h
        jc near bad
        mov [first], ax
        mov bx, ax
        mov eax, 1
        int 31h
        jc near bad
        movzx ecx, word [first]
        shr ecx, 3
        neg ecx
        add ecx, 8192
        xor eax, eax
        int 31h                 ;one allocation through index 8191
        jc near bad
        cmp ax, [first]
        jne near bad
        mov ecx, 1
        xor eax, eax
        int 31h
        jnc near bad
        cmp ax, 8011h
        jne near bad
        mov ebx, 0FFFFh
        mov ecx, 1234h
        mov edx, 5678h
        mov eax, 7
        int 31h
        jc near bad
        mov eax, 6
        int 31h
        jc near bad
        cmp cx, 1234h
        jne near bad
        cmp dx, 5678h
        jne near bad
        mov eax, 1
        int 31h
        jc near bad
        mov ebx, 0FFF7h
        mov eax, 1
        int 31h
        jc near bad
        mov ecx, 2
        xor eax, eax
        int 31h
        jc near bad
        cmp ax, 0FFF7h
        jne near bad
        mov bx, [first]
.free:  mov eax, 1
        int 31h
        jc near bad
        add bx, 8
        jnc .free
        mov ecx, 1
        xor eax, eax
        int 31h
        jc near bad
        cmp ax, [first]
        jne near bad
        mov bx, ax
        mov eax, 1
        int 31h
        jc near bad
        dec byte [rounds]
        jnz .cycle
        jmp passed
bad:    mov esi, fail
        mov bx, 1
        jmp print
passed: mov esi, ok
        xor bx, bx
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
first: dw 0
rounds: db 8
ok: db 'LDTAIL final slots and exhaustion: ok',13,10,0
fail: db 'LDTAIL: FAIL',13,10,0
%ifndef BOUND
        times 1024 db 0
stacktop:
%endif
