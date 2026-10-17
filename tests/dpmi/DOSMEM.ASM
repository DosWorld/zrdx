; Native DOS block descriptor arrays, stack safety and resize atomicity.
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 0
%endif
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
        mov byte [step], 1
        mov ebx, 1001h
        mov edx, 0DEAD0000h
        mov eax, 100h
        int 31h
        jc bad
        mov [head], dx
        mov [rmseg], ax
        mov bx, dx
        lsl eax, ebx
        jnz bad
        cmp eax, 1000Fh          ;host capability, not client operand size
        jne bad
        mov bx, [head]
        mov byte [step], 2
        add bx, 8
        mov [tail], bx
        lsl eax, ebx
        jnz bad
        cmp eax, 0Fh
        jne bad
        mov eax, 6
        int 31h
        jc bad
        shl ecx, 16
        mov cx, dx
        movzx eax, word [rmseg]
        shl eax, 4
        add eax, 10000h
        cmp eax, ecx
        jne bad
        mov byte [step], 3
        mov bx, [head]
        mov eax, 1
        int 31h
        jnc bad
        mov eax, 7
        xor ecx, ecx
        xor edx, edx
        int 31h
        jnc bad
        mov [oldss], ss
        mov [oldsp], esp
        mov ss, [tail]
        mov esp, 10h
        mov dx, [head]
        mov eax, 101h
        int 31h
        setc [guard]
        mov [error], ax
        mov ss, [oldss]
        mov esp, [oldsp]
        cmp byte [guard], 1
        jne bad
        cmp word [error], 9
        jne bad
        mov es, [tail]
        mov fs, [head]
        mov gs, [tail]
        mov dx, [head]
        mov eax, 101h
        int 31h
        jc bad
        mov word [head], 0
        mov ax, es
        test ax, ax
        jnz bad
        mov ax, fs
        test ax, ax
        jnz bad
        mov ax, gs
        test ax, ax
        jnz bad
        mov byte [step], 4
        mov ebx, 1000h
        mov eax, 100h
        int 31h
        jc bad
        mov [head], dx
        mov bx, dx
        add bx, 8
        mov [tail], bx
        mov es, dx
        mov dword [es:0], 12345678h
        mov ecx, 1
        xor eax, eax
        int 31h
        jc bad
        mov [collision], ax
        cmp ax, [tail]
        jne bad
        mov ebx, 1001h
        mov dx, [head]
        mov eax, 102h
        int 31h
        jnc bad
        cmp ax, 8
        jne bad
        cmp bx, 1000h
        jne bad
        mov bx, [head]
        lsl eax, ebx
        jnz bad
        cmp eax, 0FFFFh
        jne bad
        cmp dword [es:0], 12345678h
        jne bad
        mov bx, [collision]
        mov eax, 1
        int 31h
        jc bad
        mov word [collision], 0
        mov byte [step], 5
        mov dx, [head]
        mov ebx, 1001h
        mov eax, 102h
        int 31h
        jc bad
        mov bx, [head]
        lsl eax, ebx
        jnz bad
        cmp eax, 1000Fh          ;S9.3 successful grow preserves full head
        jne bad
        mov bx, [tail]
        lsl eax, ebx
        jnz bad
        cmp eax, 0Fh
        jne bad
        mov es, bx
        mov ebx, 10h
        mov dx, [head]
        mov eax, 102h
        int 31h
        jc bad
        mov ax, es
        test ax, ax
        jnz bad
        mov bx, [head]
        lsl eax, ebx
        jnz bad
        cmp eax, 0FFh
        jne bad
        mov es, bx
        cmp dword [es:0], 12345678h
        jne bad
        mov byte [step], 6
        mov dx, [head]
        mov ebx, 1001h
        mov eax, 102h
        int 31h
        jc bad
        mov dx, [head]
        mov eax, 101h
        int 31h
        jc bad
        mov word [head], 0
        mov bx, [tail]
        lsl eax, ebx
        jz bad
        mov byte [step], 7
        mov ebx, 1000h
        mov eax, 100h
        int 31h
        jc bad
        mov [head], dx
        mov bx, dx
        add bx, 8
        mov [tail], bx
        mov es, dx
        mov dword [es:0], 12345678h
        mov ecx, 16
        xor eax, eax
        int 31h
        jc bad
        mov [held], ax
        mov ebx, 10h
        mov eax, 100h
        int 31h
        jc bad
        mov [blockb], dx
        mov es, dx
        mov dword [es:0], 76543210h
        mov bx, [held]
        mov esi, 16
        call free_held
        mov dx, [head]
        mov ebx, 0FFFFh         ;guaranteed physical failure, slots available
        mov eax, 102h
        int 31h
        jnc bad
        cmp ax, 8
        jne bad
        cmp bx, 1000h
        jb bad
        cmp bx, 0FFFFh
        je bad
        mov bx, [head]
        lsl eax, ebx
        jnz bad
        cmp eax, 0FFFFh
        jne bad
        mov es, bx
        cmp dword [es:0], 12345678h
        jne bad
        mov es, [blockb]
        cmp dword [es:0], 76543210h
        jne bad
        mov ecx, 15
        xor eax, eax
        int 31h
        jc bad
        cmp ax, [tail]          ;every provisional descriptor was rolled back
        jne bad
        mov bx, ax
        mov esi, 15
        call free_held
        mov dx, [blockb]
        mov eax, 101h
        int 31h
        jc bad
        ; Preserve the final real segment for the independent alias probe.
        mov bx, [head]
        mov eax, 6
        int 31h
        jc bad
        shl ecx, 16
        mov cx, dx
        shr ecx, 4
        mov [rmseg], cx
        ; 0002h must not return the DOS head/follower as its cached alias.
        mov bx, [rmseg]
        mov eax, 2
        int 31h
        jc bad
        cmp ax, [head]
        je bad
        mov [alias], ax
        mov bx, ax
        lsl eax, ebx
        jnz bad
        cmp eax, 0FFFFh
        jne bad
        mov eax, 101h            ;a real-segment alias is not a DOS block
        mov dx, bx
        int 31h
        jnc bad
        cmp ax, 9
        jne bad
        mov dx, [head]
        mov eax, 101h
        int 31h
        jc bad
        mov word [head], 0
        mov bx, [alias]
        lsl eax, ebx             ;freeing the DOS head keeps the alias alive
        jnz bad
        cmp eax, 0FFFFh
        jne bad
        mov si, ok
        xor bx, bx
        jmp short print
free_held:
        mov eax, 1
        int 31h
        jc bad
        add bx, 8
        dec esi
        jnz free_held
        ret
bad:    mov dl, [step]
        add dl, '0'
        mov ah, 2
        int 21h
        mov si, fail
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
head: dw 0
alias: dw 0
tail: dw 0
rmseg: dw 0
oldss: dw 0
oldsp: dd 0
held: dw 0
blockb: dw 0
collision: dw 0
guard: db 0
error: dw 0
step: db 0
%if CLIENT32
ok: db 'NDOS32 DOS memory arrays: ok',13,10,0
%else
ok: db 'NDOS16 DOS memory arrays: ok',13,10,0
%endif
fail: db ' DOS memory arrays: FAIL',13,10,0
        times 1024 db 0
stacktop:
