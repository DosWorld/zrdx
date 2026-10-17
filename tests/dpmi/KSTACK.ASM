; Direct reads of both kernel stacks must raise user protection #PF.
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 0
%endif
start:  cld
        push cs
        pop ds
        push cs
        pop es
        mov sp, stacktop
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
        mov bx, 14
        mov eax, 202h
        int 31h
        jc bad
%if CLIENT32
        mov [oldexc], edx
        mov [oldexc+4], cx
%else
        mov [oldexc], dx
        mov [oldexc+2], cx
%endif
        mov cx, cs
        mov edx, handler
        mov eax, 203h
        int 31h
        jc bad
        mov ax, 1Bh              ;built-in public flat data selector
        mov fs, ax
        mov ebx, KSTACK0
        mov [target], ebx
probe0: mov al, [fs:ebx]
after0: mov ebx, KSTACK1
        mov [target], ebx
probe1: mov al, [fs:ebx]
after1: cmp byte [hits], 2
        jne bad
        cmp byte [broken], 0
        jne bad
        xor ax, ax
        mov fs, ax
%if CLIENT32
        mov edx, [oldexc]
        mov cx, [oldexc+4]
%else
        movzx edx, word [oldexc]
        mov cx, [oldexc+2]
%endif
        mov bx, 14
        mov eax, 203h
        int 31h
        jc bad
        mov si, ok
        xor bx, bx
        jmp print
handler:
        push bp
        mov bp, sp
        push eax
%if CLIENT32
%define ERR 10
%define IP 14
%else
%define ERR 6
%define IP 8
%endif
        cmp word [ss:bp+ERR], 5  ;present, user, read access
        jne .error
        mov eax, cr2
        cmp eax, [target]
        jne .broken
        inc byte [hits]
        jmp .resume
.error:
        mov byte [broken], 2
        jmp .resume
.broken:
        mov byte [broken], 1
.resume:
%if CLIENT32
        add dword [ss:bp+IP], after0-probe0
%else
        add word [ss:bp+IP], after0-probe0
%endif
        pop eax
        pop bp
%if CLIENT32
        o32 retf
%else
        retf
%endif
bad:    mov dl, [hits]
        add dl, '0'
        mov ah, 2
        int 21h
        mov dl, [broken]
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
oldexc: times 6 db 0
target: dd 0
hits: db 0
broken: db 0
%if CLIENT32
ok: db 'KST32 private kernel stacks: ok',13,10,0
%else
ok: db 'KST16 private kernel stacks: ok',13,10,0
%endif
fail: db 'private kernel stacks: FAIL',13,10,0
        times 1024 db 0
stacktop:
