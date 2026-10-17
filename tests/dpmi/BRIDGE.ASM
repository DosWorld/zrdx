; Native 16/32-bit DPMI contract fixture. No intermediate execution.
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
        mov [rmseg], cs
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
        mov ax, ds
        mov fs, ax
        mov gs, ax
        mov [pmds], ax
        mov byte [step], 1
        mov bx, 60h
        mov eax, 200h
        int 31h
        jc bad
        mov [oldrm], dx
        mov [oldrm+2], cx
        mov cx, [rmseg]
        mov dx, server
        mov eax, 201h
        int 31h
        jc bad
        mov ebx, 80h
        mov eax, 100h
        int 31h
        jc bad
        mov [dosseg], ax
        mov [dossel], dx
.loop:  mov ax, [rmseg]
        mov [regs+34], ax
        mov [regs+36], ax
        mov [regs+38], ax
        mov [regs+40], ax
        mov word [regs+32], 201h ;CF supplied to real target, IF set
        mov word [regs+42], server
        mov [regs+44], ax
        cmp byte [mode], 0
        jne .target
        mov dword [regs+42], 0BEEFDEADh ;0300h ignores packet CS:IP
.target:mov dword [regs+46], 0
        cmp byte [mode], 1
        jne .default_stack
        mov word [regs+46], 800h
        mov ax, [dosseg]
        mov [regs+48], ax
.default_stack:
        mov esi, regs+42
        mov edi, controls
        mov ecx, 2
        cld
        rep movsd                ;all four control words must stay unchanged
        push word 1234h
        push word 5678h
        push word 9ABCh
        mov byte [step], 2
        mov ebx, 12340160h        ;BH reset flag, BL interrupt vector
        mov ecx, 23450003h        ;three words, preserving the high word
        mov edx, 3456789Ah
        mov esi, 456789ABh
        mov edi, regs
%if !CLIENT32
        or edi, 0DEAD0000h
%endif
        mov ebp, 6789ABCDh
        mov eax, 0BAAD0300h
        mov al, [mode]
        mov [function], eax
        std
        pushfd
        pop dword [beforeflags]
        int 31h
        jc bad
        pushfd
        pop dword [returned]
        cld
        cmp eax, [function]
        jne bad
        cmp ebx, 12340160h
        jne bad
        cmp ecx, 23450003h
        jne bad
        cmp edx, 3456789Ah
        jne bad
        cmp esi, 456789ABh
        jne bad
%if CLIENT32
        cmp edi, regs
%else
        cmp edi, 0DEAD0000h + regs
%endif
        jne bad
        cmp ebp, 6789ABCDh
        jne bad
        mov eax, [returned]
        xor eax, [beforeflags]
        test eax, 600h
        jnz bad
        mov ax, ds
        cmp ax, [cs:pmds]
        jne bad
        mov ax, es
        cmp ax, [cs:pmds]
        jne bad
        mov ax, fs
        cmp ax, [cs:pmds]
        jne bad
        mov ax, gs
        cmp ax, [cs:pmds]
        jne bad
        pop ax
        cmp ax, 9ABCh
        jne bad
        pop ax
        cmp ax, 5678h
        jne bad
        pop ax
        cmp ax, 1234h
        jne bad
        mov byte [step], 3
        cmp byte [broken], 0
        jne bad
        cmp dword [regs+28], 12345678h
        jne bad
        cmp dword [regs+16], 23456789h
        jne bad
        cmp dword [regs+24], 3456789Ah
        jne bad
        cmp dword [regs+20], 456789ABh
        jne bad
        cmp dword [regs+4], 56789ABCh
        jne bad
        cmp dword [regs], 6789ABCDh
        jne bad
        cmp dword [regs+8], 789ABCDEh
        jne bad
        test byte [regs+32], 1
        jz bad
        mov si, regs+42
        mov di, controls
        mov cx, 4
        repe cmpsw
        jne bad
        inc byte [mode]
        cmp byte [mode], 3
        jb .loop
        mov dx, [dossel]
        mov eax, 101h
        int 31h
        jc bad
        mov bx, 60h
        mov dx, [oldrm]
        mov cx, [oldrm+2]
        mov eax, 201h
        int 31h
        jc bad
        mov si, ok
        xor bx, bx
        jmp print
server:
        push bp
        mov bp, sp
        push si
        mov si, 8                ;IRET return frame plus saved BP
        cmp byte [cs:mode], 1
        jne .params
        mov si, 6                ;FAR return frame plus saved BP
.params:
        cmp word [ss:bp+si], 9ABCh
        jne .bad
        cmp word [ss:bp+si+2], 5678h
        jne .bad
        cmp word [ss:bp+si+4], 1234h
        jne .bad
        jmp .values
.bad:   mov byte [cs:broken], 1
.values:
        pop si
        cmp byte [cs:mode], 1
        je .far
        or word [ss:bp+6], 1
        pop bp
        call values
        iret
.far:   pop bp
        call values
        stc
        retf
values: mov eax, 12345678h
        mov ebx, 23456789h
        mov ecx, 3456789Ah
        mov edx, 456789ABh
        mov esi, 56789ABCh
        mov edi, 6789ABCDh
        mov ebp, 789ABCDEh
        ret
bad:    cld
        mov dl, [step]
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
pmds: dw 0
beforeflags: dd 0
step: db 0
rmseg: dw 0
oldrm: dd 0
dosseg: dw 0
dossel: dw 0
mode: db 0
broken: db 0
function: dd 0
returned: dd 0
controls: times 8 db 0
regs: times 50 db 0
%if CLIENT32
ok: db 'BRG32 real mode bridges: ok',13,10,0
%else
ok: db 'BRG16 real mode bridges: ok',13,10,0
%endif
fail: db ' real mode bridges: FAIL',13,10,0
        times 2048 db 0
stacktop:
