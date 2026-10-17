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
        mov byte [step], 1
        mov bx, 13
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
        mov word [resume], gate_done
probe_gate: mov ax, [gate+2]
        and ax, ~3
        mov [expected], ax
        call far [gate]
gate_done:
        add word [gate+2], 8
        cmp word [gate+2], 6Bh
        jbe probe_gate
        mov byte [step], 2
        mov word [expected], 0
        mov word [resume], after_cr3
        xor eax, eax
        mov cr3, eax             ;must not replace the host's page tables
after_cr3:
        mov word [resume], after_dr7
        mov dr7, eax             ;must not bypass owned debug services
after_dr7:
        mov word [resume], after_esp
        mov esp, cr0             ;must not replace the host's own ESP
after_esp:
        cmp byte [hits], 7
        jne bad
        cmp byte [broken], 0
        jne bad
        pushfd
        pop eax
        test eax, 24000h
        jnz bad
        mov bx, 13
%if CLIENT32
        mov edx, [oldexc]
        mov cx, [oldexc+4]
%else
        movzx edx, word [oldexc]
        mov cx, [oldexc+2]
%endif
        mov eax, 203h
        int 31h
        jc bad
        mov si, ok
        xor bx, bx
        jmp print
handler:
        push bp
        mov bp, sp
        push ax
%if CLIENT32
%define ERR 10
%define IP 14
%else
%define ERR 6
%define IP 8
%endif
        mov ax, [ss:bp+ERR]
        cmp ax, [expected]
        je .valid
        mov byte [broken], 1
.valid: inc byte [hits]
        mov ax, [resume]
        mov [ss:bp+IP], ax
%if CLIENT32
        and word [ss:bp+IP+4], ~3
        and word [ss:bp+IP+16], ~3
        or dword [ss:bp+IP+8], 24000h
%else
        and word [ss:bp+IP+2], ~3
        and word [ss:bp+IP+8], ~3
        or word [ss:bp+IP+4], 4000h
        or dword [ss:bp+22], 20000h
%endif
        pop ax
        pop bp
%if CLIENT32
        o32 retf
%else
        retf
%endif
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
step: db 0
gate: dw 0, 53h
oldexc: times 6 db 0
expected: dw 0
resume: dw 0
hits: db 0
broken: db 0
%if CLIENT32
ok: db 'GAT32 privileged entry guards: ok',13,10,0
%else
ok: db 'GAT16 privileged entry guards: ok',13,10,0
%endif
fail: db ' privileged entry guards: FAIL',13,10,0
        times 1024 db 0
stacktop:
