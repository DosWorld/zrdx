; Native raw switches and real/protected state procedures, both client ABIs.
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
        mov [rmseg], cs
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
        pop es                  ;0305h output buffer uses client ES, not PSP ES
        mov [pmcs], cs
        mov [pmds], ds
        mov [pmss], ss
%if CLIENT32
        mov [pmsp], esp
%else
        mov [pmsp], sp
%endif
        mov eax, 305h
        int 31h
        jc bad
        cmp ax, 16
        jne bad
        mov [rstate], cx
        mov [rstate+2], bx
        mov [pstate], edi
%if CLIENT32
        mov [pstate+4], si
%else
        mov [pstate+2], si
%endif
        mov edi, saved
        xor al, al
        std
%if CLIENT32
        call dword far [pstate]
%else
        or edi, 0DEAD0000h
        call far [pstate]
%endif
        pushf
        pop ax
        test ah, 4
        jz bad
        cld
        cmp dword [saved+16], 0A5A5A5A5h
        jne bad
        mov eax, 306h
        int 31h
        jc bad
        mov [rawrm], cx
        mov [rawrm+2], bx
%if CLIENT32
        mov [rawpm], edi
        mov [rawpm+4], si
%else
        mov [rawpm], di
        mov [rawpm+2], si
%endif
        mov ax, [pmds]
        mov fs, ax
        mov gs, ax
        mov ax, [rmseg]
        mov cx, ax
        mov dx, ax
        mov ebx, rmstack
        mov si, ax
        mov edi, real_target
        mov ebp, 1234ABCDh
%if CLIENT32
        jmp dword far [rawpm]
%else
        jmp far [rawpm]
%endif
real_target:
        cmp ebp, 1234ABCDh
        jne bad
        mov ax, ds
        cmp ax, [rmseg]
        jne bad
        mov ax, es
        cmp ax, [rmseg]
        jne bad
        mov ax, ss
        cmp ax, [rmseg]
        jne bad
        cmp sp, rmstack
        jne bad
        mov ax, fs
        test ax, ax
        jnz bad
        mov ax, gs
        test ax, ax
        jnz bad
        mov edi, rsave
        xor al, al
        mov esi, 0CAFE1234h
        std
        call far [rstate]
        pushf
        pop ax
        test ah, 4
        jz bad
        cld
        cmp esi, 0CAFE1234h
        jne bad
        cmp edi, rsave
        jne bad
        cmp dword [rsave+16], 0A5A5A5A5h
        jne bad
        mov al, 1
        call far [rstate]
        mov ax, [pmds]
        mov cx, ax
        mov dx, [pmss]
        mov ebx, [pmsp]
        mov si, [pmcs]
        mov edi, protected_target
%if !CLIENT32
        or ebx, 0DEAD0000h
        or edi, 0BEEF0000h
%endif
        mov ebp, 7654DCBAh
        jmp far [rawrm]
protected_target:
        cmp ebp, 7654DCBAh
        jne bad
        mov ax, ds
        cmp ax, [pmds]
        jne bad
        mov ax, es
        cmp ax, [pmds]
        jne bad
        mov ax, ss
        cmp ax, [pmss]
        jne bad
%if CLIENT32
        cmp esp, [pmsp]
%else
        cmp sp, [pmsp]
%endif
        jne bad
        mov ax, fs
        test ax, ax
        jnz bad
        mov ax, gs
        test ax, ax
        jnz bad
        mov edi, saved
        mov al, 1
%if CLIENT32
        call dword far [pstate]
%else
        call far [pstate]
%endif
        mov si, ok
        xor bx, bx
        jmp print
bad:    cld
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
rmseg: dw 0
pmcs: dw 0
pmds: dw 0
pmss: dw 0
pmsp: dd 0
rawrm: dd 0
rawpm: times 6 db 0
rstate: dd 0
pstate: times 6 db 0
saved: times 20 db 0A5h
rsave: times 20 db 0A5h
%if CLIENT32
ok: db 'RAW32 switches and state: ok',13,10,0
%else
ok: db 'RAW16 switches and state: ok',13,10,0
%endif
fail: db 'raw switches and state: FAIL',13,10,0
        times 1024 db 0
rmstack:
        times 1024 db 0
stacktop:
