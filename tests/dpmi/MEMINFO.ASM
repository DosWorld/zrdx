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
        mov ax, 1Bh
        mov fs, ax
        mov byte [step], 5
        call shapes
        mov byte [step], 1
        call report
        mov eax, [info]
        test eax, eax
        jz bad
        test eax, 0FFFh
        jnz bad
        mov [largest], eax
        inc eax
        mov ecx, eax
        mov ebx, eax
        shr ebx, 16
        mov eax, 501h
        int 31h
        jnc bad                  ;one byte beyond the reported maximum
        mov byte [step], 2
        call report
        mov ecx, [largest]
        cmp ecx, [info]
        jne bad                  ;failed allocation must not consume pages
        mov ebx, ecx
        shr ebx, 16
        mov eax, 501h
        int 31h
        jc bad                   ;the reported maximum must be allocatable
        mov [handle], di
        mov [handle+2], si
        shl ebx, 16
        mov bx, cx
        mov ax, 1Bh
        mov fs, ax
        mov byte [fs:ebx], 5Ah
        add ebx, [largest]
        dec ebx
        mov byte [fs:ebx], 0A5h
        mov byte [step], 3
        call report
        cmp dword [info], 0
        jne bad
        mov di, [handle]
        mov si, [handle+2]
        mov eax, 502h
        int 31h
        jc bad
        mov byte [step], 4
        call report
        mov eax, [info]
        cmp eax, [largest]
        jne bad
        xor ax, ax
        mov fs, ax
        mov si, ok
        xor bx, bx
        jmp print
; Force a 0503h move past an adjacent live block, then shrink and free.
shapes: mov ebx, 0
        mov ecx, 1000h
        mov eax, 501h
        int 31h
        jc bad
        mov [blocka], di
        mov [blocka+2], si
        shl ebx, 16
        mov bx, cx
        mov dword [fs:ebx], 12345678h
        mov dword [fs:ebx+0FFCh], 87654321h
        xor ebx, ebx
        mov ecx, 1000h
        mov eax, 501h
        int 31h
        jc bad
        mov [blockb], di
        mov [blockb+2], si
        mov di, [blocka]
        mov si, [blocka+2]
        xor ebx, ebx
        mov ecx, 2000h
        mov eax, 503h
        int 31h
        jc bad
        mov [blocka], di
        mov [blocka+2], si
        shl ebx, 16
        mov bx, cx
        cmp dword [fs:ebx], 12345678h
        jne bad
        cmp dword [fs:ebx+0FFCh], 87654321h
        jne bad
        xor ebx, ebx
        mov ecx, 1000h
        mov eax, 503h
        int 31h
        jc bad
        mov eax, 502h
        int 31h
        jc bad
        mov di, [blockb]
        mov si, [blockb+2]
        mov eax, 502h
        int 31h
        jc bad
        mov eax, 502h
        int 31h
        jnc bad
        ret
report:
%if CLIENT32
        mov edi, info
%else
        mov edi, 0DEAD0000h + info
%endif
        mov eax, 500h
        int 31h
        jc bad
        cmp dword [guard0], 12345678h
        jne bad
        cmp dword [guard1], 87654321h
        jne bad
        mov si, info+4
        mov cx, 11
.check: cmp dword [si], 0FFFFFFFFh
        jne bad                  ;all unsupported fields are -1
        add si, 4
        loop .check
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
step: db 0
blocka: dd 0
blockb: dd 0
largest: dd 0
handle: dd 0
guard0: dd 12345678h
info: times 48 db 0
guard1: dd 87654321h
%if CLIENT32
ok: db 'MEM32 exact free memory: ok',13,10,0
%else
ok: db 'MEM16 exact free memory: ok',13,10,0
%endif
fail: db ' exact free memory: FAIL',13,10,0
        times 1024 db 0
stacktop:
