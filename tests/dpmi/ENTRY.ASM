bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 0
%endif
start:
        cld
        push cs
        pop ds
        push cs
        pop es
        mov sp, stacktop
        mov bx, (stacktop - $$ + 100h + 15) / 16
        mov ah, 4ah
        int 21h
        jc badrm
        mov ax, 1687h
        int 2fh
        test ax, ax
        jnz badrm
        cmp cl, 3
        jb badrm
        cmp dx, 005ah
        jb badrm
%if CLIENT32
        test bx, 1
        jz badrm
%endif
        mov [entry], di
        mov [entry + 2], es
        test si, si
        jz .private
        mov bx, si
        mov ah, 48h
        int 21h
        jc badrm
        mov es, ax
.private:
        mov ebx, 12345678h
        mov ecx, 23456789h
        mov edx, 3456789ah
        mov esi, 456789abh
        mov edi, 56789abch
        mov ebp, 6789abcdh
        mov ax, CLIENT32
        call far [entry]
        jc badrm
        mov byte [step], 1
        cmp ebx, 12345678h
        jne badpm
        cmp ecx, 23456789h
        jne badpm
        cmp edx, 3456789ah
        jne badpm
        cmp esi, 456789abh
        jne badpm
        cmp edi, 56789abch
        jne badpm
        cmp ebp, 6789abcdh
        jne badpm
        mov byte [step], 2
        mov ax, fs
        test ax, ax
        jnz badpm
        mov ax, gs
        test ax, ax
        jnz badpm
        mov byte [step], 3
        mov ax, ss
        mov bx, ds
        cmp ax, bx
        jne badpm
        lar eax, eax
        jnz badpm
        shr eax, 22
        and al, 1
        cmp al, CLIENT32
        jne badpm
        mov byte [step], 4
        mov ax, cs
        lar eax, eax
        test eax, 400000h
        jnz badpm
        mov byte [step], 5
        mov bx, es
        lsl eax, ebx
        jnz badpm
        cmp eax, 0ffh
        jne badpm
        mov byte [step], 6
        mov bx, [es:2ch]
        verr bx
        jnz badpm
        mov byte [step], 7
        push ds
        pop es
        mov bx, ds
        mov edi, buffer
%if !CLIENT32
        or edi, 0dead0000h
%endif
        mov ax, 000bh
        int 31h
        jc badpm
        and byte [buffer + 5], 0feh
        cmp byte [buffer + 5], 0f2h
        jne badpm
        mov byte [step], 8
        mov ax, 1686h
        int 2fh
        test ax, ax
        jnz badpm
        mov byte [step], 9
        mov bx, 31h
        mov edx, 0DEAD0000h
        mov ax, 204h
        int 31h
        jc badpm
%if CLIENT32
        mov [api31], edx
        mov [api31+4], cx
%else
        mov [api31], dx
        mov [api31+2], cx
        shr edx, 16
        cmp dx, 0DEADh
        jne badpm
%endif
        mov [callsp], esp
        mov eax, 604h
%if CLIENT32
        pushfd
        call dword far [api31]
%else
        pushf
        call far [api31]
%endif
        jc badpm
        test bx, bx
        jnz badpm
        cmp cx, 1000h
        jne badpm
%if CLIENT32
        cmp esp, [callsp]
%else
        cmp sp, [callsp]
%endif
        jne badpm
        mov dx, ok
        call print
        mov ax, 4c00h
        int 21h
badpm:  mov dl, [step]
        add dl, 48
        mov ah, 2
        int 21h
        mov dx, failpm
        jmp short fail
badrm:  mov dx, failrm
fail:   mov ah, 9
        int 21h
        mov ax, 4c01h
        int 21h
print:  pushad
        movzx esi, dx
.next:  lodsb
        cmp al, '$'
        je .done
        mov dl, al
        pushad
        mov ah, 2
        int 21h
        popad
        jmp .next
.done:  popad
        ret
step:   db 0
api31: dd 0
       dw 0
callsp: dd 0
entry:  dd 0
buffer: times 8 db 0
%if CLIENT32
ok:     db 'ENTRY32 native initialization: ok', 13, 10, '$'
%else
ok:     db 'ENTRY16 native initialization: ok', 13, 10, '$'
%endif
failrm: db 'ENTRY real-mode discovery/entry: FAIL', 13, 10, '$'
failpm: db 'ENTRY protected-mode contract: FAIL', 13, 10, '$'
        times 1024 db 0
stacktop:
