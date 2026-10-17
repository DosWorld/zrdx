; DPMI 0.9 S8.9/S8.12 reject present execute-only/conforming code.
; Absent descriptors relax these type restrictions (the explicit 0.9 errata).
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 0
%endif
%macro SAVE 3
        mov bx, %2
        mov eax, %1
        int 31h
        jc bad
        mov [%3], edx
        mov [%3+4], cx
%endmacro
%macro SET 3
        mov cx, [selector]
        mov edx, %3
        mov bx, %2
        mov eax, %1
        int 31h
        jc bad
%endmacro
%macro RESTORE 3
        mov edx, [%3]
        mov cx, [%3+4]
        mov bx, %2
        mov eax, %1
        int 31h
        jc bad
%endmacro
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
        SAVE 204h, 60h, old60
        SAVE 204h, 0, old0
        SAVE 202h, 3, old3
        SAVE 202h, 13, old13
        mov bx, cs
        mov edi, descriptor
        mov eax, 0Bh
        int 31h
        jc bad
        xor eax, eax
        mov ecx, 1
        int 31h
        jc bad
        mov [selector], ax
        mov [target+2], ax
        xor esi, esi
.next:  mov byte [step], 1
        mov al, [rights+si]
        mov [descriptor+5], al
        mov bx, [selector]
        mov edi, descriptor
        mov eax, 0Ch
        int 31h
        jnc bad
        mov byte [step], 2
        movzx ecx, byte [rights+si]
        mov eax, 9
        int 31h
        jnc bad
        mov byte [step], 3
        and byte [descriptor+5], 7Fh
        mov eax, 0Ch
        int 31h
        jc bad
        mov byte [descriptor+5], 0FAh
        mov eax, 0Ch
        int 31h
        jc bad
        mov byte [step], 4
        SET 205h, 60h, software
        SET 205h, 0, software
        SET 203h, 3, exception
        SET 203h, 13, general
        mov byte [step], 5
        int 60h
        int3
        call far [target]
        cmp byte [hits], 3
        jne bad
        mov byte [hits], 0
        inc si
        cmp si, 3
        jb .next
        RESTORE 205h, 60h, old60
        RESTORE 205h, 0, old0
        RESTORE 203h, 3, old3
        RESTORE 203h, 13, old13
        mov bx, [selector]
        mov eax, 1
        int 31h
        jc bad
        mov si, ok
        xor bx, bx
        jmp print
under_alias:
        int 0                   ;#GP software-INT decoder dispatches through the new alias
        retf
software:
        inc byte [hits]
%if CLIENT32
        iretd
%else
        iret
%endif
exception:
        inc byte [hits]
%if CLIENT32
        o32 retf
%else
        retf
%endif
general:
        inc byte [hits]
        push ebx
        mov bx, sp
%if CLIENT32
        add dword [ss:bx+16], 3
%else
        add word [ss:bx+10], 3
%endif
        pop ebx
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
target: dw under_alias, 0
selector: dw 0
old60: times 6 db 0
old0: times 6 db 0
old3: times 6 db 0
old13: times 6 db 0
descriptor: times 8 db 0
rights: db 0F8h, 0FEh, 0FCh
hits: db 0
step: db 0
%if CLIENT32
ok: db 'COD32 code selector classes: ok',13,10,0
%else
ok: db 'COD16 code selector classes: ok',13,10,0
%endif
fail: db ' code selector classes: FAIL',13,10,0
        times 2048 db 0
stacktop:
