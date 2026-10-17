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
        mov ecx, 1
        xor eax, eax
        int 31h
        jc bad
        mov [selector], ax
        mov bx, ax
        mov si, limits
.next:  mov ecx, [si]
        mov edx, ecx
        shr ecx, 16
%ifndef PORTABLE
        or edx, 0CAFE0000h
        or ecx, 0BEEF0000h
%endif
        mov [savedx], edx
        mov eax, 8
        mov byte [reason], 1
        int 31h
        jc bad
        mov byte [reason], 2
        cmp edx, [savedx]
        jne bad
        mov byte [reason], 3
        lsl eax, ebx
        jnz bad
        mov byte [reason], 4
        cmp eax, [si]
        jne bad
        add si, 4
        cmp si, limitsend
        jb .next
%ifndef PORTABLE
        mov byte [step], 2
        mov ecx, 10h
        xor edx, edx             ;100000h cannot be expressed exactly
        mov eax, 8
        int 31h
        jnc bad
        cmp ax, 8021h
        jne bad
        lsl eax, ebx
        jnz bad
        cmp eax, 0FFFFFFFFh
        jne bad
%endif
        mov byte [step], 3
        mov edi, descriptor
%if !CLIENT32
        or edi, 0DEAD0000h
%endif
        mov eax, 0Bh
        int 31h
        jc bad
        mov word [descriptor], 0ABCDh
        and byte [descriptor+6], 70h
        mov eax, 0Ch
        int 31h
        jc bad
        lsl eax, ebx
        jnz bad
        cmp eax, 0ABCDh
        jne bad
        mov ecx, 12h
        mov edx, 3450h
        mov eax, 7
        int 31h
        jc bad
        mov eax, 6
        int 31h
        jc bad
        cmp cx, 12h
        jne bad
        cmp dx, 3450h
        jne bad
        mov eax, 0Ah            ;the 0.9 errata explicitly permit data aliases
        int 31h
        jc bad
        push bx
        mov bx, ax
        lsl eax, ebx
        jnz bad
        cmp eax, 0ABCDh
        jne bad
        mov eax, 1
        int 31h
        jc bad
        pop bx
%ifndef PORTABLE
        mov byte [step], 4
        mov cx, 0010h            ;absent, DPL0 is invalid
        mov eax, 9
        int 31h
        jnc bad
        mov cx, 0070h            ;absent DPL3/S=1: type restrictions relaxed
        mov eax, 9
        int 31h
        jc bad
        mov cx, 2070h            ;extended reserved bit is invalid even P=0
        mov eax, 9
        int 31h
        jnc bad
        mov cx, 40F2h
        mov eax, 9
        int 31h
        jc bad
        mov eax, 1
        int 31h
        jc bad
        mov byte [step], 5
        mov bx, 4                ;reserved LDT entry 0, ignoring RPL
        mov eax, 0Dh
        int 31h
        jc bad
        mov eax, 0Dh
        int 31h
        jnc bad
        mov eax, 1
        int 31h
        jc bad
        xor ecx, ecx
        xor eax, eax
        int 31h
        jnc bad
%else
        mov cx, 40F2h
        mov eax, 9
        int 31h
        jc bad
        mov eax, 1
        int 31h
        jc bad
%endif
        mov si, ok
        xor bx, bx
        jmp print
bad:
%ifdef REFLOG
        pushad
        mov ebx, eax
        call refhex
        mov ebx, [si]
        call refhex
        popad
%endif
        mov dl, [reason]
        add dl, '0'
        mov ah, 2
        int 21h
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
%ifdef REFLOG
refhex: mov ecx, 8
.hex:   rol ebx, 4
        mov dl, bl
        and dl, 15
        add dl, '0'
        cmp dl, '9'
        jbe .put
        add dl, 7
.put:   mov ah, 2
        int 21h
        loop .hex
        mov dl, '/'
        mov ah, 2
        int 21h
        ret
%endif
reason: db 0
step: db 0
descriptor: times 8 db 0
selector: dw 0
savedx: dd 0
limits: dd 0FFFFh, 0FFFFFh, 100FFFh, 12345FFFh, 0FFFFFFFFh
limitsend:
%if CLIENT32
ok: db 'LIM32 descriptor limits and rights: ok',13,10,0
%else
ok: db 'LIM16 descriptor limits and rights: ok',13,10,0
%endif
fail: db ' descriptor limits and rights: FAIL',13,10,0
        times 1024 db 0
stacktop:
