; S11.4 permits an alternate return packet and requires at least 16 slots.
; ZRDX policy: exactly 20 slots, reusable after exhaustion and invalid frees.
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
.cycle: mov word [count], 0
        mov byte [hits], 0
.alloc: push ds
        push cs
        pop ds
        mov esi, handler
        mov edi, cbpacket
        mov ecx, 12345678h
        mov edx, 23456789h
        mov eax, 303h
        int 31h
        pop ds
        jc .full
        mov bx, [count]
        cmp bx, 20
        jae bad
        shl bx, 2
        mov [slots+bx], dx
        mov [slots+bx+2], cx
        inc word [count]
        jmp .alloc
.full:  cmp ecx, 12345678h
        jne bad
        cmp edx, 23456789h
        jne bad
        cmp word [count], 16     ;normative minimum, exact count below is policy
        jb bad
        cmp word [count], 20
        jne bad
        mov byte [step], 2
        xor esi, esi
.invoke:
        mov eax, [slots+esi*4]
        mov [rmregs+42], eax
        mov dword [rmregs+46], 0
        xor ebx, ebx
        xor ecx, ecx
        mov edi, rmregs
        mov eax, 301h
        int 31h
        jc bad
        cmp dword [rmregs+28], 13579BDFh
        jne bad
        cmp dword [cbpacket+50], 0A55AA55Ah
        jne bad
        cmp dword [alternate+50], 05AA55AA5h
        jne bad
        inc si
        cmp si, 20
        jb .invoke
        cmp byte [hits], 20
        jne bad
        mov byte [step], 3
.free:  dec word [count]
        mov bx, [count]
        shl bx, 2
        mov dx, [slots+bx]
        mov cx, [slots+bx+2]
        mov eax, 304h
        int 31h
        jc bad
        mov eax, 304h
        int 31h
        jnc bad
        cmp word [count], 0
        jne .free
        inc dx
        mov eax, 304h
        int 31h
        jnc bad
        inc cx
        mov eax, 304h
        int 31h
        jnc bad
        inc byte [round]
        cmp byte [round], 16
        jb .cycle
        mov si, ok
        xor bx, bx
        jmp print
handler:
%if CLIENT32
        mov ax, [esi]
        mov [es:edi+42], ax
        mov ax, [esi+2]
        mov [es:edi+44], ax
        add word [es:edi+46], 4
        call copy_packet
        inc byte [es:hits]
        iretd
%else
        mov ax, [si]
        mov [es:di+42], ax
        mov ax, [si+2]
        mov [es:di+44], ax
        add word [es:di+46], 4
        call copy_packet
        inc byte [es:hits]
        iret
%endif
copy_packet:
%if !CLIENT32
        movzx edi, di           ;S1 ignores high pointer words for word clients
%endif
        ; Callback entry registers are undefined except specified pointers.
        ; Copy through ES so the real-stack alias in DS is never reused.
        xor ebx, ebx
.copy:  mov al, [es:edi+ebx]
        mov [es:alternate+ebx], al
        inc bx
        cmp bx, 50
        jb .copy
        mov dword [es:edi+28], 0DEADBEEFh
        mov edi, alternate
        mov dword [es:edi+28], 13579BDFh
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
count: dw 0
round: db 0
hits: db 0
step: db 0
slots: times 20 dd 0
cbpacket: times 50 db 0
        dd 0A55AA55Ah
alternate: times 50 db 0
        dd 05AA55AA5h
rmregs: times 50 db 0
%if CLIENT32
ok: db 'CBK32 all callback slots: ok',13,10,0
%else
ok: db 'CBK16 all callback slots: ok',13,10,0
%endif
fail: db ' callback slots: FAIL',13,10,0
        times 2048 db 0
stacktop:
