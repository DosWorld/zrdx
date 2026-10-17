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
        mov [oldcs], cs
        mov bx, 13
        mov cx, cs
        mov edx, fault
        mov eax, 203h
        int 31h
        jc bad
        mov bx, 6
        mov eax, 203h
        int 31h
        jc bad
        xor bx, bx
        mov edx, soft
        mov eax, 205h
        int 31h
        jc bad
%assign P 0
%rep 11
%if P = 1
        db 26h
%elif P = 2
        db 2Eh
%elif P = 3
        db 36h
%elif P = 4
        db 3Eh
%elif P = 5
        db 64h
%elif P = 6
        db 65h
%elif P = 7
        db 66h
%elif P = 8
        db 67h
%elif P = 9
        db 0F2h
%elif P = 10
        db 0F3h
%endif
        int 0
%assign P P+1
%endrep
        db 26h,66h,67h,0F3h
        int 0
        times 13 db 66h
        int 0                   ;exactly 15 bytes
        cmp byte [seen], 13
        jne bad
        mov word [resume], long_done
        times 14 db 66h
        int 0                   ;16 bytes: must remain a CPU #GP
long_done:
        mov word [resume], prefixes_done
        times 15 db 66h          ;no opcode within architectural limit
        int 0
prefixes_done:
        mov word [resume], lock_done
        db 0F0h
        int 0                   ;LOCK is not an ignored prefix
lock_done:
        cmp byte [faults], 3
        jne bad
        cmp byte [seen], 13
        jne bad
        mov bx, cs
        mov eax, 0Ah
        int 31h
        jc bad
        mov [alias], ax
        mov bx, ax
        mov cx, 0FAh
        mov eax, 9
        int 31h
        jc bad
        xor ecx, ecx
        mov edx, cut
        mov eax, 8
        int 31h
        jc bad
        mov word [resume], cut_done
        jmp far [target]
cut:    db 66h,0CDh,0
cut_done:
        cmp byte [faults], 4
        jne bad
        mov bx, [alias]
        mov eax, 1
        int 31h
        jc bad
        jmp passed
soft:   inc byte [seen]
%if CLIENT32
        iretd
%else
        iret
%endif
fault:  push bp
        mov bp, sp
        push ax
        inc byte [faults]
        mov ax, [resume]
%if CLIENT32
        mov [ss:bp+14], ax
        mov ax, [oldcs]
        mov [ss:bp+18], ax
%else
        mov [ss:bp+8], ax
        mov ax, [oldcs]
        mov [ss:bp+10], ax
%endif
        pop ax
        pop bp
%if CLIENT32
        o32 retf
%else
        retf
%endif
bad:    mov si, fail
        mov bx, 1
        jmp print
passed: mov si, ok
        xor bx, bx
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
seen: db 0
faults: db 0
resume: dw 0
oldcs: dw 0
target: dw cut
alias: dw 0
ok: db 'PREF ignored and invalid prefixes: ok',13,10,0
fail: db 'PREF: FAIL',13,10,0
        times 1024 db 0
stacktop:
