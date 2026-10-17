; Real-mode XMS proxy: host A20 references must balance across child runs.
bits 16
org 100h
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
        mov ax, 4300h
        int 2Fh
        cmp al, 80h
        jne bad
        mov ax, 4310h
        int 2Fh
        mov [xms], bx
        mov [xms+2], es
        mov ah, 5              ;an upstream reference must survive the host
        call far [xms]
        cmp ax, 1
        jne bad
        mov ax, 352Fh
        int 21h
        mov [old2f], bx
        mov [old2f+2], es
        mov dx, hook2f
        mov ax, 252Fh
        int 21h
        mov word [block+4], cs
        mov word [block+8], cs
        mov word [block+12], cs
        call execute
        cmp byte [failed], 0
        jne .restore
        mov word [block+2], tail2
        call execute
.restore:
        lds dx, [old2f]
        mov ax, 252Fh
        int 21h
        push cs
        pop ds
        mov ah, 7
        call far [xms]
        cmp ax, 1
        je .stateok
        mov byte [failed], 2
.stateok:
        mov ax, [enables]
        test ax, ax
        jz .unbalanced
        cmp ax, [disables]
        je .release
.unbalanced:
        mov byte [failed], 3
.release:
        mov ah, 6
        call far [xms]
        test ax, ax
        jnz .released
        cmp bl, 94h             ;reference released; another owner keeps A20 on
        jne bad
.released:
        cmp byte [failed], 0
        jne bad
        mov si, ok
        xor bx, bx
        jmp print
execute:
        push cs
        pop es
        mov bx, block
        mov dx, host
        mov ax, 4B00h
        int 21h
        push cs
        pop ds
        jc .bad
        mov ah, 4Dh
        int 21h
        test ax, ax
        jnz .bad
        mov ax, [enables]
        cmp ax, [disables]
        je .done
.bad:   mov byte [failed], 1
.done:  ret
hook2f:
        pushf
        cmp ax, 4310h
        jne .chain
        popf
        mov bx, spy
        push cs
        pop es
        iret
.chain: popf
        jmp far [cs:old2f]
spy:    pushf
        cmp ah, 5
        je .enable
        cmp ah, 6
        jne .done
        inc word [cs:disables]
        jmp .done
.enable:
        inc word [cs:enables]
.done:  popf
        jmp far [cs:xms]
bad:    mov dl, [failed]
        add dl, '0'
        mov ah, 2
        int 21h
        mov ax, [enables]
        call hexword
        mov ax, [disables]
        call hexword
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
hexword:
        pushad
        mov bx, ax
        mov cx, 4
.hex:   rol bx, 4
        mov dl, bl
        and dl, 15
        add dl, '0'
        cmp dl, '9'
        jbe .digit
        add dl, 7
.digit: mov ah, 2
        int 21h
        loop .hex
        mov dl, ' '
        mov ah, 2
        int 21h
        popad
        ret
old2f: dd 0
xms: dd 0
enables: dw 0
disables: dw 0
failed: db 0
host: db 'ZRXDPMI.EXE',0
tail: db 10, ' BRG16.COM',13
tail2: db 11, ' NATL16.COM',13
block: dw 0, tail, 0, 5Ch, 0, 6Ch, 0
ok: db 'A20X paired host ownership: ok',13,10,0
fail: db 'A20X paired host ownership: FAIL',13,10,0
        times 1024 db 0
stacktop:
