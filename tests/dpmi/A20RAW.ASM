; Real-mode harness forces a physically disabled A20 before native startup.
bits 16
org 100h
start:  cld
        mov sp, stacktop
        push cs
        pop ds
        push cs
        pop es
        mov bx, (stacktop-$$+100h+15)/16
        mov ah, 4Ah
        int 21h
        jc bad
        mov [block+4], cs
        mov [block+8], cs
        mov [block+12], cs
        in al, 92h
        and al, ~1
        mov [port92], al
        and al, ~2
        out 92h, al
        call a20query
        test ax, ax
        jnz .restore_bad
        call execute
        jc .restore_bad
        call a20query
        test ax, ax
        jnz .restore_bad
        mov word [block+2], tail2
        call execute
        jc .restore_bad
        call a20query
        test ax, ax
        jnz .restore_bad
        mov al, [port92]
        out 92h, al
        mov si, ok
        xor bx, bx
        jmp print
.restore_bad:
        mov al, [port92]
        out 92h, al
        jmp bad
execute:
        push cs
        pop es
        mov bx, block
        mov dx, host
        mov ax, 4B00h
        int 21h
        push cs
        pop ds
        jc .done
        mov ah, 4Dh
        int 21h
        test ax, ax
        jz .done
        stc
.done:  ret
%include "A20PROBE.INC"
bad:    mov si, fail
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
port92: db 0
host: db 'ZRXDPMI.EXE',0
tail: db 10,' A20RM.COM',13
tail2: db 11,' NATL16.COM',13
block: dw 0,tail,0,5Ch,0,6Ch,0
ok: db 'A20R normal-off cleanup: ok',13,10,0
fail: db 'A20R normal-off cleanup: FAIL',13,10,0
        times 1024 db 0
stacktop:
