; Keep one VM alive while successive real-mode children request a mode.
bits 16
org 100h
%ifndef FIRST32
%define FIRST32 0
%endif
start:  cld
        mov sp, stacktop
        mov bx, (stacktop - $$ + 100h + 15) / 16
        mov ah, 4Ah
        int 21h
        jc bad
        mov ax, ds
        mov [block+4], ax
        mov [block+8], ax
        mov [block+12], ax
        mov dx, child16
        call run
        mov byte [tail], 1
        mov dx, child32
        call run
        mov byte [tail], 0
        mov dx, child16
        call run
        mov dx, ok
        mov ah, 9
        int 21h
        mov ax, 4C00h
        int 21h
run:    inc byte [which]
        push cs
        pop es
        mov bx, block
        mov ax, 4B00h
        int 21h
        push cs
        pop ds
        jc bad
        mov ah, 4Dh
        int 21h
        mov [error], al
        test ax, ax
        jnz bad
        ret
bad:    push cs
        pop ds
        mov dl, [which]
        add dl, '0'
        mov ah, 2
        int 21h
        mov dl, '/'
        mov ah, 2
        int 21h
        mov dl, [error]
        add dl, '0'
        mov ah, 2
        int 21h
        mov dx, fail
        mov ah, 9
        int 21h
        mov ax, 4C01h
        int 21h
%if FIRST32
child16: db 'MODE32.COM',0
child32: db 'MODE16.COM',0
%else
child16: db 'MODE16.COM',0
child32: db 'MODE32.COM',0
%endif
which: db 0
error: db 0
block: dw 0, tail, 0, 5Ch, 0, 6Ch, 0
tail: db 0, 'x', 13
%if FIRST32
%define ModeName 'NATM32'
%else
%define ModeName 'NATMODE'
%endif
ok: db ModeName, ' fixed VM bitness: ok',13,10,'$'
fail: db ModeName, ' fixed VM bitness: FAIL',13,10,'$'
        times 512 db 0
stacktop:
