; Real-mode upstream debugger state must survive complete host invocations.
; Uses one enabled watchpoint at an unmapped address; no trap is expected.
bits 16
org 100h
%macro READ 1
%assign ix 0
%rep 8
%if ix < 4 || ix > 5
        mov eax, dr %+ ix
        mov [%1+ix*4], eax
%endif
%assign ix ix+1
%endrep
%endmacro
%macro WRITE 1
        xor eax, eax
        mov dr7, eax
%assign ix 0
%rep 8
%if ix < 4 || ix > 5
        mov eax, [%1+ix*4]
        mov dr %+ ix, eax
%endif
%assign ix ix+1
%endrep
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
        pushf
        cli
        READ original
        WRITE requested
        READ expected
        mov byte [armed], 1
        popf
        mov word [block+4], cs
        mov word [block+8], cs
        mov word [block+12], cs
        call execute
        cmp byte [failed], 0
        jne restore
        mov word [block+2], tail2
        call execute
restore:
        pushf
        cli
        WRITE original
        mov byte [armed], 0
        popf
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
        pushf
        cli
        READ actual
        popf
%assign ix 0
%rep 8
%if ix < 4 || ix > 5
        mov eax, [actual+ix*4]
        cmp eax, [expected+ix*4]
        jne .bad
%endif
%assign ix ix+1
%endrep
        ret
.bad:   mov byte [failed], 1
        ret
bad:    cmp byte [armed], 0
        jne restore
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
requested: dd 0FFE00000h, 12345678h, 0ABCD0000h, 0DEADBEEFh, 0, 0
           dd 0FFFF0FF1h, 00010402h
original: times 8 dd 0
expected: times 8 dd 0
actual: times 8 dd 0
armed: db 0
failed: db 0
host: db 'ZRXDPMI.EXE',0
tail: db 10, ' BAS32.COM',13
tail2: db 11, ' NEST32.COM',13
block: dw 0, tail, 0, 5Ch, 0, 6Ch, 0
ok: db 'DRSAVE upstream registers: ok',13,10,0
fail: db 'DRSAVE upstream registers: FAIL',13,10,0
        times 2048 db 0
stacktop:
