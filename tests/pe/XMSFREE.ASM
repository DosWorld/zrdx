; Prints the total free XMS in KB as "XMSFREE n" (task 161).
; Exit code 1 when no XMS driver is present.
        org 100h
        mov ax, 4300h
        int 2Fh
        cmp al, 80h
        jne .none
        mov ax, 4310h
        int 2Fh
        mov [entry], bx
        mov [entry + 2], es
        mov ah, 8
        call far [entry]
        mov ax, dx
        mov di, buf + 7
        mov bx, 10
.d:     xor dx, dx
        div bx
        add dl, '0'
        dec di
        mov [di], dl
        or ax, ax
        jnz .d
        mov dx, msg
        mov ah, 9
        int 21h
        mov dx, di
        mov ah, 9
        int 21h
        mov ax, 4C00h
        int 21h
.none:  mov ax, 4C01h
        int 21h
msg:    db 'XMSFREE $'
entry:  dd 0
buf:    db '       ', 13, 10, '$'
