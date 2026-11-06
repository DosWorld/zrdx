; LMKD.COM: make the directory named by the command tail
; through the LFN API (7139h); exit code 1 when it fails.
bits 16
org 100h
        mov si, 81h
.sp:    lodsb
        cmp al, ' '
        je .sp
        dec si
        mov dx, si
.end:   lodsb
        cmp al, 13
        jne .end
        mov byte [si - 1], 0
        mov ax, 7139h
        stc
        int 21h
        mov ax, 4C00h
        jnc .out
        inc ax
.out:   int 21h
