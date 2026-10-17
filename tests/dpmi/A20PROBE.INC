; AX=0/1 for physical A20 off/on. Restore both bytes and all flags.
a20query:
        pushf
        cli
        push ds
        push es
        push bx
        push cx
        xor ax, ax
        mov ds, ax
        dec ax
        mov es, ax
        mov bl, [500h]
        mov cl, [es:510h]
        mov byte [500h], 0
        mov byte [es:510h], 0FFh
        xor ax, ax
        cmp byte [500h], 0FFh
        setne al
        mov [es:510h], cl
        mov [500h], bl
        pop cx
        pop bx
        pop es
        pop ds
        popf
        ret
