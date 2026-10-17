bits 16
org 100h
        mov dx, 0f4h
        xor ax, ax
        out dx, ax
        hlt
