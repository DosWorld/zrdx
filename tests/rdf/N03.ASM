; negative: segment relocation must be rejected by the loader
bits 16
global start
section .text
start:  mov ax, seg start
        mov ax, 4C00h
        int 21h
