; minimal program: code only, exits with code 0
bits 32
global start
section .text
start:  mov eax, 4C00h
        int 21h
