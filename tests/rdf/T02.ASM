; exit code is passed through (expected 42)
bits 32
global start
section .text
start:  mov eax, 4C2Ah
        int 21h
