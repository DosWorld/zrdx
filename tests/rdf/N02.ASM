; negative: DLL record must be rejected by the loader
bits 32
library mylib
global start
section .text
start:  mov eax, 4C00h
        int 21h
