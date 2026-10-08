; negative: unresolved import (not linked) must be rejected by the loader
bits 32
global start
extern foo
section .text
start:  call foo
        mov eax, 4C00h
        int 21h
