bits 32
global addtwo, shared_d, shared_b
section .text
addtwo: add eax, ebx
        ret
section .data
shared_d: dd 0CAFEh
section .bss
shared_b: resd 1
