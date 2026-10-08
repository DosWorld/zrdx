bits 32
global fc, counter
section .text
fc:     inc dword [counter]
        ret
section .bss
counter: resd 1
