; LE fixture. LECHK.PY adds a selector fixup (page 1 ->
; BSS object) to the RLINK output, since RLINK drops "dw seg",
; and clears that object's flags in a copy: the loader must
; stop with an error, not loop. Exit code 42.
bits 32
global start
section .text
start:  mov eax, 4C2Ah
        int 21h
section .data
segref: dw 0
section .bss
target: resd 1
