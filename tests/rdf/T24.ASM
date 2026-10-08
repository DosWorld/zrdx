; data holds pointers to bss and to code
%include "t.inc"
bits 32
global start
TNAME 'T24 data to bss and code'
section .data
pb:     dd bv
pc:     dd fn
section .bss
bv:     resd 1
section .text
fn:     mov eax, 77
        ret
start:  BEGIN
        mov edi, [pb]
        mov dword [edi], 5555h
        cmp dword [bv], 5555h
        jne bad
        mov eax, fn
        cmp [pc], eax
        jne bad
        call [pc]
        cmp eax, 77
        jne bad
        PASS
bad:    FAIL
