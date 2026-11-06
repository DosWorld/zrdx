; Host without a DPMI function (task 169): in a build made with
; -DFAKELACK=2 the second LoadLibrary fails like a host that
; answers 8001h, and GetLastError must be 14. A normal build
; loads the DLL and the fixture reports "skip".
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
        jmp start
%include "ZRDXAPI.INC"
start:  call ZrdxInit
        mov bl, 'A'
        push nReal
        call LoadLibrary
        add esp, 4
        or eax, eax
        jnz skip
        mov bl, 'B'
        call GetLastError
        cmp eax, 14
        jne fail
        mov edx, okmsg
        jmp fin
skip:   push eax
        call FreeLibrary
        add esp, 4
        mov edx, skipmsg
        jmp fin
fail:   mov [bmx], bl
        mov edx, bm
fin:    mov ah, 9
        int 21h
        mov eax, 4c00h
        int 21h
nReal: db 'REAL.DLL', 0
okmsg: db 13, 10, 'PE lack: ok', 13, 10, '$'
skipmsg: db 13, 10, 'PE lack: skip', 13, 10, '$'
bm: db 13, 10, 'PE lack: fail '
bmx: db 'X', 13, 10, '$'
