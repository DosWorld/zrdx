; Runtime loading driver (task 012): exports the SDK wrappers,
; calls PlugRun of PLUGX.DLL and reports its step code.
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
IATVA equ (BASE - 1000h) + 2400h
%ifndef TAG
%define TAG 'runtime'
%endif
        jmp start
%include "ZRDXAPI.INC"
start:  call ZrdxInit
        call [IATVA]
        or eax, eax
        jnz bad
        mov edx, okmsg
        mov eax, 4c00h
        jmp fin
bad:    add al, 'A'
        mov [bmx], al
        mov edx, bm
        mov eax, 4c01h
fin:    push eax
        mov ah, 9
        int 21h
        pop eax
%ifdef RETEXIT
        movzx eax, al
        ret
%else
        int 21h
%endif
okmsg: db 13, 10, 'PE ', TAG, ': ok', 13, 10, '$'
bm: db 13, 10, 'PE ', TAG, ': fail '
bmx: db 'X', 13, 10, '$'
