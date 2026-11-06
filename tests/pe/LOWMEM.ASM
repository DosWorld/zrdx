; Low memory fixture (task 015): takes all DPMI memory, then
; LoadLibrary must fail with 4 and leave no trace; after freeing
; the memory the same load works.
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
        jmp start
%include "ZRDXAPI.INC"
%macro HOG 1
%%l:    mov ebx, %1 >> 16
        mov ecx, %1 & 0FFFFh
        mov ax, 501h
        int 31h
        jc %%d
        mov [ebp], si
        mov [ebp + 2], di
        add ebp, 4
        jmp %%l
%%d:
%endmacro
start:  call ZrdxInit
        mov ebp, table
        HOG 100000h
        HOG 10000h
        HOG 1000h
        mov bl, 'A'
        push nReal
        call LoadLibrary
        add esp, 4
        or eax, eax
        jnz fail
        mov bl, 'B'
        call GetLastError
        cmp eax, 4
        jne fail
        mov bl, 'C'
        push nReal
        call GetModuleHandle
        add esp, 4
        or eax, eax
        jnz fail
        mov edx, table
back:   cmp edx, ebp
        jae freed
        mov si, [edx]
        mov di, [edx + 2]
        mov ax, 502h
        int 31h
        add edx, 4
        jmp back
freed:  mov bl, 'D'
        push nReal
        call LoadLibrary
        add esp, 4
        or eax, eax
        jz fail
        mov bl, 'E'
        push eax
        call FreeLibrary
        add esp, 4
        or eax, eax
        jnz fail
        mov edx, okmsg
        mov eax, 4c00h
        jmp fin
fail:   mov [bmx], bl
        mov edx, bm
        mov eax, 4c01h
fin:    push eax
        mov ah, 9
        int 21h
        pop eax
        int 21h
nReal: db 'REAL.DLL', 0
okmsg: db 13, 10, 'PE low memory: ok', 13, 10, '$'
bm: db 13, 10, 'PE low memory: fail '
bmx: db 'X', 13, 10, '$'
table: times 512 dd 0
