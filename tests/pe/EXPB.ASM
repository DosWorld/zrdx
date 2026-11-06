bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
; Damaged export arrays (task 009): lookup must give code 3.
        mov ax, 168Ah
        mov esi, vname
        int 2Fh
        or al, al
        jnz fail
        mov [vp], edi
        mov [vp + 4], es
        mov eax, 2
        mov edx, mname
        call far [vp]
        jc fail
        mov ebx, eax
        mov eax, 3
        mov edx, nAlpha
        call far [vp]
        jnc fail
        cmp eax, 3
        jne fail
        mov edx, okmsg
        mov eax, 4c00h
        jmp fin
fail:   mov edx, bm
        mov eax, 4c01h
fin:    push eax
        mov ah, 9
        int 21h
        pop eax
        int 21h
vp: dd 0, 0
vname: db 'ZRDX', 0
mname: db 'EXPB.EXE', 0
nAlpha: db 'Alpha', 0
okmsg: db 'PE exports bad: ok', 13, 10, '$'
bm: db 'PE exports bad: failed', 13, 10, '$'
