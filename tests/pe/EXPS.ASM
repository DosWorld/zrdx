bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
; Export lookup fixture (task 009), runs on stub-pmx only.
; Exports: base 5, slots Alpha(5) -, Beta(7) Gamma(8) Fwd(9, to
; C.Fc) alpha(10); GetProcAddress by name and ordinal.
        jmp start
        align 16
fa:     ret
        align 16
fb:     ret
        align 16
fc:     ret
        align 16
fd:     ret
        align 16
%macro OK 2
        mov byte [step], 'A' + n
        mov eax, 3
        mov ebx, [hnd]
        mov edx, %1
        call far [vp]
        jc fail
        cmp eax, %2
        jne fail
%assign n n + 1
%endmacro
%macro ERR 2
        mov byte [step], 'A' + n
        mov eax, 3
        mov ebx, [hnd]
        mov edx, %1
        call far [vp]
        jnc fail
        cmp eax, %2
        jne fail
%assign n n + 1
%endmacro
%macro NZ 1
        mov byte [step], 'A' + n
        mov eax, 3
        mov ebx, [hnd]
        mov edx, %1
        call far [vp]
        jc fail
        or eax, eax
        jz fail
%assign n n + 1
%endmacro
%assign n 0
start:  mov ax, 168Ah
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
        mov [hnd], eax
        OK nAlpha, fa
        OK nBeta, fb
        OK nGamma, fc
        OK nalpha, fd
        ERR nALPHA, 6
        ERR nDelta, 6
        ERR nEmpty, 6
        OK 5, fa
        OK 7, fb
        OK 8, fc
        OK 10, fd
        ERR 6, 6
        ERR 4, 6
        ERR 11, 6
        NZ 9
        NZ nFwd
        mov dword [hnd], 1
        ERR nAlpha, 5
        mov edx, okmsg
        mov eax, 4c00h
        jmp fin
fail:   mov al, [step]
        mov [bm + 12], al
        mov edx, bm
        mov eax, 4c01h
fin:    push eax
        mov ah, 9
        int 21h
        pop eax
        int 21h
vp: dd 0, 0
hnd: dd 0
step: db 0
vname: db 'ZRDX', 0
mname: db 'EXPS.EXE', 0
nAlpha: db 'Alpha', 0
nBeta: db 'Beta', 0
nGamma: db 'Gamma', 0
nalpha: db 'alpha', 0
nALPHA: db 'ALPHA', 0
nDelta: db 'Delta', 0
nEmpty: db 0
nFwd: db 'Fwd', 0
okmsg: db 'PE exports: ok', 13, 10, '$'
bm: db 'PE exports: X', 13, 10, '$'
