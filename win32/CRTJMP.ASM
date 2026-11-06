; This file is part of the ZRDX OSE project
; (C) 2026, Viacheslav Komenda
;
; MSVCRT.DLL: setjmp and longjmp. The jmp_buf has the layout of
; the Microsoft runtime (16 dwords): Ebp, Ebx, Edi, Esi, Esp,
; Eip first, the rest (SEH registration, try level) is zeroed.

bits 32

segment _TEXT use32 class=CODE

global __setjmp3, __setjmp, _longjmp

; int _setjmp3(jmp_buf env, int count, ...)
__setjmp3:
__setjmp:
        mov edx, [esp + 4]
        mov [edx], ebp
        mov [edx + 4], ebx
        mov [edx + 8], edi
        mov [edx + 12], esi
        lea eax, [esp + 4]
        mov [edx + 16], eax
        mov eax, [esp]
        mov [edx + 20], eax
        xor eax, eax
        mov ecx, 10
.clr:   mov [edx + 20 + ecx * 4], eax
        loop .clr
        ret

; void longjmp(jmp_buf env, int value)
_longjmp:
        mov edx, [esp + 4]
        mov eax, [esp + 8]
        test eax, eax
        jnz .go
        inc eax
.go:    mov ebp, [edx]
        mov ebx, [edx + 4]
        mov edi, [edx + 8]
        mov esi, [edx + 12]
        mov ecx, [edx + 20]
        mov esp, [edx + 16]
        jmp ecx
