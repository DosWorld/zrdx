; Exit detach fixture (task 013): DllMain prints a line when it
; is detached at process exit (reason 0, reserved 1).
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
IATVA equ (BASE - 1000h) + 2400h
        mov eax, [esp + 8]
        or eax, eax
        jnz ok
        cmp dword [esp + 12], 1
        jne ok
        mov edx, msg
        mov ah, 9
        int 21h
ok:     mov eax, 1
        ret
msg: db 'PE exit detach', 13, 10, '$'
