; Init order driver (task 011): runs after every DllMain.
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
        mov edx, msg
        mov ah, 9
        int 21h
        mov eax, 4c00h
        int 21h
msg: db 13, 10, 'PE init: ok', 13, 10, '$'
