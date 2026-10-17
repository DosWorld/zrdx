bits 32
org 401000h
        mov edx,message
        mov ah,9
        int 21h
        mov eax,4c00h
        int 21h
message: db 'PE single module: ok',13,10,'$'
