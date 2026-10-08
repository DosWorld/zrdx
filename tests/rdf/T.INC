%macro PRINT 1
        mov edx, %1
        mov ah, 9
        int 21h
%endmacro

%macro EXITC 1
        mov eax, 4C00h + %1
        int 21h
%endmacro

%macro TNAME 1
section .data
t_name: db %1, ': $'
t_ok:   db 'ok', 13, 10, '$'
t_fail: db 'FAIL', 13, 10, '$'
section .text
%endmacro

%macro BEGIN 0
        PRINT t_name
%endmacro

%macro PASS 0
        PRINT t_ok
        EXITC 0
%endmacro

%macro FAIL 0
        PRINT t_fail
        EXITC 1
%endmacro
