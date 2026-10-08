;A macro file for structured programming in ASM
;use and redistribution are permitted without any limitations.
;(C) Sergey Belyakov, 1998
;(C) Viacheslav Komenda, 2026
;NASM version

%assign zn 0
%assign zi_else 0
%assign zi_endif 0
%assign zd_do 0
%assign zd_while 0
%assign zd_enddo 0

%macro zlab 1
..@z %+ %1:
%endmacro

%macro zgen 2-3
%if %0 > 2
%1 near ..@z %+ %2
%else
%1 ..@z %+ %2
%endif
%endmacro

%macro jecxz_n 1
        jecxz %%t
        jmp short %%s
%%t:
        jmp %1
%%s:
%endmacro

%macro _ifnot 1-2
%push zif
%assign %$ie zi_else
%assign %$in zi_endif
%assign zi_endif zn
%assign zn zn+1
%assign zi_else zn
%assign zn zn+1
%if %0 > 1
zgen %1, zi_else, near
%else
zgen %1, zi_else
%endif
%endmacro

%macro _else 0-1
%if %0 > 0
zgen %1, zi_endif
%endif
zlab zi_else
%assign zi_else zn
%assign zn zn+1
%endmacro

%macro _endif 0
zlab zi_else
zlab zi_endif
%assign zi_else %$ie
%assign zi_endif %$in
%pop
%endmacro

%macro _toelse 0-1
%if %0 > 0
zgen %1, zi_else
%else
zgen jmp, zi_else
%endif
%endmacro

%macro _toendif 0-1
%if %0 > 0
zgen %1, zi_endif
%else
zgen jmp, zi_endif
%endif
%endmacro

%macro _do 0-1
%push zdo
%assign %$od zd_do
%assign %$ow zd_while
%assign %$oe zd_enddo
%assign zd_do zn
%assign zn zn+1
%assign zd_while zn
%assign zn zn+1
%assign zd_enddo zn
%assign zn zn+1
%if %0 > 0
zgen %1, zd_while
%endif
zlab zd_do
%endmacro

%macro _while 0
zlab zd_while
%endmacro

%macro _towhile 0-1
%if %0 > 0
zgen %1, zd_while
%else
zgen jmp, zd_while
%endif
%endmacro

%macro _loop 0-1
%if %0 > 0
zgen %1, zd_do
%else
zgen jmp, zd_do
%endif
%endmacro

%macro _break 0-2
%if %0 > 1
zgen %1, zd_enddo, near
%elif %0 > 0
zgen %1, zd_enddo
%else
zgen jmp, zd_enddo
%endif
%endmacro

%macro _enddo 0-1
%if %0 > 0
zgen %1, zd_do
%endif
zlab zd_enddo
%assign zd_do %$od
%assign zd_while %$ow
%assign zd_enddo %$oe
%pop
%endmacro
