; One-function module (task 010): returns VALUE, or calls its
; first import and adds one (CALLIMP). Exported at RVA 1000h.
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
IATVA equ (BASE - 1000h) + 2400h
%ifdef CALLIMP
        call [IATVA]
        inc eax
%else
        mov eax, VALUE
%endif
        ret
