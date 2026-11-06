; This file is part of the ZRDX OSE project
; (C) 2026, Viacheslav Komenda
;
; MSVCRT.DLL: math functions on the x87 (cdecl, arguments on the
; stack, result in ST0). The transcendental ones run through
; WRAP at 64 bit precision (the process default is 53 bit) and
; restore the control word; the rounding mode is changed only
; for floor, ceil, modf and _ftol and restored.

bits 32

extern _crt_errno

segment _TEXT use32 class=CODE

global _sqrt, _sin, _cos, _tan, _atan, _atan2, _asin, _acos
global _crt_expm1, _exp, _log, _log10, _pow, _floor, _ceil
global _fabs, _fmod
global _ldexp, _frexp, _modf, __ftol, __CIsin, __CIcos, __CItan
global __CIatan, __CIatan2, __CIasin, __CIacos, __CIexp, __CIlog
global __CIlog10, __CIpow, __CIsqrt, __CIfmod

EDOM equ 33
ERANGE equ 34

; sets errno and leaves ST0 = NaN
nan_edom:
        mov dword [_crt_errno], EDOM
        fstp st0
        fldz
        fldz
        fdivp st1, st0
        ret

_sqrt:
        fld qword [esp + 4]
        ftst
        fstsw ax
        sahf
        jb .neg
        fsqrt
        ret
.neg:   fstp st0
        jmp nan_edom

_sin:
        fld qword [esp + 4]
        fsin
        ret

_cos:
        fld qword [esp + 4]
        fcos
        ret

_tan:
        fld qword [esp + 4]
        fptan
        fstp st0
        ret

_atan:
        fld qword [esp + 4]
        fld1
        fpatan
        ret

_atan2_core:
        fld qword [esp + 4]
        fld qword [esp + 12]
        fpatan
        ret

; |x| <= 1 check: leaves nothing, CF set if outside
unit_check:
        fld qword [esp + 8]
        fabs
        fld1
        fcompp
        fstsw ax
        sahf
        ret                     ; CF set if 1 < |x|

_asin_core:
        call unit_check
        jb nan_edom_nothing
        call unit_root
        fpatan
        ret

_acos_core:
        call unit_check
        jb nan_edom_nothing
        call unit_root
        fxch
        fpatan
        ret

; ST0 = x -> ST0 = sqrt((1 - x) * (1 + x)), ST1 = x
unit_root:
        fld qword [esp + 8]
        fld1
        fadd st0, st1
        fld1
        fsub st0, st2
        fmulp st1, st0
        fsqrt
        ret

nan_edom_nothing:
        mov dword [_crt_errno], EDOM
        fldz
        fldz
        fdivp st1, st0
        ret

; ST0 = y -> ST0 = 2^y
exp2_st0:
        fld st0
        frndint
        fsub st1, st0
        fxch
        f2xm1
        fld1
        faddp st1, st0
        fscale
        fstp st1
        ret

_exp_core:
        fld qword [esp + 4]
        fldl2e
        fmulp st1, st0
        jmp exp2_st0

; expm1 for |x| < 0.5
_crt_expm1_core:
        fld qword [esp + 4]
        fldl2e
        fmulp st1, st0
        f2xm1
        ret

_log_core:
        fldln2
        jmp log_common
_log10_core:
        fldlg2
log_common:
        fld qword [esp + 4]
        ftst
        fstsw ax
        sahf
        jb .neg
        je .zero
        fyl2x
        ret
.zero:  mov dword [_crt_errno], ERANGE
        fyl2x
        ret
.neg:   fstp st0
        fstp st0
        jmp nan_edom_nothing

; pow(x, y)
_pow_core:
        fld qword [esp + 12]
        ftst
        fstsw ax
        sahf
        jne .ynz
        fstp st0
        fld1
        ret
.ynz:   fld qword [esp + 4]
        ftst
        fstsw ax
        sahf
        je .xzero
        jb .xneg
        fyl2x
        jmp exp2_st0
.xzero: fstp st0
        ftst
        fstsw ax
        sahf
        fstp st0
        jb .inf
        fldz
        ret
.inf:   mov dword [_crt_errno], ERANGE
        fld1
        fldz
        fdivp st1, st0
        ret
.xneg:  fxch                    ; y, x: y must be an integer
        fld st0
        frndint
        fcomp st1
        fstsw ax
        sahf
        jne .nonint
        fld1
        fld1
        faddp st1, st0          ; 2, y, x
        fxch                    ; y, 2, x
.rem:   fprem
        fstsw ax
        test ah, 4
        jnz .rem
        ftst
        fstsw ax
        sahf
        setnz dl                ; odd exponent
        fstp st0
        fstp st0
        fstp st0
        fld qword [esp + 12]
        fld qword [esp + 4]
        fabs
        fyl2x
        call exp2_st0
        test dl, dl
        jz .even
        fchs
.even:  ret
.nonint: fstp st0
        fstp st0
        jmp nan_edom_nothing

; save/restore the control word around a directed rounding
; calls _NAME_core with its NARGS argument dwords at 64 bit
; precision; the control word is restored afterwards
%macro WRAP 2
_%1:
        push eax
        fnstcw [esp]
        mov ax, [esp]
        push eax
        or ax, 0300h
        mov [esp + 4], ax
        fldcw [esp + 4]
%rep %2
        push dword [esp + 8 + 4 * %2]
%endrep
        call _%1_core
        add esp, 4 * %2
        fldcw [esp]
        add esp, 8
        ret
%endmacro

WRAP exp, 2
WRAP crt_expm1, 2
WRAP log, 2
WRAP log10, 2
WRAP pow, 4
WRAP atan2, 4
WRAP asin, 2
WRAP acos, 2

%macro DIRECTED 1
        sub esp, 8
        fnstcw [esp]
        mov ax, [esp]
        mov [esp + 2], ax
        and ax, 0F3FFh
        or ax, %1
        mov [esp + 4], ax
        fldcw [esp + 4]
%endmacro

_floor:
        fld qword [esp + 4]
        DIRECTED 0400h
        frndint
        fldcw [esp + 2]
        add esp, 8
        ret

_ceil:
        fld qword [esp + 4]
        DIRECTED 0800h
        frndint
        fldcw [esp + 2]
        add esp, 8
        ret

_fabs:
        fld qword [esp + 4]
        fabs
        ret

_fmod:
        fld qword [esp + 12]
        ftst
        fstsw ax
        sahf
        jne .ok
        fstp st0
        jmp nan_edom_nothing
.ok:    fld qword [esp + 4]
.rem:   fprem
        fstsw ax
        test ah, 4
        jnz .rem
        fstp st1
        ret

_ldexp:
        fild dword [esp + 12]
        fld qword [esp + 4]
        fscale
        fstp st1
        ret

_frexp:
        mov edx, [esp + 12]
        fld qword [esp + 4]
        ftst
        fstsw ax
        sahf
        jne .nz
        mov dword [edx], 0
        ret
.nz:    fxtract
        fld1
        fld1
        faddp st1, st0
        fdivp st1, st0
        fxch
        fld1
        faddp st1, st0
        fistp dword [edx]
        ret

_modf:
        mov edx, [esp + 12]
        fld qword [esp + 4]
        fld st0
        DIRECTED 0C00h
        frndint
        fldcw [esp + 2]
        add esp, 8
        fst qword [edx]
        fsubp st1, st0
        ret

; conversion with truncation, result in EAX (EDX:EAX)
__ftol:
        sub esp, 12
        fnstcw [esp + 8]
        mov ax, [esp + 8]
        or ax, 0C00h
        mov [esp + 10], ax
        fldcw [esp + 10]
        fistp qword [esp]
        fldcw [esp + 8]
        mov eax, [esp]
        mov edx, [esp + 4]
        add esp, 12
        ret

; Microsoft intrinsic entries: arguments in ST0 (and ST1)
%macro CI1 2
%1:
        sub esp, 8
        fstp qword [esp]
        call %2
        add esp, 8
        ret
%endmacro
%macro CI2 2
%1:
        sub esp, 16
        fstp qword [esp + 8]
        fstp qword [esp]
        call %2
        add esp, 16
        ret
%endmacro

        CI1 __CIsin, _sin
        CI1 __CIcos, _cos
        CI1 __CItan, _tan
        CI1 __CIatan, _atan
        CI1 __CIasin, _asin
        CI1 __CIacos, _acos
        CI1 __CIexp, _exp
        CI1 __CIlog, _log
        CI1 __CIlog10, _log10
        CI1 __CIsqrt, _sqrt
        CI2 __CIatan2, _atan2
        CI2 __CIpow, _pow
        CI2 __CIfmod, _fmod
