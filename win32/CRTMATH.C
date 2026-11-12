/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: math functions built on those of CRTM.ASM, and the
 * floating point classification and status calls.
 */
#include "crt.h"

extern double __cdecl exp(double x);
extern double __cdecl sqrt(double x);
extern double __cdecl crt_expm1(double x);

/* what the compiler expects of a floating point run-time */
int fltused_;
char _8087 = 3;

static DWORD hi_of(double x)
{
    return ((const DWORD *)&x)[1];
}

static DWORD lo_of(double x)
{
    return ((const DWORD *)&x)[0];
}

int __cdecl _finite(double x)
{
    return ((hi_of(x) >> 20) & 0x7FF) != 0x7FF;
}

int __cdecl _isnan(double x)
{
    return ((hi_of(x) >> 20) & 0x7FF) == 0x7FF &&
           ((hi_of(x) & 0xFFFFF) | lo_of(x)) != 0;
}

double __cdecl _chgsign(double x)
{
    return -x;
}

double __cdecl _copysign(double x, double y)
{
    double a = x < 0 ? -x : x;

    return (hi_of(y) >> 31) ? -a : a;
}

double __cdecl _hypot(double x, double y)
{
    return sqrt(x * x + y * y);
}

double __cdecl sinh(double x)
{
    double a = x < 0 ? -x : x, e;

    if (a < 0.5) {
        e = crt_expm1(a);
        a = (e + e / (e + 1.0)) / 2.0;
    } else {
        e = exp(a);
        a = (e - 1.0 / e) / 2.0;
    }
    return x < 0 ? -a : a;
}

double __cdecl cosh(double x)
{
    double e = exp(x < 0 ? -x : x);

    return (e + 1.0 / e) / 2.0;
}

double __cdecl tanh(double x)
{
    double a = x < 0 ? -x : x, e;

    if (a > 20.0)
        return x < 0 ? -1.0 : 1.0;
    if (a < 0.25) {
        e = crt_expm1(2.0 * a);
        a = e / (e + 2.0);
    } else {
        e = exp(2.0 * a);
        a = (e - 1.0) / (e + 1.0);
    }
    return x < 0 ? -a : a;
}

extern unsigned crt_status87(void);
#pragma aux crt_status87 = \
    "fnstsw ax" \
    "and eax, 0FFFFh" \
    value [eax];

extern void crt_clear87(void);
#pragma aux crt_clear87 = "fnclex";

/* x87 exception flags in the Microsoft bit positions */
unsigned __cdecl _statusfp(void)
{
    unsigned sw = crt_status87(), ms = 0;

    ms |= (sw & 0x01) ? 0x10 : 0;
    ms |= (sw & 0x02) ? 0x80000UL : 0;
    ms |= (sw & 0x04) ? 0x08 : 0;
    ms |= (sw & 0x08) ? 0x04 : 0;
    ms |= (sw & 0x10) ? 0x02 : 0;
    ms |= (sw & 0x20) ? 0x01 : 0;
    return ms;
}

unsigned __cdecl _clearfp(void)
{
    unsigned s = _statusfp();

    crt_clear87();
    return s;
}
