/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: string to number conversions (strtol family, atoi,
 * strtod, atof). strtod is exact: see CRTFLT.C.
 */
#include "crt.h"

#define LONG_MAX_V 0x7FFFFFFFUL

/* what the compiler expects of a floating point run-time; the
   x87 is used inline (-fpi87), there is no emulator */
int fltused_;
char _8087 = 3;

static int is_space(int c)
{
    return c == ' ' || (c >= 9 && c <= 13);
}

static int digit_val(int c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'a' && c <= 'z')
        return c - 'a' + 10;
    if (c >= 'A' && c <= 'Z')
        return c - 'A' + 10;
    return 99;
}

/* common part: value as unsigned, sets *neg and *over; *end at
   the first unconverted character (the start if nothing) */
static DWORD parse(const char *s, char **end, int base, int *neg,
                   int *over)
{
    const char *p = s, *start;
    DWORD v = 0, cutoff, cutlim;
    int d, any = 0;

    *neg = 0;
    *over = 0;
    while (is_space((unsigned char) * p))
        p++;
    if (*p == '-' || *p == '+')
        *neg = *p++ == '-';
    if (base == 0)
    {
        if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X'))
            base = 16;
        else
            base = p[0] == '0' ? 8 : 10;
    }
    if (base < 2 || base > 36)
    {
        crt_errno = CRT_EINVAL;
        if (end)
            *end = (char *)s;
        return 0;
    }
    if (base == 16 && p[0] == '0' && (p[1] == 'x'
        || p[1] == 'X') &&
            digit_val((unsigned char)p[2]) < 16)
        p += 2;
    start = p;
    cutoff = 0xFFFFFFFFUL / (DWORD)base;
    cutlim = 0xFFFFFFFFUL % (DWORD)base;
    for (; (d = digit_val((unsigned char) * p)) < base; p++)
    {
        any = 1;
        if (v > cutoff || (v == cutoff && (DWORD)d > cutlim))
            *over = 1;
        else
            v = v * (DWORD)base + (DWORD)d;
    }
    if (end)
        *end = (char *)(any ? p : s);
    (void)start;
    return v;
}

unsigned long __cdecl strtoul(const char *s, char **end,
                              int base)
{
    int neg, over;
    DWORD v = parse(s, end, base, &neg, &over);

    if (over)
    {
        crt_errno = CRT_ERANGE;
        return 0xFFFFFFFFUL;
    }
    return neg ? 0 - v : v;
}

long __cdecl strtol(const char *s, char **end, int base)
{
    int neg, over;
    DWORD v = parse(s, end, base, &neg, &over);

    if (over || (!neg && v > LONG_MAX_V) ||
            (neg && v > LONG_MAX_V + 1))
    {
        crt_errno = CRT_ERANGE;
        return neg ? (long)0x80000000UL : (long)LONG_MAX_V;
    }
    return neg ? (long)(0 - v) : (long)v;
}

int __cdecl atoi(const char *s)
{
    return (int)strtol(s, NULL, 10);
}

long __cdecl atol(const char *s)
{
    return strtol(s, NULL, 10);
}

double __cdecl strtod(const char *s, char **end)
{
    DWORD w[2];

    crt_strtod_bits(s, end, &w[0], &w[1]);
    if (((w[1] >> 20) & 0x7FF) == 0x7FF)
        crt_errno = CRT_ERANGE;
    return *(double *)w;
}

double __cdecl atof(const char *s)
{
    return strtod(s, NULL);
}
