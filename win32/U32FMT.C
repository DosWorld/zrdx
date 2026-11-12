/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * USER32.DLL: wsprintf and wvsprintf, written from the public
 * API description: flags - # 0, width, .precision (strings),
 * size prefixes h l, types c C d i u x X s S p, at most 1024
 * characters with the terminating zero.
 */
#include "w32.h"

#define MAXOUT 1024

typedef struct Fmt {
    BYTE *out;
    int wide;
    int n;
} Fmt;

W32_ANCHOR;

static int isdig(DWORD c)
{
    return c >= '0' && c <= '9';
}

static DWORD get(const void *p, int wide, int i)
{
    return wide ? ((const WORD *)p)[i] : ((const BYTE *)p)[i];
}

static void put(Fmt *f, DWORD c)
{
    if (f->n < MAXOUT - 1) {
        if (f->wide)
            ((WORD *)f->out)[f->n] = (WORD)c;
        else
            f->out[f->n] = (BYTE)c;
        f->n++;
    }
}

static void pad(Fmt *f, int count, DWORD c)
{
    while (count-- > 0)
        put(f, c);
}

/* digits of v in base (10 or 16) into buf, returns the count */
static int digits(DWORD v, DWORD base, int upper, char *buf)
{
    char tmp[12];
    int n = 0, i = 0;

    do {
        tmp[n] = "0123456789abcdef"[v % base];
        if (upper && tmp[n] > '9')
            tmp[n] -= 32;
        n++;
        v /= base;
    } while (v);
    while (n)
        buf[i++] = tmp[--n];
    return i;
}

typedef struct Spec {
    int left, zero, alt, width, prec, shortp;
} Spec;

/* an item of len characters (text at s, wide or not) */
static void emit(Fmt *f, const Spec *sp, const void *s,
                 int swide, int len, const char *prefix)
{
    int i, np = 0;

    while (prefix && prefix[np])
        np++;
    if (!sp->left && !sp->zero)
        pad(f, sp->width - len - np, ' ');
    for (i = 0; i < np; i++)
        put(f, (BYTE)prefix[i]);
    if (!sp->left && sp->zero)
        pad(f, sp->width - len - np, '0');
    for (i = 0; i < len; i++)
        put(f, get(s, swide, i));
    if (sp->left)
        pad(f, sp->width - len - np, ' ');
}

static int slen(const void *s, int wide, int prec)
{
    int n = 0;

    while ((prec < 0 || n < prec) && get(s, wide, n))
        n++;
    return n;
}

static const WORD nullw[] = {'(', 'n', 'u', 'l', 'l', ')', 0};

/* one conversion; returns the next argument pointer */
static const DWORD *conv(Fmt *f, Spec *sp, DWORD c, int lng,
                         const DWORD *a)
{
    char buf[12];
    const char *prefix = 0;
    const void *s;
    DWORD v = *a++;
    int n, sw = (c == 'c' || c == 's') ? f->wide : !f->wide;

    switch (c) {
    case 'c':
    case 'C':
        emit(f, sp, &v, sw, 1, 0);
        break;
    case 's':
    case 'S':
        s = (const void *)v;
        if (s == 0)
            s = sw ? (const void *)nullw :
                (const void *)"(null)";
        emit(f, sp, s, sw, slen(s, sw, sp->prec), 0);
        break;
    default:
        if (sp->shortp && !lng) {
            v &= 0xFFFFUL;
            if ((c == 'd' || c == 'i') && v > 0x7FFFUL)
                v |= 0xFFFF0000UL;
        }
        if ((c == 'd' || c == 'i') && (long)v < 0) {
            prefix = "-";
            v = (DWORD) - (long)v;
        }
        if (c == 'p') {
            sp->zero = 1;
            sp->width = 8;
            c = 'X';
        } else if ((c == 'x' || c == 'X') && sp->alt && v) {
            prefix = c == 'x' ? "0x" : "0X";
        }
        n = digits(v, (c == 'x' || c == 'X') ? 16 : 10, c == 'X',
                   buf);
        emit(f, sp, buf, 0, n, prefix);
    }
    return a;
}

static int run(Fmt *f, const void *fmt, const DWORD *a)
{
    int i = 0;
    DWORD c;
    Spec sp;
    int lng;

    while ((c = get(fmt, f->wide, i++)) != 0) {
        if (c != '%') {
            put(f, c);
            continue;
        }
        sp.left = sp.zero = sp.alt = sp.width = sp.shortp = 0;
        sp.prec = -1;
        lng = 0;
        for (;; i++) {
            c = get(fmt, f->wide, i);
            if (c == '-')
                sp.left = 1;
            else if (c == '0')
                sp.zero = 1;
            else if (c == '#')
                sp.alt = 1;
            else
                break;
        }
        while (isdig(c = get(fmt, f->wide, i))) {
            sp.width = sp.width * 10 + (int)(c - '0');
            i++;
        }
        if (get(fmt, f->wide, i) == '.') {
            i++;
            sp.prec = 0;
            while (isdig(c = get(fmt, f->wide, i))) {
                sp.prec = sp.prec * 10 + (int)(c - '0');
                i++;
            }
        }
        for (;; i++) {
            c = get(fmt, f->wide, i);
            if (c == 'l')
                lng = 1;
            else if (c == 'h')
                sp.shortp = 1;
            else
                break;
        }
        c = get(fmt, f->wide, i);
        if (c == 0)
            break;
        i++;
        if (c == '%')
            put(f, c);
        else
            a = conv(f, &sp, c, lng, a);
    }
    return f->n;
}

/* returns the count of characters written (not the zero) */
static int format(void *out, const void *fmt, const DWORD *args,
                  int wide)
{
    Fmt f;

    f.out = out;
    f.wide = wide;
    f.n = 0;
    run(&f, fmt, args);
    if (wide)
        ((WORD *)out)[f.n] = 0;
    else
        ((BYTE *)out)[f.n] = 0;
    return f.n;
}

int __cdecl wsprintfA(char *out, const char *fmt, ...)
{
    return format(out, fmt, (const DWORD *)(&fmt + 1), 0);
}

int __cdecl wsprintfW(WORD *out, const WORD *fmt, ...)
{
    return format(out, fmt, (const DWORD *)(&fmt + 1), 1);
}

int WINAPI wvsprintfA(char *out, const char *fmt,
                      const DWORD *args)
{
    return format(out, fmt, args, 0);
}

int WINAPI wvsprintfW(WORD *out, const WORD *fmt,
                      const DWORD *args)
{
    return format(out, fmt, args, 1);
}
