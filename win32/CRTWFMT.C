/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: wide printf family. The wide format is turned into
 * a narrow one (%s, %c take wide arguments, %S, %C narrow) and
 * run by the narrow engine; output characters are 8 bit (ANSI).
 */
#include "crt.h"

typedef WORD wchar;

extern void *__cdecl malloc(DWORD);
extern void __cdecl free(void *);

typedef struct WOut
{
    wchar *p;
    int cap;
    int n;
} WOut;

static void out_w(int c, void *ctx)
{
    WOut *s = ctx;

    if (s->n < s->cap)
        s->p[s->n] = (wchar) (unsigned char)c;
    s->n++;
}

static void out_none(int c, void *ctx)
{
}

static void out_file(int c, void *ctx)
{
    fputc(c, (FILE *) ctx);
}

static int spec_char(wchar c)
{
    return (c >= '0' && c <= '9') || c == '-' || c == '+' ||
           c == ' ' || c == '#' || c == '.' || c == '*' ||
           c == 'h' || c == 'l' || c == 'w' || c == 'I';
}

/* narrow copy of a wide format, malloc'ed (NULL: no memory) */
static char *narrow_fmt(const wchar * w)
{
    DWORD n = 0, i = 0;
    char *b;
    int pre;

    while (w[n])
        n++;
    b = malloc(n * 2 + 1);
    if (!b)
        return NULL;
    for (n = 0; w[n]; n++)
    {
        b[i++] = (char)(w[n] < 0x80 ? w[n] : '?');
        if (w[n] != '%')
            continue;
        if (w[n + 1] == '%')
        {
            b[i++] = '%';
            n++;
            continue;
        }
        pre = 0;
        while (spec_char(w[n + 1]))
        {
            n++;
            pre |= (w[n] == 'h' || w[n] == 'l' || w[n] == 'w' ||
                    w[n] == 'I');
            b[i++] = (char)w[n];
        }
        if (w[n + 1] == 's' || w[n + 1] == 'c')
        {
            if (!pre)
                b[i++] = 'l';
        }
        if (w[n + 1])
        {
            n++;
            b[i++] = (char)((w[n] == 'S' || w[n] == 'C') ?
                            w[n] + 32 : (w[n] < 0x80 ? w[n] : '?'));
        }
    }
    b[i] = 0;
    return b;
}

int __cdecl _vsnwprintf(wchar * buf, DWORD count, const wchar * fmt,
                        va_list ap)
{
    WOut s;
    char *nf = narrow_fmt(fmt);

    if (!nf)
        return -1;
    s.p = buf;
    s.cap = (int)count;
    s.n = 0;
    crt_format(out_w, &s, nf, ap);
    free(nf);
    if (s.n < s.cap)
    {
        buf[s.n] = 0;
        return s.n;
    }
    return s.n == s.cap ? s.n : -1;
}

int __cdecl vswprintf(wchar * buf, const wchar * fmt, va_list ap)
{
    return _vsnwprintf(buf, 0x7FFFFFFF, fmt, ap);
}

int __cdecl _vscwprintf(const wchar * fmt, va_list ap)
{
    char *nf = narrow_fmt(fmt);
    int n;

    if (!nf)
        return -1;
    n = crt_format(out_none, NULL, nf, ap);
    free(nf);
    return n;
}

int __cdecl vfwprintf(FILE * f, const wchar * fmt, va_list ap)
{
    char *nf = narrow_fmt(fmt);
    int n;

    if (!nf)
        return -1;
    n = crt_format(out_file, f, nf, ap);
    free(nf);
    return n;
}

int __cdecl vwprintf(const wchar * fmt, va_list ap)
{
    return vfwprintf(&_iob[1], fmt, ap);
}

int __cdecl _snwprintf(wchar * buf, DWORD count, const wchar * fmt,
                       ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = _vsnwprintf(buf, count, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl swprintf(wchar * buf, const wchar * fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = vswprintf(buf, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl _scwprintf(const wchar * fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = _vscwprintf(fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl fwprintf(FILE * f, const wchar * fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = vfwprintf(f, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl wprintf(const wchar * fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = vfwprintf(&_iob[1], fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl fputwc(wchar c, FILE * f)
{
    return fputc(c < 0x100 ? c : '?', f) == EOF ? 0xFFFF : c;
}

int __cdecl putwchar(wchar c)
{
    return fputwc(c, &_iob[1]);
}

int __cdecl fputws(const wchar * s, FILE * f)
{
    for (; *s; s++)
        if (fputwc(*s, f) == 0xFFFF)
            return EOF;
    return 0;
}
