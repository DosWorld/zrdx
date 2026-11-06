/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: the scanf family. One engine reads characters from
 * a string or a FILE through two callbacks.
 */
#include "crt.h"

extern double __cdecl strtod(const char *s, char **end);
extern int __cdecl ungetc(int c, FILE * f);
extern int __cdecl fgetc(FILE * f);

typedef int (*GetFn)(void *ctx);
typedef void (*UngetFn)(int c, void *ctx);

typedef struct In
{
    GetFn get;
    UngetFn unget;
    void *ctx;
    int count;
    int eof;
} In;

static int next(In * in)
{
    int c = in->get(in->ctx);

    if (c == EOF)
        in->eof = 1;
    else
        in->count++;
    return c;
}

static void back(In * in, int c)
{
    if (c != EOF)
    {
        in->unget(c, in->ctx);
        in->count--;
    }
}

static int is_space(int c)
{
    return c == ' ' || (c >= 9 && c <= 13);
}

static int dval(int c)
{
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;
    if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    return 99;
}

static int skip_space(In * in)
{
    int c;

    do
        c = next(in);
    while (c != EOF && is_space(c));
    back(in, c);
    return c != EOF;
}

/* integer conversion into lo:hi; returns 1 on success. width is
   the number of characters that may still be read */
static int scan_int(In * in, int base, int width, int sgn,
                    DWORD *lo,
                    DWORD *hi)
{
    int c, neg = 0, got = 0, d;
    DWORD t0, t1, t2, t3;

    *lo = 0;
    *hi = 0;
    if (!skip_space(in))
        return 0;
    c = next(in);
    width--;
    if ((c == '-' || c == '+') && width > 0)
    {
        neg = c == '-';
        c = next(in);
        width--;
    }
    if (c == '0' && (base == 0 || base == 16) && width > 0)
    {
        int c2 = next(in), c3;

        width--;
        got = 1;
        if ((c2 == 'x' || c2 == 'X') && width > 0)
        {
            c3 = next(in);
            if (dval(c3) < 16)
            {
                base = 16;
                c = c3;
                width--;
                got = 0;
            }
            else
            {
                back(in, c3);
                back(in, c2);
                width++;
                c = next(in);
            }
        }
        else
            c = c2;
        if (base == 0)
            base = 8;
    }
    if (base == 0)
        base = 10;
    for (;;)
    {
        d = dval(c);
        if (c == EOF || d >= base)
            break;
        got = 1;
        t0 = (*lo & 0xFFFF) * (DWORD)base + (DWORD)d;
        t1 = (*lo >> 16) * (DWORD)base + (t0 >> 16);
        t2 = (*hi & 0xFFFF) * (DWORD)base + (t1 >> 16);
        t3 = (*hi >> 16) * (DWORD)base + (t2 >> 16);
        *lo = (t0 & 0xFFFF) | ((t1 & 0xFFFF) << 16);
        *hi = (t2 & 0xFFFF) | ((t3 & 0xFFFF) << 16);
        if (width <= 0)
        {
            c = EOF;
            break;
        }
        c = next(in);
        width--;
    }
    if (c != EOF || !in->eof)
        back(in, c);
    if (!got)
        return 0;
    if (neg)
    {
        *lo = ~*lo + 1;
        *hi = ~*hi + (*lo == 0);
    }
    (void)sgn;
    return 1;
}

/* floating point text (at most width characters) into buf */
static int scan_float(In * in, int width, char *buf)
{
    int n = 0, c, digits = 0, seen_e = 0, seen_dot = 0;

    if (!skip_space(in))
        return 0;
    c = next(in);
    if ((c == '-' || c == '+') && n < width)
    {
        buf[n++] = (char)c;
        c = next(in);
    }
    for (; c != EOF && n < width && n < 380; c = next(in))
    {
        if (c >= '0' && c <= '9')
            digits = 1;
        else if (c == '.' && !seen_dot && !seen_e)
            seen_dot = 1;
        else if ((c == 'e' || c == 'E') && digits && !seen_e)
        {
            seen_e = 1;
            buf[n++] = (char)c;
            c = next(in);
            if (c == '-' || c == '+')
                buf[n++] = (char)c;
            else
                back(in, c);
            continue;
        }
        else
            break;
        buf[n++] = (char)c;
    }
    back(in, c);
    buf[n] = 0;
    return digits;
}

typedef struct Set
{
    const char *p;
    int len;
    int neg;
} Set;

static int in_set(const Set * s, int c)
{
    int i, hit = 0;

    for (i = 0; i < s->len; i++)
    {
        if (i + 2 < s->len && s->p[i + 1] == '-')
        {
            if (c >= (unsigned char)s->p[i] &&
                    c <= (unsigned char)s->p[i + 2])
                hit = 1;
            i += 2;
        }
        else if (c == (unsigned char)s->p[i])
            hit = 1;
    }
    return hit != s->neg;
}

static int scan_engine(In * in, const char *fmt, va_list ap)
{
    int assigned = 0, c, suppress, width, size, conv, ok,
        first = 1;
    DWORD lo, hi;
    char buf[400];

    for (; *fmt; fmt++)
    {
        if (is_space((unsigned char) * fmt))
        {
            skip_space(in);
            continue;
        }
        if (*fmt != '%' || fmt[1] == '%')
        {
            if (*fmt == '%')
                fmt++;
            c = next(in);
            if (c != (unsigned char) * fmt)
            {
                back(in, c);
                return assigned == 0 && c == EOF && first ? EOF :
                       assigned;
            }
            continue;
        }
        fmt++;
        suppress = 0;
        if (*fmt == '*')
        {
            suppress = 1;
            fmt++;
        }
        width = 0;
        while (*fmt >= '0' && *fmt <= '9')
            width = width * 10 + (*fmt++ - '0');
        size = 0;
        for (;; fmt++)
        {
            if (*fmt == 'h')
                size = 1;
            else if (*fmt == 'l' && fmt[1] == 'l')
            {
                size = 3;
                fmt++;
            }
            else if (*fmt == 'l')
                size = 2;
            else if (*fmt == 'L')
                size = 2;
            else if (fmt[0] == 'I' && fmt[1] == '6'
                     && fmt[2] == '4')
            {
                size = 3;
                fmt += 2;
            }
            else
                break;
        }
        conv = *fmt;
        if (conv == 0)
            break;
        if (width == 0)
            width = 0x7FFFFFFF;
        ok = 0;
        switch (conv)
        {
        case 'd':
        case 'i':
        case 'u':
        case 'o':
        case 'x':
        case 'X':
        case 'p':
            ok = scan_int(in, conv == 'o' ? 8 : conv == 'd' ||
                          conv == 'u' ? 10 : conv == 'i' ? 0
                          : 16,
                          width, conv == 'd' || conv == 'i', &lo,
                          &hi);
            if (ok && !suppress)
            {
                if (size == 3)
                {
                    DWORD *q = va_arg(ap, DWORD *);

                    q[0] = lo;
                    q[1] = hi;
                }
                else if (size == 1)
                {
                    unsigned short *q = va_arg(ap, unsigned short *);

                    *q = (unsigned short)lo;
                }
                else
                    *va_arg(ap, DWORD *) = lo;
                assigned++;
            }
            break;
        case 'e':
        case 'E':
        case 'f':
        case 'g':
        case 'G':
            ok = scan_float(in, width, buf);
            if (ok && !suppress)
            {
                double d = strtod(buf, NULL);

                if (size >= 2)
                    *va_arg(ap, double *) = d;
                else
                    *va_arg(ap, float *) = (float)d;
                assigned++;
            }
            break;
        case 'c':
        {
            char *q = suppress ? NULL : va_arg(ap, char *);
            int n = width == 0x7FFFFFFF ? 1 : width;

            for (ok = 0; ok < n; ok++)
            {
                c = next(in);
                if (c == EOF)
                    break;
                if (q)
                    q[ok] = (char)c;
            }
            ok = ok == n;
            if (ok && q)
                assigned++;
        }
        break;
        case 's':
        {
            char *q = suppress ? NULL : va_arg(ap, char *);
            int n = 0;

            if (skip_space(in))
            {
                while (n < width && (c = next(in)) != EOF)
                {
                    if (is_space(c))
                    {
                        back(in, c);
                        break;
                    }
                    if (q)
                        q[n] = (char)c;
                    n++;
                }
            }
            if (q && n)
                q[n] = 0;
            ok = n > 0;
            if (ok && q)
                assigned++;
        }
        break;
        case '[':
        {
            char *q = suppress ? NULL : va_arg(ap, char *);
            Set set;
            int n = 0;

            fmt++;
            set.neg = *fmt == '^';
            if (set.neg)
                fmt++;
            set.p = fmt;
            if (*fmt == ']')
                fmt++;
            while (*fmt && *fmt != ']')
                fmt++;
            set.len = (int)(fmt - set.p);
            while (n < width && (c = next(in)) != EOF)
            {
                if (!in_set(&set, c))
                {
                    back(in, c);
                    break;
                }
                if (q)
                    q[n] = (char)c;
                n++;
            }
            if (q && n)
                q[n] = 0;
            ok = n > 0;
            if (ok && q)
                assigned++;
        }
        break;
        case 'n':
            if (!suppress)
                *va_arg(ap, int *) = in->count;
            ok = 1;
            break;
        default:
            return assigned;
        }
        if (!ok)
            return assigned == 0 && in->eof && first ? EOF
                   : assigned;
        first = 0;
    }
    return assigned;
}

typedef struct StrIn
{
    const char *p;
} StrIn;

static int str_get(void *ctx)
{
    StrIn *s = ctx;

    return *s->p ? (unsigned char) * s->p++ : EOF;
}

static void str_unget(int c, void *ctx)
{
    StrIn *s = ctx;

    s->p--;
}

static int file_get(void *ctx)
{
    return fgetc((FILE *) ctx);
}

static void file_unget(int c, void *ctx)
{
    ungetc(c, (FILE *) ctx);
}

int crt_vsscanf(const char *s, const char *fmt, va_list ap)
{
    StrIn si;
    In in;

    si.p = s;
    in.get = str_get;
    in.unget = str_unget;
    in.ctx = &si;
    in.count = 0;
    in.eof = 0;
    return scan_engine(&in, fmt, ap);
}

int crt_vfscanf(FILE * f, const char *fmt, va_list ap)
{
    In in;

    in.get = file_get;
    in.unget = file_unget;
    in.ctx = f;
    in.count = 0;
    in.eof = 0;
    return scan_engine(&in, fmt, ap);
}

int __cdecl sscanf(const char *s, const char *fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = crt_vsscanf(s, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl fscanf(FILE * f, const char *fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = crt_vfscanf(f, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl scanf(const char *fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = crt_vfscanf(&_iob[0], fmt, ap);
    va_end(ap);
    return n;
}
