/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: the format engine of the printf family and the
 * functions built on it. Floating point output is the Microsoft
 * runtime form: exponents have three digits (1.000000e+000) and
 * infinities and NaNs print as 1.#INF and 1.#QNAN. 64 bit
 * integers (%I64d, %lld) are formatted from two 32 bit halves.
 */
#include "crt.h"

#define F_MINUS 1
#define F_PLUS 2
#define F_SPACE 4
#define F_ALT 8
#define F_ZERO 16
#define SZ_H 1
#define SZ_64 2
#define SZ_W 3
#define FLTMAX 1500

typedef struct Out {
    OutFn fn;
    void *ctx;
    int count;
} Out;

static void put(Out * o, int c)
{
    o->fn(c, o->ctx);
    o->count++;
}

static void fill(Out * o, int c, int n)
{
    while (n-- > 0)
        put(o, c);
}

static int slen(const char *s)
{
    int n = 0;

    while (s[n])
        n++;
    return n;
}

/* digits of hi:lo in a base, least significant first (16 bit
   limbs) */
static int utoa64(DWORD lo, DWORD hi, int base, int upper,
                  char *buf)
{
    DWORD l[4], rem, cur;
    int n = 0, i;
    const char *dg = upper ? "0123456789ABCDEF"
                     : "0123456789abcdef";

    l[0] = lo & 0xFFFF;
    l[1] = lo >> 16;
    l[2] = hi & 0xFFFF;
    l[3] = hi >> 16;
    while (l[0] | l[1] | l[2] | l[3]) {
        rem = 0;
        for (i = 3; i >= 0; i--) {
            cur = (rem << 16) | l[i];
            l[i] = cur / (DWORD)base;
            rem = cur % (DWORD)base;
        }
        buf[n++] = dg[rem];
    }
    return n;
}

/* prefix, zeros and body (reverse = body is stored last first)
   with
   the space padding of width */
static void emit(Out * o, int flags, int width, const char *pre,
                 int zeros, const char *body, int nbody,
                 int reverse)
{
    int total = slen(pre) + zeros + nbody;

    if (!(flags & F_MINUS))
        fill(o, ' ', width - total);
    while (*pre)
        put(o, *pre++);
    fill(o, '0', zeros);
    if (reverse)
        while (nbody > 0)
            put(o, body[--nbody]);
    else
        while (nbody-- > 0)
            put(o, *body++);
    if (flags & F_MINUS)
        fill(o, ' ', width - total);
}

static void do_int(Out * o, int flags, int width, int prec,
                   int conv,
                   int size, DWORD lo, DWORD hi)
{
    char buf[70], pre[4];
    int nd, neg = 0, np = 0, zeros = 0, base, sgn;

    sgn = conv == 'd' || conv == 'i';
    base = conv == 'o' ? 8 : (conv == 'x' || conv == 'X') ? 16
           : 10;
    if (size != SZ_64) {
        if (size == SZ_H)
            lo = sgn ? (DWORD)(long)(short)lo : lo & 0xFFFF;
        hi = sgn && (lo >> 31) ? 0xFFFFFFFFUL : 0;
    }
    if (sgn && (hi >> 31)) {
        neg = 1;
        lo = ~lo + 1;
        hi = ~hi + (lo == 0);
    }
    nd = utoa64(lo, hi, base, conv == 'X', buf);
    if (nd == 0 && prec != 0)
        buf[nd++] = '0';
    if (neg)
        pre[np++] = '-';
    else if (sgn && (flags & F_PLUS))
        pre[np++] = '+';
    else if (sgn && (flags & F_SPACE))
        pre[np++] = ' ';
    if ((flags & F_ALT) && (conv == 'x' || conv == 'X') && (lo
            | hi)) {
        pre[np++] = '0';
        pre[np++] = conv;
    }
    pre[np] = 0;
    if (prec > nd)
        zeros = prec - nd;
    if ((flags & F_ALT) && conv == 'o' && zeros == 0 &&
            (nd == 0 || buf[nd - 1] != '0'))
        zeros = 1;
    if ((flags & F_ZERO) && !(flags & F_MINUS) && prec < 0 &&
            width > np + nd + zeros)
        zeros += width - np - nd - zeros;
    emit(o, flags, width, pre, zeros, buf, nd, 1);
}

static void do_str(Out * o, int flags, int width, int prec,
                   const char *s, int wide)
{
    char tmp[8];
    int n, i, k, pad;
    const WORD *w = (const WORD *)s;

    if (s == NULL) {
        s = "(null)";
        wide = 0;
    }
    if (!wide) {
        for (n = 0; s[n] && (prec < 0 || n < prec); n++)
            ;
        emit(o, flags, width, "", 0, s, n, 0);
        return;
    }
    for (n = 0; w[n] && (prec < 0 || n < prec); n++)
        ;
    pad = width - n;
    if (!(flags & F_MINUS))
        fill(o, ' ', pad);
    for (i = 0; i < n; i++) {
        k = WideCharToMultiByte(0, 0, &w[i], 1, tmp, 8, NULL,
                                NULL);
        for (width = 0; width < k; width++)
            put(o, tmp[width]);
    }
    if (flags & F_MINUS)
        fill(o, ' ', pad);
}

/* text of a nonfinite value, NULL if the value is finite */
static const char *special(DWORD lo, DWORD hi)
{
    if (((hi >> 20) & 0x7FF) != 0x7FF)
        return NULL;
    if (((hi & 0xFFFFF) | lo) == 0)
        return (hi >> 31) ? "-1.#INF" : "1.#INF";
    if (hi == 0xFFF80000UL && lo == 0)
        return "-1.#IND";
    return (hi & 0x80000) ? ((hi >> 31) ? "-1.#QNAN"
                             : "1.#QNAN") :
           ((hi >> 31) ? "-1.#SNAN" : "1.#SNAN");
}

/* body of %e style: d.ddd e+xxx; digits dg (n of them) */
static int body_exp(char *b, const char *dg, int prec, int decpt,
                    int iszero,
                    int upper, int alt)
{
    int k = 0, e = iszero ? 0 : decpt - 1, i;

    b[k++] = dg[0];
    if (prec > 0 || alt)
        b[k++] = '.';
    for (i = 1; i <= prec; i++)
        b[k++] = dg[i];
    b[k++] = upper ? 'E' : 'e';
    b[k++] = e < 0 ? '-' : '+';
    if (e < 0)
        e = -e;
    b[k++] = (char)('0' + (e / 100) % 10);
    b[k++] = (char)('0' + (e / 10) % 10);
    b[k++] = (char)('0' + e % 10);
    return k;
}

/* body of %f style with prec fraction digits */
static int body_fix(char *b, const char *dg, int n, int decpt,
                    int prec,
                    int alt)
{
    int k = 0, i, idx;

    if (decpt > 0)
        for (i = 0; i < decpt; i++)
            b[k++] = i < n ? dg[i] : '0';
    else
        b[k++] = '0';
    if (prec > 0 || alt)
        b[k++] = '.';
    for (i = 0; i < prec; i++) {
        idx = decpt + i;
        b[k++] = idx < 0 ? '0' : idx < n ? dg[idx] : '0';
    }
    return k;
}

/* drops trailing zeros of the fraction (and a bare point) */
static int strip(char *b, int k)
{
    int i, epos = k;

    for (i = 0; i < k; i++)
        if (b[i] == 'e' || b[i] == 'E') {
            epos = i;
            break;
        }
    for (i = 0; i < epos; i++)
        if (b[i] == '.')
            break;
    if (i == epos)
        return k;
    while (epos > i + 1 && b[epos - 1] == '0') {
        crt_copy(b + epos - 1, b + epos, (DWORD)(k - epos + 1));
        epos--;
        k--;
    }
    if (epos == i + 1) {
        crt_copy(b + i, b + i + 1, (DWORD)(k - i - 1));
        k--;
    }
    return k;
}

static void do_float(Out * o, int flags, int width, int prec,
                     int conv,
                     DWORD lo, DWORD hi)
{
    static char dg[FLTMAX], body[FLTMAX + 20];
    char pre[2];
    const char *sp = special(lo, hi);
    int neg = (int)(hi >> 31), n, decpt = 0, k, np = 0, zero,
                                                zeros = 0;
    int lower = conv | 0x20, P;

    if (sp) {
        int len = slen(sp);

        emit(o, flags, width, "", 0, sp, len, 0);
        return;
    }
    zero = ((hi & 0x7FFFFFFFUL) | lo) == 0;
    if (prec < 0)
        prec = 6;
    if (prec > 700)
        prec = 700;
    if (neg)
        pre[np++] = '-';
    else if (flags & F_PLUS)
        pre[np++] = '+';
    else if (flags & F_SPACE)
        pre[np++] = ' ';
    pre[np] = 0;
    if (lower == 'f') {
        n = crt_cvt(lo, hi, 1, prec, dg, &decpt);
        k = body_fix(body, dg, n, decpt, prec, flags & F_ALT);
    } else if (lower == 'e') {
        n = crt_cvt(lo, hi, 0, prec + 1, dg, &decpt);
        k = body_exp(body, dg, prec, decpt, zero, conv == 'E',
                     flags & F_ALT);
    } else {
        P = prec == 0 ? 1 : prec;
        n = crt_cvt(lo, hi, 0, P, dg, &decpt);
        if (!zero && (decpt - 1 < -4 || decpt - 1 >= P)) {
            k = body_exp(body, dg, P - 1, decpt, zero,
                         conv == 'G',
                         flags & F_ALT);
        } else {
            if (zero)
                decpt = 1;
            k = body_fix(body, dg, n, decpt, P - decpt > 0 ?
                         P - decpt : 0, flags & F_ALT);
        }
        if (!(flags & F_ALT))
            k = strip(body, k);
    }
    if ((flags & F_ZERO) && !(flags & F_MINUS) && width > np + k)
        zeros = width - np - k;
    emit(o, flags, width, pre, zeros, body, k, 0);
}

int crt_format(OutFn fn, void *ctx, const char *fmt, va_list ap)
{
    Out o;
    int flags, width, prec, size, c;
    DWORD lo, hi;
    double dv;
    const DWORD *bits;

    o.fn = fn;
    o.ctx = ctx;
    o.count = 0;
    for (; *fmt; fmt++) {
        if (*fmt != '%') {
            put(&o, *fmt);
            continue;
        }
        fmt++;
        flags = 0;
        for (;; fmt++) {
            if (*fmt == '-')
                flags |= F_MINUS;
            else if (*fmt == '+')
                flags |= F_PLUS;
            else if (*fmt == ' ')
                flags |= F_SPACE;
            else if (*fmt == '#')
                flags |= F_ALT;
            else if (*fmt == '0')
                flags |= F_ZERO;
            else
                break;
        }
        width = 0;
        if (*fmt == '*') {
            width = va_arg(ap, int);
            if (width < 0) {
                flags |= F_MINUS;
                width = -width;
            }
            fmt++;
        } else
            while (*fmt >= '0' && *fmt <= '9')
                width = width * 10 + (*fmt++ - '0');
        prec = -1;
        if (*fmt == '.') {
            fmt++;
            prec = 0;
            if (*fmt == '*') {
                prec = va_arg(ap, int);
                fmt++;
            } else
                while (*fmt >= '0' && *fmt <= '9')
                    prec = prec * 10 + (*fmt++ - '0');
        }
        size = 0;
        for (;; fmt++) {
            if (*fmt == 'h')
                size = SZ_H;
            else if (*fmt == 'l' && fmt[1] == 'l') {
                size = SZ_64;
                fmt++;
            } else if (*fmt == 'l' || *fmt == 'w')
                size = SZ_W;
            else if (*fmt == 'L')
                size = 0;
            else if (fmt[0] == 'I' && fmt[1] == '6'
                     && fmt[2] == '4') {
                size = SZ_64;
                fmt += 2;
            } else if (fmt[0] == 'I' && fmt[1] == '3'
                       && fmt[2] == '2') {
                size = 0;
                fmt += 2;
            } else
                break;
        }
        c = *fmt;
        switch (c) {
        case 'd':
        case 'i':
        case 'u':
        case 'o':
        case 'x':
        case 'X':
            if (size == SZ_W)
                size = 0;
            lo = va_arg(ap, DWORD);
            hi = size == SZ_64 ? va_arg(ap, DWORD) : 0;
            do_int(&o, flags, width, prec, c, size, lo, hi);
            break;
        case 'p':
            lo = va_arg(ap, DWORD);
            {
                char b[10];
                int nd = utoa64(lo, 0, 16, 1, b);

                emit(&o, flags, width, "", 8 - nd > 0 ? 8 - nd
                     : 0, b,
                     nd, 1);
            }
            break;
        case 'c':
        case 'C': {
            char ch[4];
            int wide = c == 'C' ? size != SZ_W : size == SZ_W;

            lo = va_arg(ap, DWORD);
            if (wide) {
                WORD wc = (WORD)lo;

                hi = (DWORD)WideCharToMultiByte(0, 0, &wc, 1, ch,
                                                4,
                                                NULL, NULL);
            } else {
                ch[0] = (char)lo;
                hi = 1;
            }
            emit(&o, flags, width, "", 0, ch, (int)hi, 0);
        }
        break;
        case 's':
        case 'S': {
            const char *s = va_arg(ap, const char *);
            int wide = c == 'S' ? size != SZ_W : size == SZ_W;

            do_str(&o, flags, width, prec, s, wide);
        }
        break;
        case 'n':
            *va_arg(ap, int *) = o.count;
            break;
        case 'e':
        case 'E':
        case 'f':
        case 'F':
        case 'g':
        case 'G':
            dv = va_arg(ap, double);
            bits = (const DWORD *)&dv;
            do_float(&o, flags, width, prec, c, bits[0],
                     bits[1]);
            break;
        case '%':
            put(&o, '%');
            break;
        case 0:
            return o.count;
        default:
            put(&o, c);
        }
    }
    return o.count;
}

typedef struct StrOut {
    char *p;
    int cap;
    int n;
} StrOut;

static void out_file(int c, void *ctx)
{
    fputc(c, (FILE *) ctx);
}

static void out_str(int c, void *ctx)
{
    StrOut *s = ctx;

    if (s->n < s->cap)
        s->p[s->n] = (char)c;
    s->n++;
}

static void out_none(int c, void *ctx)
{
}

int __cdecl vfprintf(FILE * f, const char *fmt, va_list ap)
{
    int n = crt_format(out_file, f, fmt, ap);

    return ferror(f) ? -1 : n;
}

int __cdecl fprintf(FILE * f, const char *fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = vfprintf(f, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl vprintf(const char *fmt, va_list ap)
{
    return vfprintf(&_iob[1], fmt, ap);
}

int __cdecl printf(const char *fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = vfprintf(&_iob[1], fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl vsprintf(char *buf, const char *fmt, va_list ap)
{
    StrOut s;

    s.p = buf;
    s.cap = 0x7FFFFFFF;
    s.n = 0;
    crt_format(out_str, &s, fmt, ap);
    buf[s.n] = 0;
    return s.n;
}

int __cdecl sprintf(char *buf, const char *fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = vsprintf(buf, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl _vsnprintf(char *buf, unsigned count,
                       const char *fmt,
                       va_list ap)
{
    StrOut s;

    s.p = buf;
    s.cap = (int)count;
    s.n = 0;
    crt_format(out_str, &s, fmt, ap);
    if (s.n < s.cap) {
        buf[s.n] = 0;
        return s.n;
    }
    return s.n == s.cap ? s.n : -1;
}

int __cdecl _snprintf(char *buf, unsigned count, const char *fmt,
                      ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = _vsnprintf(buf, count, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl _vscprintf(const char *fmt, va_list ap)
{
    return crt_format(out_none, NULL, fmt, ap);
}

int __cdecl _scprintf(const char *fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = _vscprintf(fmt, ap);
    va_end(ap);
    return n;
}
