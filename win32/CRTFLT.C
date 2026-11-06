/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: exact conversion between a double and decimal
 * text. No C runtime helper is used (no 64 bit integers, no
 * floating point arithmetic): numbers are arrays of decimal
 * digits or of 16 bit limbs and the double is handled as its
 * bit pattern. The conversion to text is exact (the decimal
 * expansion of the binary value is built and then rounded to
 * even); the conversion from text rounds to nearest even.
 */
#include "crt.h"

#define MAXDIG 800
#define MAXLIMB 300
#define STRTOD_DIGITS 780

/* decimal digit array, least significant digit first */
static void mulsmall(char *d, int *n, DWORD f)
{
    DWORD carry = 0, v;
    int i;

    for (i = 0; i < *n; i++)
    {
        v = (DWORD)(unsigned char)d[i] * f + carry;
        d[i] = (char)(v % 10);
        carry = v / 10;
    }
    while (carry)
    {
        d[(*n)++] = (char)(carry % 10);
        carry /= 10;
    }
}

/* digits of |x| (most significant first) in out, *decpt = point
   position; returns the digit count (a nonzero finite value) */
static int exact_digits(DWORD lo, DWORD hi, char *out,
                        int *decpt)
{
    static char dig[MAXDIG];
    DWORD l[4];
    int expo = (int)((hi >> 20) & 0x7FF), e2, n = 0, i, k;
    DWORD rem, cur;

    l[0] = lo & 0xFFFF;
    l[1] = lo >> 16;
    l[2] = hi & 0xFFFF;
    l[3] = (hi >> 16) & 0xF;
    if (expo)
    {
        l[3] |= 0x10;
        e2 = expo - 1075;
    }
    else
        e2 = -1074;
    while (l[0] | l[1] | l[2] | l[3])
    {
        rem = 0;
        for (i = 3; i >= 0; i--)
        {
            cur = (rem << 16) | l[i];
            l[i] = cur / 10;
            rem = cur % 10;
        }
        dig[n++] = (char)rem;
    }
    if (e2 >= 0)
    {
        for (k = e2; k >= 15; k -= 15)
            mulsmall(dig, &n, 32768UL);
        if (k)
            mulsmall(dig, &n, 1UL << k);
        *decpt = n;
    }
    else
    {
        for (k = -e2; k >= 6; k -= 6)
            mulsmall(dig, &n, 15625UL);
        if (k)
        {
            DWORD p = 1;

            while (k--)
                p *= 5;
            mulsmall(dig, &n, p);
        }
        *decpt = n + e2;
    }
    for (i = 0; i < n; i++)
        out[i] = (char)('0' + dig[n - 1 - i]);
    return n;
}

int crt_cvt(DWORD lo, DWORD hi, int mode, int ndig, char *out,
            int *decpt)
{
    static char s[MAXDIG];
    int len, need, i, up, d0;

    if (((hi & 0x7FFFFFFFUL) | lo) == 0)
    {
        *decpt = 1;
        need = mode ? ndig + 1 : ndig;
        for (i = 0; i < need; i++)
            out[i] = '0';
        return need;
    }
    len = exact_digits(lo, hi, s, &d0);
    *decpt = d0;
    need = mode ? d0 + ndig : ndig;
    if (need < 0)
        return 0;
    if (need >= len)
    {
        for (i = 0; i < need; i++)
            out[i] = i < len ? s[i] : '0';
        return need;
    }
    up = s[need] > '5';
    if (s[need] == '5')
    {
        for (i = need + 1; i < len && s[i] == '0'; i++)
            ;
        up = i < len || (need > 0 && ((s[need - 1] - '0') & 1));
    }
    for (i = 0; i < need; i++)
        out[i] = s[i];
    if (!up)
        return need;
    for (i = need - 1; i >= 0 && out[i] == '9'; i--)
        out[i] = '0';
    if (i >= 0)
    {
        out[i]++;
        return need;
    }
    out[0] = '1';
    for (i = 1; i < need; i++)
        out[i] = '0';
    (*decpt)++;
    if (mode)
        out[need++] = '0';
    else if (need == 0)
        need = 1;
    return need;
}

/* ---- text to double: big numbers of 16 bit limbs ---- */

typedef struct Big
{
    DWORD l[MAXLIMB];
    int n;
} Big;

static void big_mul(Big * b, DWORD f, DWORD add)
{
    DWORD carry = add, v;
    int i;

    for (i = 0; i < b->n; i++)
    {
        v = b->l[i] * f + carry;
        b->l[i] = v & 0xFFFF;
        carry = v >> 16;
    }
    while (carry && b->n < MAXLIMB)
    {
        b->l[b->n++] = carry & 0xFFFF;
        carry >>= 16;
    }
}

/* b = b / f, returns the remainder (f < 65536) */
static DWORD big_div(Big * b, DWORD f)
{
    DWORD rem = 0, cur;
    int i;

    for (i = b->n - 1; i >= 0; i--)
    {
        cur = (rem << 16) | b->l[i];
        b->l[i] = cur / f;
        rem = cur % f;
    }
    while (b->n && b->l[b->n - 1] == 0)
        b->n--;
    return rem;
}

static void big_shl(Big * b, int bits)
{
    int w = bits / 16, s = bits % 16, i;
    DWORD v, carry = 0;

    if (b->n == 0)
        return;
    if (s)
    {
        for (i = 0; i < b->n; i++)
        {
            v = (b->l[i] << s) | carry;
            b->l[i] = v & 0xFFFF;
            carry = v >> 16;
        }
        if (carry)
            b->l[b->n++] = carry;
    }
    if (w)
    {
        for (i = b->n - 1; i >= 0; i--)
            b->l[i + w] = b->l[i];
        for (i = 0; i < w; i++)
            b->l[i] = 0;
        b->n += w;
    }
}

static int big_bits(const Big * b)
{
    int bits;
    DWORD top;

    if (b->n == 0)
        return 0;
    top = b->l[b->n - 1];
    for (bits = 0; top; bits++)
        top >>= 1;
    return (b->n - 1) * 16 + bits;
}

/* bit i of b */
static DWORD big_bit(const Big * b, int i)
{
    return i / 16 < b->n ? (b->l[i / 16] >> (i % 16)) & 1 : 0;
}

/* value = big * 2^e2 (+ sticky) rounded to a double (bit halves)
   */
static void pack(const Big * b, int e2, int sticky, DWORD *lo,
                 DWORD *hi)
{
    int len = big_bits(b), exp, keep, i, up;
    DWORD m_lo = 0, m_hi = 0, rb, st;

    if (len == 0)
    {
        *lo = 0;
        *hi = 0;
        return;
    }
    exp = len - 1 + e2;
    if (exp > 1023)
    {
        *lo = 0;
        *hi = 0x7FF00000UL;
        return;
    }
    keep = exp >= -1022 ? 53 : 53 - (-1022 - exp);
    if (keep <= 0)
    {
        if (keep == 0 && (len > 1 || sticky))
        {
            *lo = 1;
            *hi = 0;
        }
        else
        {
            *lo = 0;
            *hi = 0;
        }
        return;
    }
    st = (DWORD)sticky;
    if (len > keep)
    {
        for (i = 0; i < len - keep - 1; i++)
            st |= big_bit(b, i);
        rb = big_bit(b, len - keep - 1);
    }
    else
        rb = 0;
    for (i = 0; i < keep; i++)
    {
        int src = len - 1 - i;
        DWORD bit = src >= 0 ? big_bit(b, src) : 0;

        m_hi = (m_hi << 1) | (m_lo >> 31);
        m_lo = (m_lo << 1) | bit;
    }
    up = rb && (st || (m_lo & 1));
    if (up && ++m_lo == 0)
        m_hi++;
    if (keep == 53 && (m_hi >> 21))
    {
        m_lo = (m_lo >> 1) | (m_hi << 31);
        m_hi >>= 1;
        exp++;
        if (exp > 1023)
        {
            *lo = 0;
            *hi = 0x7FF00000UL;
            return;
        }
    }
    if (exp >= -1022 && keep == 53)
    {
        *lo = m_lo;
        *hi = ((DWORD)(exp + 1023) << 20) | (m_hi & 0xFFFFF);
    }
    else
    {
        *lo = m_lo;
        *hi = m_hi;
        if (keep < 53 && (m_hi >> 20))
            *hi = (m_hi & 0xFFFFF) | 0x100000UL;
    }
}

void crt_strtod_bits(const char *s, char **end, DWORD *lo,
                     DWORD *hi)
{
    static char dig[STRTOD_DIGITS + 4];
    Big *b;
    const char *p = s;
    int neg = 0, nd = 0, point = 0, seen = 0, dropped = 0,
        nz = 0;
    int frac = 0, ex, e10, sticky = 0, i, shift;
    DWORD rem;

    *lo = 0;
    *hi = 0;
    while (*p == ' ' || (*p >= 9 && *p <= 13))
        p++;
    if (*p == '-' || *p == '+')
        neg = *p++ == '-';
    for (;; p++)
    {
        if (*p >= '0' && *p <= '9')
        {
            seen = 1;
            if (point)
                frac++;
            if (nd == 0 && *p == '0')
                continue;
            if (nd < STRTOD_DIGITS)
                dig[nd++] = *p;
            else
            {
                dropped++;
                nz |= *p != '0';
            }
        }
        else if (*p == '.' && !point)
            point = 1;
        else
            break;
    }
    if (!seen)
    {
        if (end)
            *end = (char *)s;
        return;
    }
    ex = dropped - frac;
    if ((*p == 'e' || *p == 'E') &&
            ((p[1] >= '0' && p[1] <= '9') ||
             ((p[1] == '-' || p[1] == '+') &&
              p[2] >= '0' && p[2] <= '9')))
    {
        int eneg = 0, v = 0;

        p++;
        if (*p == '-' || *p == '+')
            eneg = *p++ == '-';
        while (*p >= '0' && *p <= '9')
        {
            if (v < 100000)
                v = v * 10 + (*p - '0');
            p++;
        }
        ex += eneg ? -v : v;
    }
    if (end)
        *end = (char *)p;
    if (neg)
        *hi = 0x80000000UL;
    if (nd == 0)
        return;
    if (nd + ex > 310)
    {
        *hi |= 0x7FF00000UL;
        return;
    }
    if (nd + ex < -330)
        return;
    b = calloc(1, sizeof(Big));
    if (b == NULL)
        return;
    sticky = nz;
    for (i = 0; i < nd; i++)
        big_mul(b, 10, (DWORD)(dig[i] - '0'));
    e10 = ex;
    if (e10 >= 0)
    {
        for (; e10 >= 4; e10 -= 4)
            big_mul(b, 10000, 0);
        for (; e10 > 0; e10--)
            big_mul(b, 10, 0);
        pack(b, 0, sticky, lo, hi);
    }
    else
    {
        int k = -e10, steps;

        shift = 70 + (k * 233) / 100 - big_bits(b);
        if (shift > 0)
            big_shl(b, shift);
        else
            shift = 0;
        for (steps = k; steps >= 6; steps -= 6)
            sticky |= big_div(b, 15625UL) != 0;
        if (steps)
        {
            DWORD p5 = 1;

            for (i = 0; i < steps; i++)
                p5 *= 5;
            rem = big_div(b, p5);
            sticky |= rem != 0;
        }
        pack(b, -(shift + k), sticky, lo, hi);
    }
    free(b);
    if (neg)
        *hi |= 0x80000000UL;
}
