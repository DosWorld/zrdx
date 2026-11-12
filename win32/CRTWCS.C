/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: wide character strings, character classes and
 * conversions (UTF-16 code units; letters of ASCII, Latin-1,
 * Greek and Cyrillic are classified).
 */
#include "crt.h"

typedef WORD wchar;

extern double __cdecl strtod(const char *, char **);
extern long __cdecl strtol(const char *, char **, int);
extern unsigned long __cdecl strtoul(const char *, char **, int);
extern void *__cdecl malloc(DWORD);

/* lower-case start, upper-case start, count */
static const WORD ranges[][3] = {
    {0xE0, 0xC0, 23}, {0xF8, 0xD8, 7}, {0x3B1, 0x391, 17},
    {0x3C3, 0x3A3, 7}, {0x430, 0x410, 32}, {0x450, 0x400, 16}
};

/* upper or lower form of c */
static int wmap(int c, int up)
{
    unsigned i;
    int from, to;

    if (up && c == 0xFF)
        return 0x178;
    if (!up && c == 0x178)
        return 0xFF;
    for (i = 0; i < sizeof(ranges) / sizeof(ranges[0]); i++) {
        from = up ? ranges[i][0] : ranges[i][1];
        to = up ? ranges[i][1] : ranges[i][0];
        if (c >= from && c < from + ranges[i][2])
            return c - from + to;
    }
    if (c >= 'a' && c <= 'z' && up)
        return c - 32;
    if (c >= 'A' && c <= 'Z' && !up)
        return c + 32;
    return c;
}

int __cdecl towupper(int c)
{
    return wmap(c, 1);
}

int __cdecl towlower(int c)
{
    return wmap(c, 0);
}

static int upper_p(int c)
{
    return wmap(c, 0) != c;
}

static int lower_p(int c)
{
    return wmap(c, 1) != c || c == 0xDF;
}

int __cdecl iswupper(int c)
{
    return upper_p(c);
}

int __cdecl iswlower(int c)
{
    return lower_p(c);
}

int __cdecl iswalpha(int c)
{
    return upper_p(c) || lower_p(c) || c == 0xAA || c == 0xBA ||
           c == 0xB5;
}

int __cdecl iswdigit(int c)
{
    return c >= '0' && c <= '9';
}

int __cdecl iswxdigit(int c)
{
    return iswdigit(c) || (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

int __cdecl iswspace(int c)
{
    return (c >= 9 && c <= 13) || c == ' ' || c == 0x85 ||
           c == 0xA0 || c == 0x3000;
}

int __cdecl iswcntrl(int c)
{
    return c < 32 || (c >= 0x7F && c < 0xA0);
}

int __cdecl iswalnum(int c)
{
    return iswalpha(c) || iswdigit(c);
}

int __cdecl iswprint(int c)
{
    return !iswcntrl(c);
}

int __cdecl iswgraph(int c)
{
    return iswprint(c) && !iswspace(c);
}

int __cdecl iswpunct(int c)
{
    return iswgraph(c) && !iswalnum(c);
}

int __cdecl iswctype(int c, int mask)
{
    return ((mask & 0x01) && iswupper(c)) ||
           ((mask & 0x02) && iswlower(c)) ||
           ((mask & 0x04) && iswdigit(c)) ||
           ((mask & 0x08) && iswspace(c)) ||
           ((mask & 0x10) && iswpunct(c)) ||
           ((mask & 0x20) && iswcntrl(c)) ||
           ((mask & 0x80) && iswxdigit(c)) ||
           ((mask & 0x100) && iswalpha(c));
}

DWORD __cdecl wcslen(const wchar * s)
{
    DWORD n = 0;

    while (s[n])
        n++;
    return n;
}

wchar *__cdecl wcscpy(wchar * d, const wchar * s)
{
    wchar *r = d;

    while ((*d++ = *s++) != 0)
        ;
    return r;
}

wchar *__cdecl wcsncpy(wchar * d, const wchar * s, DWORD n)
{
    DWORD i;

    for (i = 0; i < n && s[i]; i++)
        d[i] = s[i];
    for (; i < n; i++)
        d[i] = 0;
    return d;
}

wchar *__cdecl wcscat(wchar * d, const wchar * s)
{
    wcscpy(d + wcslen(d), s);
    return d;
}

wchar *__cdecl wcsncat(wchar * d, const wchar * s, DWORD n)
{
    wchar *e = d + wcslen(d);

    while (n-- && *s)
        *e++ = *s++;
    *e = 0;
    return d;
}

static int wcmp(const wchar * a, const wchar * b, DWORD n,
                int fold)
{
    int x, y;

    while (n--) {
        x = *a++;
        y = *b++;
        if (fold) {
            x = towlower(x);
            y = towlower(y);
        }
        if (x != y)
            return x < y ? -1 : 1;
        if (!x)
            break;
    }
    return 0;
}

int __cdecl wcscmp(const wchar * a, const wchar * b)
{
    return wcmp(a, b, 0xFFFFFFFFUL, 0);
}

int __cdecl wcsncmp(const wchar * a, const wchar * b, DWORD n)
{
    return wcmp(a, b, n, 0);
}

int __cdecl _wcsicmp(const wchar * a, const wchar * b)
{
    return wcmp(a, b, 0xFFFFFFFFUL, 1);
}

int __cdecl _wcsnicmp(const wchar * a, const wchar * b, DWORD n)
{
    return wcmp(a, b, n, 1);
}

wchar *__cdecl wcschr(const wchar * s, wchar c)
{
    for (;; s++) {
        if (*s == c)
            return (wchar *) s;
        if (!*s)
            return 0;
    }
}

wchar *__cdecl wcsrchr(const wchar * s, wchar c)
{
    const wchar *r = 0;

    for (;; s++) {
        if (*s == c)
            r = s;
        if (!*s)
            return (wchar *) r;
    }
}

wchar *__cdecl wcsstr(const wchar * s, const wchar * t)
{
    DWORD n = wcslen(t);

    for (; *s || !n; s++)
        if (!wcsncmp(s, t, n))
            return (wchar *) s;
    return 0;
}

DWORD __cdecl wcsspn(const wchar * s, const wchar * set)
{
    DWORD n = 0;

    while (s[n] && wcschr(set, s[n]))
        n++;
    return n;
}

DWORD __cdecl wcscspn(const wchar * s, const wchar * set)
{
    DWORD n = 0;

    while (s[n] && !wcschr(set, s[n]))
        n++;
    return n;
}

wchar *__cdecl wcspbrk(const wchar * s, const wchar * set)
{
    s += wcscspn(s, set);
    return *s ? (wchar *) s : 0;
}

wchar *__cdecl wcstok(wchar * s, const wchar * set)
{
    static wchar *save;
    wchar *t;

    if (!s)
        s = save;
    if (!s)
        return 0;
    s += wcsspn(s, set);
    if (!*s)
        return save = 0;
    t = s + wcscspn(s, set);
    save = *t ? t + 1 : 0;
    *t = 0;
    return s;
}

wchar *__cdecl _wcsdup(const wchar * s)
{
    DWORD n = (wcslen(s) + 1) * 2;
    wchar *p = malloc(n);

    if (p)
        crt_copy(p, s, n);
    return p;
}

wchar *__cdecl _wcslwr(wchar * s)
{
    wchar *p;

    for (p = s; *p; p++)
        *p = (wchar) towlower(*p);
    return s;
}

wchar *__cdecl _wcsupr(wchar * s)
{
    wchar *p;

    for (p = s; *p; p++)
        *p = (wchar) towupper(*p);
    return s;
}

wchar *__cdecl _wcsrev(wchar * s)
{
    DWORD i = 0, j = wcslen(s);
    wchar t;

    while (i + 1 < j) {
        t = s[i];
        s[i++] = s[--j];
        s[j] = t;
    }
    return s;
}

/* narrow copy of the leading ASCII part of a wide number */
static DWORD narrow(const wchar * s, char *b, DWORD size)
{
    DWORD i = 0;

    while (i + 1 < size && s[i] && s[i] < 0x80) {
        b[i] = (char)s[i];
        i++;
    }
    b[i] = 0;
    return i;
}

static void wend(const wchar * s, wchar ** end, const char *b,
                 const char *stop)
{
    if (end)
        *end = (wchar *) s + (stop - b);
}

#define NBUF 400

double __cdecl wcstod(const wchar * s, wchar ** end)
{
    char b[NBUF], *stop;
    double r;

    narrow(s, b, NBUF);
    r = strtod(b, &stop);
    wend(s, end, b, stop);
    return r;
}

long __cdecl wcstol(const wchar * s, wchar ** end, int base)
{
    char b[NBUF], *stop;
    long r;

    narrow(s, b, NBUF);
    r = strtol(b, &stop, base);
    wend(s, end, b, stop);
    return r;
}

unsigned long __cdecl wcstoul(const wchar * s, wchar ** end,
                              int base)
{
    char b[NBUF], *stop;
    unsigned long r;

    narrow(s, b, NBUF);
    r = strtoul(b, &stop, base);
    wend(s, end, b, stop);
    return r;
}

int __cdecl _wtoi(const wchar * s)
{
    return (int)wcstol(s, 0, 10);
}

long __cdecl _wtol(const wchar * s)
{
    return wcstol(s, 0, 10);
}

double __cdecl _wtof(const wchar * s)
{
    return wcstod(s, 0);
}

wchar *__cdecl _itow(int v, wchar * out, int radix)
{
    char b[40];
    int i;

    extern char *__cdecl _itoa(int, char *, int);

    _itoa(v, b, radix);
    for (i = 0; (out[i] = (wchar) (unsigned char)b[i]) != 0; i++)
        ;
    return out;
}

wchar *__cdecl _ltow(long v, wchar * out, int radix)
{
    return _itow((int)v, out, radix);
}

/* code page 0 or 65001 is not used: bytes <-> low 8 bits (ANSI)
   through the KERNEL32 conversion of the current code page */
DWORD __cdecl mbstowcs(wchar * d, const char *s, DWORD n)
{
    int len = lstrlenA(s);

    if (!d)
        return (DWORD) MultiByteToWideChar(0, 0, s, len, 0, 0);
    len = MultiByteToWideChar(0, 0, s, len, d, (int)n);
    if ((DWORD) len < n)
        d[len] = 0;
    return (DWORD) len;
}

DWORD __cdecl wcstombs(char *d, const wchar * s, DWORD n)
{
    int len = (int)wcslen(s);

    if (!d)
        return (DWORD) WideCharToMultiByte(0, 0, s, len, 0, 0, 0,
                                           0);
    len = WideCharToMultiByte(0, 0, s, len, d, (int)n, 0, 0);
    if ((DWORD) len < n)
        d[len] = 0;
    return (DWORD) len;
}

int __cdecl mbtowc(wchar * w, const char *s, DWORD n)
{
    wchar t;

    if (!s)
        return 0;
    if (!n)
        return -1;
    if (!w)
        w = &t;
    if (!MultiByteToWideChar(0, 0, s, 1, w, 1))
        return -1;
    return *s ? 1 : 0;
}

int __cdecl wctomb(char *s, wchar w)
{
    BOOL bad = 0;

    if (!s)
        return 0;
    if (!WideCharToMultiByte(0, 0, &w, 1, s, 1, 0, &bad) || bad)
        return -1;
    return 1;
}
