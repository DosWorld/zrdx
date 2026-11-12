/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: character classes, case mapping, comparison and
 * the lstr functions. Case mapping covers ASCII, Latin-1, Latin
 * Extended-A, Greek and Cyrillic; comparison is by code point
 * (after folding with IGNORECASE), not by a locale collation.
 */
#include "w32.h"

#define C1_UPPER 1
#define C1_LOWER 2
#define C1_DIGIT 4
#define C1_SPACE 8
#define C1_PUNCT 16
#define C1_CNTRL 32
#define C1_BLANK 64
#define C1_XDIGIT 128
#define C1_ALPHA 256
#define LCMAP_LOWERCASE 0x100UL
#define LCMAP_UPPERCASE 0x200UL
#define NORM_IGNORECASE 1UL

extern int WINAPI MultiByteToWideChar(UINT, DWORD, const char *,
                                      int,
                                      WORD *, int);
extern int WINAPI WideCharToMultiByte(UINT, DWORD, const WORD *,
                                      int,
                                      char *, int, const char *,
                                      BOOL *);

DWORD w32_upper(DWORD c)
{
    if (c < 0x80)
        return c >= 'a' && c <= 'z' ? c - 32 : c;
    if (c == 0xFF)
        return 0x178;
    if (c >= 0xE0 && c <= 0xFE && c != 0xF7)
        return c - 32;
    if (c == 0xB5)
        return 0x39C;
    if (c >= 0x100 && c <= 0x17F) {
        if (c == 0x131 || c == 0x138 || c == 0x149 || c == 0x17F)
            return c;
        if ((c >= 0x139 && c <= 0x148) || (c >= 0x179
                                           && c <= 0x17E))
            return (c & 1) ? c : c - 1;
        if (c == 0x178)
            return c;
        return (c & 1) ? c - 1 : c;
    }
    if (c >= 0x3B1 && c <= 0x3C9)
        return c == 0x3C2 ? 0x3A3 : c - 32;
    if (c >= 0x430 && c <= 0x44F)
        return c - 32;
    if (c >= 0x450 && c <= 0x45F)
        return c - 80;
    return c;
}

DWORD w32_lower(DWORD c)
{
    if (c < 0x80)
        return c >= 'A' && c <= 'Z' ? c + 32 : c;
    if (c >= 0xC0 && c <= 0xDE && c != 0xD7)
        return c + 32;
    if (c == 0x178)
        return 0xFF;
    if (c >= 0x100 && c <= 0x17F) {
        if (c == 0x130 || c == 0x138 || c == 0x149 || c == 0x17F)
            return c;
        if ((c >= 0x139 && c <= 0x148) || (c >= 0x179
                                           && c <= 0x17E))
            return (c & 1) ? c + 1 : c;
        return (c & 1) ? c : c + 1;
    }
    if (c >= 0x391 && c <= 0x3A9 && c != 0x3A2)
        return c + 32;
    if (c >= 0x410 && c <= 0x42F)
        return c + 32;
    if (c >= 0x400 && c <= 0x40F)
        return c + 80;
    return c;
}

static WORD ctype(DWORD c)
{
    WORD f = 0;

    if (c < 0x20 || (c >= 0x7F && c < 0xA0)) {
        f = C1_CNTRL;
        if (c >= 9 && c <= 13)
            f |= C1_SPACE;
        if (c == 9)
            f |= C1_BLANK;
        return f;
    }
    if (c == ' ')
        return C1_SPACE | C1_BLANK;
    if (c >= '0' && c <= '9')
        return C1_DIGIT | C1_XDIGIT;
    if (c < 0x80 && ((c >= 'a' && c <= 'f') || (c >= 'A' &&
                     c <= 'F')))
        f = C1_XDIGIT;
    if (c == 0xA0)
        return C1_SPACE | C1_BLANK;
    if (w32_lower(c) != c)
        return f | C1_UPPER | C1_ALPHA;
    if (w32_upper(c) != c || c == 0xAA || c == 0xBA
            || c == 0xDF ||
            c == 0xFF)
        return f | C1_LOWER | C1_ALPHA;
    if (c >= 0x100 &&
            ((c >= 0x370 && c <= 0x52F) || (c >= 0x5D0
                                            && c <= 0x6FF) ||
             (c >= 0x3040 && c <= 0x9FFF)))
        return C1_ALPHA;
    if (c >= 0xC0 && c != 0xD7 && c != 0xF7 && c < 0x100)
        return C1_ALPHA;
    return C1_PUNCT;
}

BOOL WINAPI GetStringTypeW(DWORD info, const WORD *src, int cch,
                           WORD *out)
{
    int i, n;
    W32T("GetStringTypeW");

    if (info != 1 || src == NULL || out == NULL || cch == 0 ||
            cch < -1) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    if (cch == -1) {
        for (n = 0; src[n]; n++)
            ;
        n++;
    } else
        n = cch;
    for (i = 0; i < n; i++)
        out[i] = ctype(src[i]);
    return TRUE;
}

BOOL WINAPI GetStringTypeA(DWORD locale, DWORD info,
                           const char *src,
                           int cch, WORD *out)
{
    int i, n = cch;
    W32T("GetStringTypeA");

    if (src == NULL || out == NULL || cch == 0 || cch < -1 ||
            info != 1) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    if (cch == -1) {
        for (n = 0; src[n]; n++)
            ;
        n++;
    }
    for (i = 0; i < n; i++) {
        WORD w;

        MultiByteToWideChar(0, 0, src + i, 1, &w, 1);
        out[i] = ctype(w);
    }
    return TRUE;
}

static DWORD fold(DWORD flags, DWORD c)
{
    if (flags & LCMAP_UPPERCASE)
        return w32_upper(c);
    if (flags & LCMAP_LOWERCASE)
        return w32_lower(c);
    return c;
}

int WINAPI LCMapStringW(DWORD locale, DWORD flags,
                        const WORD *src,
                        int cch, WORD *dst, int cchdst)
{
    int i, n;
    W32T("LCMapStringW");

    if (src == NULL || cch == 0 || cch < -1 || cchdst < 0 ||
            (flags & ~(LCMAP_LOWERCASE | LCMAP_UPPERCASE)) ||
            (flags & LCMAP_LOWERCASE
             && flags & LCMAP_UPPERCASE) ||
            (cchdst && dst == NULL)) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    if (cch == -1) {
        for (n = 0; src[n]; n++)
            ;
        n++;
    } else
        n = cch;
    if (cchdst == 0)
        return n;
    if (cchdst < n) {
        w32_seterr(ERROR_INSUFFICIENT_BUFFER);
        return 0;
    }
    for (i = 0; i < n; i++)
        dst[i] = (WORD)fold(flags, src[i]);
    return n;
}

int WINAPI LCMapStringA(DWORD locale, DWORD flags,
                        const char *src,
                        int cch, char *dst, int cchdst)
{
    int i, n = cch;
    W32T("LCMapStringA");

    if (src == NULL || cch == 0 || cch < -1 || cchdst < 0 ||
            (cchdst && dst == NULL) ||
            (flags & ~(LCMAP_LOWERCASE | LCMAP_UPPERCASE)) ||
            (flags & LCMAP_LOWERCASE &&
             flags & LCMAP_UPPERCASE)) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    if (cch == -1) {
        for (n = 0; src[n]; n++)
            ;
        n++;
    }
    if (cchdst == 0)
        return n;
    if (cchdst < n) {
        w32_seterr(ERROR_INSUFFICIENT_BUFFER);
        return 0;
    }
    for (i = 0; i < n; i++) {
        WORD w;
        char back;

        MultiByteToWideChar(0, 0, src + i, 1, &w, 1);
        w = (WORD)fold(flags, w);
        if (WideCharToMultiByte(0, 0, &w, 1, &back, 1, NULL,
                                NULL))
            dst[i] = back;
        else
            dst[i] = src[i];
    }
    return n;
}

static int cmpw(DWORD flags, const WORD *a, int na,
                const WORD *b,
                int nb)
{
    int i = 0;
    DWORD x, y;

    if (na == -1)
        for (na = 0; a[na]; na++)
            ;
    if (nb == -1)
        for (nb = 0; b[nb]; nb++)
            ;
    while (i < na && i < nb) {
        x = a[i];
        y = b[i];
        if (flags & NORM_IGNORECASE) {
            x = w32_upper(x);
            y = w32_upper(y);
        }
        if (x != y)
            return x < y ? 1 : 3;
        i++;
    }
    return na == nb ? 2 : na < nb ? 1 : 3;
}

int WINAPI CompareStringW(DWORD locale, DWORD flags,
                          const WORD *a,
                          int na, const WORD *b, int nb)
{
    W32T("CompareStringW");
    if (a == NULL || b == NULL || na < -1 || nb < -1) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    return cmpw(flags, a, na, b, nb);
}

int WINAPI CompareStringA(DWORD locale, DWORD flags,
                          const char *a,
                          int na, const char *b, int nb)
{
    WORD wa[260], wb[260];
    int ca, cb;
    W32T("CompareStringA");

    if (a == NULL || b == NULL || na < -1 || nb < -1) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    ca = na ? MultiByteToWideChar(0, 0, a, na, wa, 260) : 0;
    cb = nb ? MultiByteToWideChar(0, 0, b, nb, wb, 260) : 0;
    if ((na && ca == 0) || (nb && cb == 0))
        return 0;
    return cmpw(flags, wa, na == -1 ? ca - 1 : ca, wb,
                nb == -1 ? cb - 1 : cb);
}

int WINAPI lstrlenA(const char *s)
{
    int n = 0;

    if (s == NULL)
        return 0;
    while (s[n])
        n++;
    return n;
}

int WINAPI lstrlenW(const WORD *s)
{
    int n = 0;

    if (s == NULL)
        return 0;
    while (s[n])
        n++;
    return n;
}

char *WINAPI lstrcpyA(char *d, const char *s)
{
    w32_copy(d, s, (DWORD)lstrlenA(s) + 1);
    return d;
}

WORD *WINAPI lstrcpyW(WORD *d, const WORD *s)
{
    int i = 0;

    do
        d[i] = s[i];
    while (s[i++]);
    return d;
}

char *WINAPI lstrcpynA(char *d, const char *s, int max)
{
    int i;

    if (max <= 0)
        return d;
    for (i = 0; i < max - 1 && s[i]; i++)
        d[i] = s[i];
    d[i] = 0;
    return d;
}

char *WINAPI lstrcatA(char *d, const char *s)
{
    lstrcpyA(d + lstrlenA(d), s);
    return d;
}

int WINAPI lstrcmpA(const char *a, const char *b)
{
    while (*a && *a == *b) {
        a++;
        b++;
    }
    return (BYTE) * a < (BYTE) * b ? -1
           : (BYTE) * a > (BYTE) * b;
}

int WINAPI lstrcmpiA(const char *a, const char *b)
{
    return CompareStringA(0, NORM_IGNORECASE, a, -1, b, -1) - 2;
}

int WINAPI lstrcmpW(const WORD *a, const WORD *b)
{
    return cmpw(0, a, -1, b, -1) - 2;
}

int WINAPI lstrcmpiW(const WORD *a, const WORD *b)
{
    return cmpw(NORM_IGNORECASE, a, -1, b, -1) - 2;
}

BOOL WINAPI IsDBCSLeadByte(BYTE b)
{
    return FALSE;
}
