/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * USER32.DLL for the ZRDX PE service (docs/WIN32.MD): the
 * character functions that console programs use. The case
 * mapping and the character classes are those of KERNEL32.DLL,
 * which this DLL imports.
 */
#include "w32.h"

#define LCMAP_LOWERCASE 0x100UL
#define LCMAP_UPPERCASE 0x200UL
#define C1_UPPER 1
#define C1_LOWER 2
#define C1_DIGIT 4
#define C1_ALPHA 256

W32_ANCHOR;

extern int WINAPI LCMapStringA(DWORD, DWORD, const char *, int,
                               char *,
                               int);
extern int WINAPI LCMapStringW(DWORD, DWORD, const WORD *, int,
                               WORD *,
                               int);
extern BOOL WINAPI GetStringTypeW(DWORD, const WORD *, int,
                                  WORD *);
extern BOOL WINAPI GetStringTypeA(DWORD, DWORD, const char *,
                                  int,
                                  WORD *);
extern int WINAPI lstrlenA(const char *);
extern int WINAPI lstrlenW(const WORD *);

/* a pointer whose high word is zero is a single character */
static char *map_a(char *s, DWORD flags)
{
    char c;

    if (((DWORD)s >> 16) == 0) {
        c = (char)(DWORD)s;
        LCMapStringA(0, flags, &c, 1, &c, 1);
        return (char *)(DWORD)(BYTE)c;
    }
    LCMapStringA(0, flags, s, lstrlenA(s), s, lstrlenA(s));
    return s;
}

static WORD *map_w(WORD *s, DWORD flags)
{
    WORD c;

    if (((DWORD)s >> 16) == 0) {
        c = (WORD)(DWORD)s;
        LCMapStringW(0, flags, &c, 1, &c, 1);
        return (WORD *)(DWORD)c;
    }
    LCMapStringW(0, flags, s, lstrlenW(s), s, lstrlenW(s));
    return s;
}

char *WINAPI CharUpperA(char *s)
{
    return map_a(s, LCMAP_UPPERCASE);
}

char *WINAPI CharLowerA(char *s)
{
    return map_a(s, LCMAP_LOWERCASE);
}

WORD *WINAPI CharUpperW(WORD *s)
{
    return map_w(s, LCMAP_UPPERCASE);
}

WORD *WINAPI CharLowerW(WORD *s)
{
    return map_w(s, LCMAP_LOWERCASE);
}

DWORD WINAPI CharUpperBuffA(char *s, DWORD n)
{
    return n &&
           LCMapStringA(0, LCMAP_UPPERCASE, s, (int)n, s,
                        (int)n) ?
           n : 0;
}

DWORD WINAPI CharLowerBuffA(char *s, DWORD n)
{
    return n &&
           LCMapStringA(0, LCMAP_LOWERCASE, s, (int)n, s,
                        (int)n) ?
           n : 0;
}

DWORD WINAPI CharUpperBuffW(WORD *s, DWORD n)
{
    return n &&
           LCMapStringW(0, LCMAP_UPPERCASE, s, (int)n, s,
                        (int)n) ?
           n : 0;
}

DWORD WINAPI CharLowerBuffW(WORD *s, DWORD n)
{
    return n &&
           LCMapStringW(0, LCMAP_LOWERCASE, s, (int)n, s,
                        (int)n) ?
           n : 0;
}

static WORD type_w(WORD c)
{
    WORD t = 0;

    GetStringTypeW(1, &c, 1, &t);
    return t;
}

static WORD type_a(char c)
{
    WORD t = 0;

    GetStringTypeA(0, 1, &c, 1, &t);
    return t;
}

BOOL WINAPI IsCharAlphaA(char c)
{
    return (type_a(c) & C1_ALPHA) != 0;
}

BOOL WINAPI IsCharAlphaW(WORD c)
{
    return (type_w(c) & C1_ALPHA) != 0;
}

BOOL WINAPI IsCharAlphaNumericA(char c)
{
    return (type_a(c) & (C1_ALPHA | C1_DIGIT)) != 0;
}

BOOL WINAPI IsCharAlphaNumericW(WORD c)
{
    return (type_w(c) & (C1_ALPHA | C1_DIGIT)) != 0;
}

BOOL WINAPI IsCharUpperA(char c)
{
    return (type_a(c) & C1_UPPER) != 0;
}

BOOL WINAPI IsCharLowerA(char c)
{
    return (type_a(c) & C1_LOWER) != 0;
}

BOOL WINAPI IsCharUpperW(WORD c)
{
    return (type_w(c) & C1_UPPER) != 0;
}

BOOL WINAPI IsCharLowerW(WORD c)
{
    return (type_w(c) & C1_LOWER) != 0;
}

char *WINAPI CharNextA(const char *s)
{
    return (char *)(*s ? s + 1 : s);
}

char *WINAPI CharPrevA(const char *start, const char *s)
{
    return (char *)(s > start ? s - 1 : start);
}
