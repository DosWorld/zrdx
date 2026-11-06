/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: character classes and the "C" locale. The table
 * _ctype (index c + 1, 257 entries) and the pointer _pctype are
 * what the ctype macros of compiled programs read.
 */
#include "crt.h"

#define C_UPPER 0x01
#define C_LOWER 0x02
#define C_DIGIT 0x04
#define C_SPACE 0x08
#define C_PUNCT 0x10
#define C_CNTRL 0x20
#define C_BLANK 0x40
#define C_HEX 0x80
#define C_ALPHA 0x100

typedef struct LConv
{
    char *decimal_point;
    char *thousands_sep;
    char *grouping;
    char *int_curr_symbol;
    char *currency_symbol;
    char *mon_decimal_point;
    char *mon_thousands_sep;
    char *mon_grouping;
    char *positive_sign;
    char *negative_sign;
    char int_frac_digits;
    char frac_digits;
    char p_cs_precedes;
    char p_sep_by_space;
    char n_cs_precedes;
    char n_sep_by_space;
    char p_sign_posn;
    char n_sign_posn;
} LConv;

unsigned short _ctype[257];
unsigned short *_pctype = _ctype + 1;
int __mb_cur_max = 1;

static LConv lconv;
static char dot[] = ".";
static char empty[] = "";

void crt_ctype_init(void)
{
    int c;
    unsigned short f;

    for (c = 0; c < 256; c++)
    {
        f = 0;
        if (c >= 'A' && c <= 'Z')
            f = C_UPPER | C_ALPHA;
        else if (c >= 'a' && c <= 'z')
            f = C_LOWER | C_ALPHA;
        else if (c >= '0' && c <= '9')
            f = C_DIGIT | C_HEX;
        else if (c == ' ')
            f = C_SPACE | C_BLANK;
        else if (c >= 9 && c <= 13)
            f = C_SPACE | C_CNTRL;
        else if (c < 32 || c == 127)
            f = C_CNTRL;
        else if (c < 127)
            f = C_PUNCT;
        if ((c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))
            f |= C_HEX;
        _ctype[c + 1] = f;
    }
    _pctype = _ctype + 1;
    lconv.decimal_point = dot;
    lconv.thousands_sep = empty;
    lconv.grouping = empty;
    lconv.int_curr_symbol = empty;
    lconv.currency_symbol = empty;
    lconv.mon_decimal_point = empty;
    lconv.mon_thousands_sep = empty;
    lconv.mon_grouping = empty;
    lconv.positive_sign = empty;
    lconv.negative_sign = empty;
    lconv.int_frac_digits = 127;
    lconv.frac_digits = 127;
    lconv.p_cs_precedes = 127;
    lconv.p_sep_by_space = 127;
    lconv.n_cs_precedes = 127;
    lconv.n_sep_by_space = 127;
    lconv.p_sign_posn = 127;
    lconv.n_sign_posn = 127;
}

static int cls(int c, int mask)
{
    return c >= -1 && c <= 255 ? _ctype[c + 1] & mask : 0;
}

int __cdecl isalpha(int c)
{
    return cls(c, C_ALPHA);
}

int __cdecl isupper(int c)
{
    return cls(c, C_UPPER);
}

int __cdecl islower(int c)
{
    return cls(c, C_LOWER);
}

int __cdecl isdigit(int c)
{
    return cls(c, C_DIGIT);
}

int __cdecl isxdigit(int c)
{
    return cls(c, C_HEX);
}

int __cdecl isspace(int c)
{
    return cls(c, C_SPACE);
}

int __cdecl ispunct(int c)
{
    return cls(c, C_PUNCT);
}

int __cdecl iscntrl(int c)
{
    return cls(c, C_CNTRL);
}

int __cdecl isalnum(int c)
{
    return cls(c, C_ALPHA | C_DIGIT);
}

int __cdecl isprint(int c)
{
    return cls(c, C_ALPHA | C_DIGIT | C_PUNCT | C_BLANK) &&
           c != '\t';
}

int __cdecl isgraph(int c)
{
    return cls(c, C_ALPHA | C_DIGIT | C_PUNCT);
}

int __cdecl __isascii(int c)
{
    return (unsigned)c < 128;
}

int __cdecl __toascii(int c)
{
    return c & 127;
}

int __cdecl _isctype(int c, int mask)
{
    return cls(c, mask);
}

int __cdecl toupper(int c)
{
    return c >= 'a' && c <= 'z' ? c - 32 : c;
}

int __cdecl tolower(int c)
{
    return c >= 'A' && c <= 'Z' ? c + 32 : c;
}

int __cdecl _toupper(int c)
{
    return c - 32;
}

int __cdecl _tolower(int c)
{
    return c + 32;
}

unsigned short *__cdecl __pctype_func(void)
{
    return _pctype;
}

int __cdecl ___mb_cur_max_func(void)
{
    return __mb_cur_max;
}

char *__cdecl setlocale(int category, const char *locale)
{
    if (locale == NULL || locale[0] == 0 ||
            (locale[0] == 'C' && locale[1] == 0))
        return "C";
    return NULL;
}

LConv *__cdecl localeconv(void)
{
    return &lconv;
}
