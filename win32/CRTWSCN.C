/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: wide scanf family, fgetws, _wfopen and the wide
 * start-up vectors (__wgetmainargs). Input is read as bytes; the
 * wide format is narrowed as for printf (CRTWFMT.C).
 */
#include "crt.h"

typedef WORD wchar;

extern void *__cdecl malloc(DWORD);
extern void __cdecl free(void *);
extern int __cdecl fgetc(FILE *);
extern int __cdecl ungetc(int, FILE *);
extern FILE *__cdecl fopen(const char *, const char *);
extern int __cdecl __getmainargs(int *, char ***, char ***, int,
                                 void *);

wchar **_wenviron;
static wchar **wargv;
static int wargc;

static int scan_with(FILE * f, const char *s, const wchar * fmt,
                     va_list ap)
{
    char *nf = crt_narrow_fmt(fmt);
    int n;

    if (!nf)
        return -1;
    n = f ? crt_vfscanf(f, nf, ap) : crt_vsscanf(s, nf, ap);
    free(nf);
    return n;
}

int __cdecl vfwscanf(FILE * f, const wchar * fmt, va_list ap)
{
    return scan_with(f, NULL, fmt, ap);
}

int __cdecl fwscanf(FILE * f, const wchar * fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = scan_with(f, NULL, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl wscanf(const wchar * fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = scan_with(&_iob[0], NULL, fmt, ap);
    va_end(ap);
    return n;
}

int __cdecl swscanf(const wchar * s, const wchar * fmt, ...)
{
    va_list ap;
    char *ns;
    DWORD i, len = 0;
    int n;

    while (s[len])
        len++;
    ns = malloc(len + 1);
    if (!ns)
        return -1;
    for (i = 0; i <= len; i++)
        ns[i] = (char)(s[i] < 0x100 ? s[i] : '?');
    va_start(ap, fmt);
    n = scan_with(NULL, ns, fmt, ap);
    va_end(ap);
    free(ns);
    return n;
}

wchar *__cdecl fgetws(wchar * buf, int count, FILE * f)
{
    int i = 0, c = 0;

    while (i + 1 < count && c != '\n' && (c = fgetc(f)) != EOF)
        buf[i++] = (wchar) (unsigned char)c;
    buf[i] = 0;
    return i ? buf : NULL;
}

FILE *__cdecl _wfopen(const wchar * path, const wchar * mode)
{
    char p[270], m[16];
    int i;

    if (!WideCharToMultiByte(0, 0, path, -1, p, 270, 0, 0))
        return NULL;
    for (i = 0; mode[i] && i < 15; i++)
        m[i] = (char)mode[i];
    m[i] = 0;
    return fopen(p, m);
}

/* wide copy of a narrow string vector, one block */
static wchar **widen(char **v)
{
    DWORD n = 0, total = 0, i;
    wchar **w, *p;

    while (v[n])
        total += (DWORD) lstrlenA(v[n++]) + 1;
    w = malloc((n + 1) * 4 + total * 2);
    if (!w)
        return NULL;
    p = (wchar *) (w + n + 1);
    for (i = 0; i < n; i++) {
        w[i] = p;
        p += MultiByteToWideChar(0, 0, v[i], -1, p,
                                 lstrlenA(v[i]) + 1);
    }
    w[n] = NULL;
    return w;
}

void crt_wide_free(void)
{
    free(wargv);
    free(_wenviron);
    wargv = NULL;
    _wenviron = NULL;
}

int __cdecl __wgetmainargs(int *argc, wchar *** argv,
                           wchar *** env, int wildcard,
                           void *startinfo)
{
    char **a, **e;
    int n;

    if (__getmainargs(&n, &a, &e, wildcard, startinfo))
        return -1;
    if (!wargv)
        wargv = widen(a);
    if (!_wenviron)
        _wenviron = widen(e);
    if (!wargv || !_wenviron)
        return -1;
    wargc = n;
    *argc = n;
    *argv = wargv;
    *env = _wenviron;
    return 0;
}

wchar ***__cdecl __p___wargv(void)
{
    return &wargv;
}

int *__cdecl __p___wargc(void)
{
    return &wargc;
}
