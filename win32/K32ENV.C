/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: the environment. The DOS environment is copied
 * at first use into one block "A=B\0C=D\0\0" (variables of any
 * length); every change builds a new block.
 */
#include "w32.h"

#define ENVMAX 32768UL
#define PAGE 0x1000UL

extern void *WINAPI VirtualAlloc(void *, DWORD, DWORD, DWORD);
extern BOOL WINAPI VirtualFree(void *, DWORD, DWORD);

static char *eblk;
static DWORD ehandle;
static DWORD elen;

static DWORD slen(const char *s)
{
    DWORD n = 0;

    while (s[n])
        n++;
    return n;
}

static char up(char c)
{
    return c >= 'a' && c <= 'z' ? (char)(c - 32) : c;
}

/* name (nul ended) equals the name part of the entry at e */
static int match(const char *e, const char *name)
{
    while (*name && *e != '=' && up(*e) == up(*name)) {
        e++;
        name++;
    }
    return *name == 0 && *e == '=';
}

/* new zero filled block of at least n bytes */
static char *newblock(DWORD n, DWORD *handle)
{
    return w32_pages((n + 2 * PAGE - 1) & ~(PAGE - 1), handle);
}

static int einit(void)
{
    char *psp, *env;
    struct dregs r;
    DWORD sel, n = 0;

    if (eblk)
        return 1;
    env = NULL;
    psp = w32_psp();
    if (psp) {
        sel = *(WORD *)(psp + 0x2C);
        w32_zero(&r, sizeof r);
        r.eax = 0x0006;
        r.ebx = sel;
        dpmi31(&r);
        if (!r.cf && sel)
            env = (char *)(((r.ecx & 0xFFFF) << 16) |
                           (r.edx & 0xFFFF));
    }
    if (env)
        while (n < ENVMAX && (env[n] || env[n + 1]))
            n++;
    eblk = newblock(n + 2, &ehandle);
    if (eblk == NULL)
        return 0;
    if (env && n)
        w32_copy(eblk, env, n);
    elen = n ? n + 1 : 0;
    return 1;
}

void w32_envfree(void)
{
    if (eblk)
        w32_freepages(ehandle);
    eblk = NULL;
}

static char *find(const char *name)
{
    char *e;

    for (e = eblk; *e; e += slen(e) + 1)
        if (match(e, name))
            return e;
    return NULL;
}

DWORD WINAPI GetEnvironmentVariableA(const char *name, char *buf,
                                     DWORD size)
{
    char *e;
    DWORD n;
    W32T("GetEnvironmentVariableA");

    if (name == NULL || !einit()) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    e = find(name);
    if (e == NULL) {
        w32_seterr(ERROR_ENVVAR_NOT_FOUND);
        return 0;
    }
    e += slen(name) + 1;
    n = slen(e);
    if (n >= size)
        return n + 1;
    w32_copy(buf, e, n + 1);
    return n;
}

BOOL WINAPI SetEnvironmentVariableA(const char *name,
                                    const char *value)
{
    char *nb, *e, *d;
    DWORD h, n, vl = 0;
    W32T("SetEnvironmentVariableA");

    if (name == NULL || name[0] == 0 || !einit()) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    for (n = 0; name[n]; n++)
        if (name[n] == '=') {
            w32_seterr(ERROR_INVALID_PARAMETER);
            return FALSE;
        }
    if (value)
        vl = slen(value);
    if (value == NULL && find(name) == NULL) {
        w32_seterr(ERROR_ENVVAR_NOT_FOUND);
        return FALSE;
    }
    if (elen + n + vl + 3 > ENVMAX * 2) {
        w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
        return FALSE;
    }
    nb = newblock(elen + n + vl + 3, &h);
    if (nb == NULL) {
        w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
        return FALSE;
    }
    d = nb;
    for (e = eblk; *e; e += slen(e) + 1)
        if (!match(e, name)) {
            w32_copy(d, e, slen(e) + 1);
            d += slen(e) + 1;
        }
    if (value) {
        w32_copy(d, name, n);
        d += n;
        *d++ = '=';
        w32_copy(d, value, vl + 1);
        d += vl + 1;
    }
    elen = (DWORD)(d - nb) + (d != nb);
    w32_freepages(ehandle);
    eblk = nb;
    ehandle = h;
    return TRUE;
}

/* narrow a wide string to the low bytes, n = characters or -1 */
static BOOL narrow(const WORD *w, char *out, DWORD max)
{
    DWORD i;

    for (i = 0; i < max; i++) {
        out[i] = (char)w[i];
        if (w[i] == 0)
            return TRUE;
    }
    return FALSE;
}

DWORD WINAPI GetEnvironmentVariableW(const WORD *name, WORD *buf,
                                     DWORD size)
{
    char n[40], v[ENVMAX / 64];
    DWORD len, i;
    W32T("GetEnvironmentVariableW");

    if (name == NULL || !narrow(name, n, 40)) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    len = GetEnvironmentVariableA(n, v, sizeof v);
    if (len >= sizeof v)
        len = sizeof v - 1;
    if (len == 0)
        return 0;
    if (len >= size)
        return len + 1;
    for (i = 0; i <= len; i++)
        buf[i] = (BYTE)v[i];
    return len;
}

BOOL WINAPI SetEnvironmentVariableW(const WORD *name,
                                    const WORD *value)
{
    char n[40], v[ENVMAX / 64];
    W32T("SetEnvironmentVariableW");

    if (name == NULL || !narrow(name, n, 40) ||
            (value && !narrow(value, v, sizeof v))) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    return SetEnvironmentVariableA(n, value ? v : NULL);
}

char *WINAPI GetEnvironmentStringsA(void)
{
    char *c;
    W32T("GetEnvironmentStringsA");

    if (!einit())
        return NULL;
    c = VirtualAlloc(NULL, elen + 2, 0x3000UL, 4);
    if (c)
        w32_copy(c, eblk, elen + 1);
    return c;
}

WORD *WINAPI GetEnvironmentStringsW(void)
{
    WORD *c;
    DWORD i;
    W32T("GetEnvironmentStringsW");

    if (!einit())
        return NULL;
    c = VirtualAlloc(NULL, 2 * (elen + 2), 0x3000UL, 4);
    if (c)
        for (i = 0; i <= elen; i++)
            c[i] = (BYTE)eblk[i];
    return c;
}

BOOL WINAPI FreeEnvironmentStringsA(char *p)
{
    W32T("FreeEnvironmentStringsA");
    return VirtualFree(p, 0, 0x8000UL);
}

BOOL WINAPI FreeEnvironmentStringsW(WORD *p)
{
    W32T("FreeEnvironmentStringsW");
    return VirtualFree(p, 0, 0x8000UL);
}
