/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: general utilities (qsort, bsearch, rand, abs, the
 * environment, integer to text, path splitting).
 */
#include "crt.h"

typedef int (__cdecl * CmpFn)(const void *, const void *);

extern DWORD WINAPI GetEnvironmentVariableA(const char *, char *,
        DWORD);
extern BOOL WINAPI SetEnvironmentVariableA(const char *,
        const char *);
extern DWORD WINAPI GetFullPathNameA(const char *, DWORD, char *,
                                     char **);
extern DWORD __cdecl strlen(const char *s);
extern char *__cdecl strcpy(char *d, const char *s);
extern char *__cdecl strchr(const char *s, int c);
extern char *__cdecl strrchr(const char *s, int c);
extern void *__cdecl memcpy(void *d, const void *s, DWORD n);

static unsigned long seed = 1;

typedef struct EnvCache {
    struct EnvCache *next;
    char text[1];
} EnvCache;

static EnvCache *envcache;

void crt_lib_free(void)
{
    EnvCache *c;

    while ((c = envcache) != NULL) {
        envcache = c->next;
        free(c);
    }
}

static void swap(char *a, char *b, DWORD size)
{
    char t;

    while (size--) {
        t = *a;
        *a++ = *b;
        *b++ = t;
    }
}

/* heap sort: no recursion, no extra memory */
static void sift(char *base, DWORD size, DWORD root, DWORD n,
                 CmpFn cmp)
{
    DWORD child;

    for (;;) {
        child = 2 * root + 1;
        if (child >= n)
            return;
        if (child + 1 < n && cmp(base + child * size,
                                 base + (child + 1) * size) < 0)
            child++;
        if (cmp(base + root * size, base + child * size) >= 0)
            return;
        swap(base + root * size, base + child * size, size);
        root = child;
    }
}

void __cdecl qsort(void *base, DWORD n, DWORD size, CmpFn cmp)
{
    char *b = base;
    DWORD i;

    if (n < 2 || size == 0)
        return;
    for (i = n / 2; i > 0; i--)
        sift(b, size, i - 1, n, cmp);
    for (i = n - 1; i > 0; i--) {
        swap(b, b + i * size, size);
        sift(b, size, 0, i, cmp);
    }
}

void *__cdecl bsearch(const void *key, const void *base, DWORD n,
                      DWORD size, CmpFn cmp)
{
    DWORD lo = 0, hi = n, mid;
    int r;

    while (lo < hi) {
        mid = lo + (hi - lo) / 2;
        r = cmp(key, (const char *)base + mid * size);
        if (r == 0)
            return (char *)base + mid * size;
        if (r < 0)
            hi = mid;
        else
            lo = mid + 1;
    }
    return NULL;
}

void __cdecl srand(unsigned s)
{
    seed = s;
}

int __cdecl rand(void)
{
    seed = seed * 214013UL + 2531011UL;
    return (int)((seed >> 16) & 0x7FFF);
}

int __cdecl abs(int v)
{
    return v < 0 ? -v : v;
}

long __cdecl labs(long v)
{
    return v < 0 ? -v : v;
}

char *__cdecl getenv(const char *name)
{
    char probe[2];
    DWORD n = GetEnvironmentVariableA(name, probe, 0);
    EnvCache *c;

    if (n == 0)
        return NULL;
    c = malloc(sizeof(EnvCache) + n);
    if (c == NULL)
        return NULL;
    GetEnvironmentVariableA(name, c->text, n + 1);
    c->next = envcache;
    envcache = c;
    return c->text;
}

int __cdecl _putenv(const char *s)
{
    char name[64];
    DWORD n = 0;

    while (s[n] && s[n] != '=' && n < 63) {
        name[n] = s[n];
        n++;
    }
    name[n] = 0;
    if (s[n] != '=' || n == 0)
        return -1;
    if (s[n + 1] == 0)
        SetEnvironmentVariableA(name, NULL);
    else if (!SetEnvironmentVariableA(name, s + n + 1))
        return -1;
    return 0;
}

int __cdecl system(const char *cmd)
{
    crt_errno = CRT_ENOENT;
    return -1;
}

static const char digits[] =
    "0123456789abcdefghijklmnopqrstuvwxyz";

static char *utoa_base(unsigned long v, char *buf, int radix,
                       int neg)
{
    char tmp[34];
    int n = 0, i = 0;

    if (radix < 2 || radix > 36) {
        buf[0] = 0;
        return buf;
    }
    do {
        tmp[n++] = digits[v % radix];
        v /= radix;
    } while (v);
    if (neg)
        buf[i++] = '-';
    while (n)
        buf[i++] = tmp[--n];
    buf[i] = 0;
    return buf;
}

char *__cdecl _ultoa(unsigned long v, char *buf, int radix)
{
    return utoa_base(v, buf, radix, 0);
}

char *__cdecl _ltoa(long v, char *buf, int radix)
{
    if (v < 0 && radix == 10)
        return utoa_base(0 - (unsigned long)v, buf, radix, 1);
    return utoa_base((unsigned long)v, buf, radix, 0);
}

char *__cdecl _itoa(int v, char *buf, int radix)
{
    return _ltoa(v, buf, radix);
}

char *__cdecl _fullpath(char *buf, const char *path, DWORD size)
{
    char tmp[300];
    DWORD n = GetFullPathNameA(path, sizeof tmp, tmp, NULL);

    if (n == 0 || n >= sizeof tmp)
        return NULL;
    if (buf == NULL)
        buf = malloc(n + 1 > size ? n + 1 : size);
    else if (n >= size) {
        crt_errno = CRT_ERANGE;
        return NULL;
    }
    if (buf)
        crt_copy(buf, tmp, n + 1);
    return buf;
}

void __cdecl _splitpath(const char *path, char *drive, char *dir,
                        char *fname, char *ext)
{
    const char *p = path, *slash, *dot;
    DWORD n;

    if (drive)
        drive[0] = 0;
    if (p[0] && p[1] == ':') {
        if (drive) {
            drive[0] = p[0];
            drive[1] = ':';
            drive[2] = 0;
        }
        p += 2;
    }
    slash = strrchr(p, '\\');
    dot = strrchr(p, '/');
    if (dot > slash)
        slash = dot;
    n = slash ? (DWORD)(slash + 1 - p) : 0;
    if (dir) {
        crt_copy(dir, p, n);
        dir[n] = 0;
    }
    p += n;
    dot = strrchr(p, '.');
    n = dot ? (DWORD)(dot - p) : strlen(p);
    if (fname) {
        crt_copy(fname, p, n);
        fname[n] = 0;
    }
    if (ext)
        strcpy(ext, dot ? dot : "");
}

void __cdecl _makepath(char *path, const char *drive,
                       const char *dir,
                       const char *fname, const char *ext)
{
    DWORD n = 0, k;

    path[0] = 0;
    if (drive && drive[0]) {
        path[n++] = drive[0];
        path[n++] = ':';
    }
    if (dir && dir[0]) {
        k = strlen(dir);
        crt_copy(path + n, dir, k);
        n += k;
        if (path[n - 1] != '\\' && path[n - 1] != '/')
            path[n++] = '\\';
    }
    if (fname) {
        k = strlen(fname);
        crt_copy(path + n, fname, k);
        n += k;
    }
    if (ext && ext[0]) {
        if (ext[0] != '.')
            path[n++] = '.';
        k = strlen(ext);
        crt_copy(path + n, ext, k);
        n += k;
    }
    path[n] = 0;
}
