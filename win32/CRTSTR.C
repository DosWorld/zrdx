/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: string and memory functions (ISO C, C locale).
 */
#include "crt.h"

static char *tok_save;

int __cdecl toupper(int c);
int __cdecl tolower(int c);

DWORD __cdecl strlen(const char *s)
{
    DWORD n = 0;

    while (s[n])
        n++;
    return n;
}

void *__cdecl memcpy(void *d, const void *s, DWORD n)
{
    crt_copy(d, s, n);
    return d;
}

void *__cdecl memmove(void *d, const void *s, DWORD n)
{
    char *dp = d;
    const char *sp = s;

    if (dp < sp || dp >= sp + n)
        crt_copy(d, s, n);
    else
        while (n--)
            dp[n] = sp[n];
    return d;
}

void *__cdecl memset(void *d, int c, DWORD n)
{
    char *p = d;

    while (n--)
        *p++ = (char)c;
    return d;
}

int __cdecl memcmp(const void *a, const void *b, DWORD n)
{
    const unsigned char *x = a, *y = b;

    for (; n; n--, x++, y++)
        if (*x != *y)
            return *x < *y ? -1 : 1;
    return 0;
}

int __cdecl _memicmp(const void *a, const void *b, DWORD n)
{
    const unsigned char *x = a, *y = b;
    int p, q;

    for (; n; n--, x++, y++)
    {
        p = tolower(*x);
        q = tolower(*y);
        if (p != q)
            return p < q ? -1 : 1;
    }
    return 0;
}

void *__cdecl memchr(const void *s, int c, DWORD n)
{
    const unsigned char *p = s;

    for (; n; n--, p++)
        if (*p == (unsigned char)c)
            return (void *)p;
    return NULL;
}

char *__cdecl strcpy(char *d, const char *s)
{
    char *r = d;

    while ((*d++ = *s++) != 0)
        ;
    return r;
}

char *__cdecl strncpy(char *d, const char *s, DWORD n)
{
    DWORD i;

    for (i = 0; i < n && s[i]; i++)
        d[i] = s[i];
    for (; i < n; i++)
        d[i] = 0;
    return d;
}

char *__cdecl strcat(char *d, const char *s)
{
    strcpy(d + strlen(d), s);
    return d;
}

char *__cdecl strncat(char *d, const char *s, DWORD n)
{
    char *e = d + strlen(d);

    while (n-- && *s)
        *e++ = *s++;
    *e = 0;
    return d;
}

int __cdecl strcmp(const char *a, const char *b)
{
    while (*a && *a == *b)
    {
        a++;
        b++;
    }
    return (unsigned char) * a < (unsigned char) * b ? -1 :
           (unsigned char) * a > (unsigned char) * b;
}

int __cdecl strncmp(const char *a, const char *b, DWORD n)
{
    for (; n; n--, a++, b++)
    {
        if (*a != *b)
            return (unsigned char) * a < (unsigned char) * b
                ? -1 : 1;
        if (*a == 0)
            break;
    }
    return 0;
}

int __cdecl _stricmp(const char *a, const char *b)
{
    int p, q;

    for (;; a++, b++)
    {
        p = tolower((unsigned char) * a);
        q = tolower((unsigned char) * b);
        if (p != q)
            return p < q ? -1 : 1;
        if (p == 0)
            return 0;
    }
}

int __cdecl _strnicmp(const char *a, const char *b, DWORD n)
{
    int p, q;

    for (; n; n--, a++, b++)
    {
        p = tolower((unsigned char) * a);
        q = tolower((unsigned char) * b);
        if (p != q)
            return p < q ? -1 : 1;
        if (p == 0)
            break;
    }
    return 0;
}

int __cdecl strcoll(const char *a, const char *b)
{
    return strcmp(a, b);
}

DWORD __cdecl strxfrm(char *d, const char *s, DWORD n)
{
    DWORD len = strlen(s);

    if (n > len)
        strcpy(d, s);
    return len;
}

char *__cdecl strchr(const char *s, int c)
{
    for (;; s++)
    {
        if (*s == (char)c)
            return (char *)s;
        if (*s == 0)
            return NULL;
    }
}

char *__cdecl strrchr(const char *s, int c)
{
    const char *r = NULL;

    for (;; s++)
    {
        if (*s == (char)c)
            r = s;
        if (*s == 0)
            return (char *)r;
    }
}

char *__cdecl strstr(const char *h, const char *n)
{
    DWORD len = strlen(n);

    if (len == 0)
        return (char *)h;
    for (; *h; h++)
        if (*h == *n && strncmp(h, n, len) == 0)
            return (char *)h;
    return NULL;
}

DWORD __cdecl strspn(const char *s, const char *set)
{
    DWORD n = 0;

    while (s[n] && strchr(set, s[n]))
        n++;
    return n;
}

DWORD __cdecl strcspn(const char *s, const char *set)
{
    DWORD n = 0;

    while (s[n] && !strchr(set, s[n]))
        n++;
    return n;
}

char *__cdecl strpbrk(const char *s, const char *set)
{
    s += strcspn(s, set);
    return *s ? (char *)s : NULL;
}

char *__cdecl strtok(char *s, const char *delim)
{
    char *start;

    if (s == NULL)
        s = tok_save;
    if (s == NULL)
        return NULL;
    s += strspn(s, delim);
    if (*s == 0)
    {
        tok_save = NULL;
        return NULL;
    }
    start = s;
    s += strcspn(s, delim);
    if (*s)
    {
        *s = 0;
        tok_save = s + 1;
    }
    else
        tok_save = NULL;
    return start;
}

char *__cdecl _strdup(const char *s)
{
    DWORD n = strlen(s) + 1;
    char *p = malloc(n);

    if (p)
        crt_copy(p, s, n);
    return p;
}

char *__cdecl _strlwr(char *s)
{
    char *p;

    for (p = s; *p; p++)
        *p = (char)tolower((unsigned char) * p);
    return s;
}

char *__cdecl _strupr(char *s)
{
    char *p;

    for (p = s; *p; p++)
        *p = (char)toupper((unsigned char) * p);
    return s;
}

char *__cdecl _strrev(char *s)
{
    DWORD i = 0, j = strlen(s);
    char t;

    while (i + 1 < j)
    {
        t = s[i];
        s[i++] = s[--j];
        s[j] = t;
    }
    return s;
}

char *__cdecl _strset(char *s, int c)
{
    char *p;

    for (p = s; *p; p++)
        *p = (char)c;
    return s;
}

char *__cdecl _strnset(char *s, int c, DWORD n)
{
    char *p;

    for (p = s; *p && n; p++, n--)
        *p = (char)c;
    return s;
}

void __cdecl _swab(char *s, char *d, int n)
{
    int i;

    for (i = 0; i + 1 < n; i += 2)
    {
        d[i] = s[i + 1];
        d[i + 1] = s[i];
    }
}

char *__cdecl strerror(int e)
{
    switch (e)
    {
    case 0:
        return "No error";
    case 1:
        return "Operation not permitted";
    case 2:
        return "No such file or directory";
    case 9:
        return "Bad file descriptor";
    case 12:
        return "Not enough space";
    case 13:
        return "Permission denied";
    case 17:
        return "File exists";
    case 22:
        return "Invalid argument";
    case 24:
        return "Too many open files";
    case 28:
        return "No space left on device";
    case 33:
        return "Domain error";
    case 34:
        return "Result too large";
    }
    return "Unknown error";
}

char *__cdecl _strerror(const char *prefix)
{
    static char buf[160];
    char *m = strerror(crt_errno);

    buf[0] = 0;
    if (prefix && *prefix)
    {
        strncat(buf, prefix, 100);
        strcat(buf, ": ");
    }
    strcat(buf, m);
    strcat(buf, "\n");
    return buf;
}
