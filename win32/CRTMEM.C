/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: errno and the memory functions (on the process
 * heap of KERNEL32.DLL).
 */
#include "crt.h"

int crt_errno;
static int dos_errno;

int *__cdecl _errno(void)
{
    return &crt_errno;
}

DWORD *__cdecl __doserrno(void)
{
    return (DWORD *)&dos_errno;
}

/* Win32 error -> errno (and _doserrno) */
void crt_seterr_win(DWORD e)
{
    dos_errno = (int)e;
    switch (e) {
    case 2:
    case 3:
        crt_errno = CRT_ENOENT;
        break;
    case 4:
        crt_errno = CRT_EMFILE;
        break;
    case 5:
    case 19:
    case 32:
    case 33:
        crt_errno = CRT_EACCES;
        break;
    case 6:
        crt_errno = CRT_EBADF;
        break;
    case 8:
        crt_errno = CRT_ENOMEM;
        break;
    case 39:
    case 112:
        crt_errno = CRT_ENOSPC;
        break;
    case 80:
    case 183:
        crt_errno = CRT_EEXIST;
        break;
    default:
        crt_errno = CRT_EINVAL;
    }
}

void crt_copy(void *dst, const void *src, DWORD n)
{
    char *d = dst;
    const char *s = src;

    while (n--)
        *d++ = *s++;
}

void *__cdecl malloc(DWORD n)
{
    void *p = HeapAlloc(GetProcessHeap(), 0, n ? n : 1);

    if (p == NULL)
        crt_errno = CRT_ENOMEM;
    return p;
}

void *__cdecl calloc(DWORD count, DWORD size)
{
    void *p;

    if (size && count > 0xFFFFFFFFUL / size) {
        crt_errno = CRT_ENOMEM;
        return NULL;
    }
    p = HeapAlloc(GetProcessHeap(), 8,
                  count * size ? count * size : 1);
    if (p == NULL)
        crt_errno = CRT_ENOMEM;
    return p;
}

void __cdecl free(void *p)
{
    if (p)
        HeapFree(GetProcessHeap(), 0, p);
}

void *__cdecl realloc(void *p, DWORD n)
{
    void *q;

    if (p == NULL)
        return malloc(n);
    if (n == 0) {
        free(p);
        return NULL;
    }
    q = HeapReAlloc(GetProcessHeap(), 0, p, n);
    if (q == NULL)
        crt_errno = CRT_ENOMEM;
    return q;
}

DWORD __cdecl _msize(void *p)
{
    return HeapSize(GetProcessHeap(), 0, p);
}

void *__cdecl _expand(void *p, DWORD n)
{
    return HeapReAlloc(GetProcessHeap(), 0x10, p, n);
}
