/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * NTDLL.DLL: the Rtl and Nt functions that console programs
 * import, over KERNEL32.DLL (docs/WIN32.MD, task 212).
 */
#include "w32.h"

#define STATUS_SUCCESS 0L
#define STATUS_INVALID_PARAMETER 0xC000000DUL
#define STATUS_INVALID_HANDLE 0xC0000008UL

W32_ANCHOR;

extern DWORD WINAPI GetLastError(void);
extern void WINAPI SetLastError(DWORD error);
extern void *WINAPI HeapAlloc(HANDLE heap, DWORD flags,
                              DWORD size);
extern BOOL WINAPI HeapFree(HANDLE heap, DWORD flags, void *p);
extern void *WINAPI HeapReAlloc(HANDLE heap, DWORD flags,
                                void *p, DWORD size);
extern void WINAPI RtlUnwind(void *target, void *ip, void *rec,
                             DWORD value);

typedef struct OsVersion {
    DWORD size, major, minor, build, platform;
    WORD csd[128];
    WORD spmajor, spminor, suite;
    BYTE product, reserved;
} OsVersion;

void WINAPI NtUnwind(void *target, void *ip, void *rec,
                     DWORD value)
{
    RtlUnwind(target, ip, rec, value);
}

DWORD WINAPI RtlGetLastWin32Error(void)
{
    return GetLastError();
}

void WINAPI RtlSetLastWin32Error(DWORD error)
{
    SetLastError(error);
}

void WINAPI RtlFillMemory(void *dst, DWORD len, BYTE fill)
{
    BYTE *p = dst;

    while (len--)
        *p++ = fill;
}

void WINAPI RtlZeroMemory(void *dst, DWORD len)
{
    RtlFillMemory(dst, len, 0);
}

void WINAPI RtlMoveMemory(void *dst, const void *src, DWORD len)
{
    BYTE *d = dst;
    const BYTE *s = src;

    if (d <= s || d >= s + len) {
        while (len--)
            *d++ = *s++;
    } else {
        while (len--)
            d[len] = s[len];
    }
}

void *WINAPI RtlAllocateHeap(HANDLE heap, DWORD flags,
                             DWORD size)
{
    return HeapAlloc(heap, flags, size);
}

BYTE WINAPI RtlFreeHeap(HANDLE heap, DWORD flags, void *p)
{
    return (BYTE)(HeapFree(heap, flags, p) != 0);
}

void *WINAPI RtlReAllocateHeap(HANDLE heap, DWORD flags, void *p,
                               DWORD size)
{
    return HeapReAlloc(heap, flags, p, size);
}

/* OSVERSIONINFOW (size 276) or the EXW form (size 284) */
long WINAPI RtlGetVersion(OsVersion *v)
{
    DWORD size;

    if (v == NULL || v->size < 276)
        return (long)STATUS_INVALID_PARAMETER;
    size = v->size;
    RtlZeroMemory(v, size > sizeof *v ? sizeof *v : size);
    v->size = size;
    v->major = 10;
    v->build = 10240;
    v->platform = 2;
    if (size >= 284)
        v->product = 1;
    return STATUS_SUCCESS;
}

long WINAPI NtTerminateProcess(HANDLE process, long status)
{
    if (process != NULL && process != (HANDLE) - 1L)
        return (long)STATUS_INVALID_HANDLE;
    ExitProcess((UINT)status);
    return STATUS_SUCCESS;
}
