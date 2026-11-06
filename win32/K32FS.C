/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: directories, names and attributes over DOS.
 * Every call tries the long file name function (INT 21h 71xxh)
 * first and the plain DOS function when the LFN API is absent.
 */
#include "w32.h"

extern DWORD WINAPI GetEnvironmentVariableA(const char *, char *,
        DWORD);
extern HANDLE WINAPI CreateFileA(const char *, DWORD, DWORD,
                                 void *,
                                 DWORD, DWORD, HANDLE);
extern BOOL WINAPI CloseHandle(HANDLE);
extern BOOL WINAPI ReadFile(HANDLE, void *, DWORD, DWORD *,
                            void *);
extern BOOL WINAPI WriteFile(HANDLE, const void *, DWORD,
                             DWORD *,
                             void *);

void w32_dos2(struct dregs *r, DWORD lfnax, DWORD plainax)
{
    struct dregs saved = *r;

    r->eax = lfnax;
    dos21(r);
    if (r->cf && (r->eax & 0xFFFF) == 0x7100)
    {
        *r = saved;
        r->eax = plainax;
        dos21(r);
    }
}

static DWORD slen(const char *s)
{
    DWORD n = 0;

    while (s[n])
        n++;
    return n;
}

/* sets the Win32 error from a failed DOS call, returns FALSE */
static BOOL dosfail(const struct dregs *r)
{
    w32_seterr(w32_doserr(r));
    return FALSE;
}

/* common shape: one name in DS:DX */
static BOOL byname(const char *name, DWORD lfnax, DWORD plainax)
{
    struct dregs r;

    if (name == NULL)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    w32_zero(&r, sizeof r);
    r.edx = (DWORD)name;
    w32_dos2(&r, lfnax, plainax);
    return r.cf ? dosfail(&r) : TRUE;
}

DWORD WINAPI GetFileAttributesA(const char *name)
{
    struct dregs r;
    W32T("GetFileAttributesA");

    if (name == NULL)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0xFFFFFFFFUL;
    }
    w32_zero(&r, sizeof r);
    r.edx = (DWORD)name;
    r.eax = 0x7143;
    r.ebx = 0;
    dos21(&r);
    if (r.cf && (r.eax & 0xFFFF) == 0x7100)
    {
        w32_zero(&r, sizeof r);
        r.edx = (DWORD)name;
        r.eax = 0x4300;
        dos21(&r);
    }
    if (r.cf)
    {
        dosfail(&r);
        return 0xFFFFFFFFUL;
    }
    return (r.ecx & 0xFFFF) ? (r.ecx & 0xFFFF) : 0x80UL;
}

BOOL WINAPI SetFileAttributesA(const char *name, DWORD attr)
{
    struct dregs r;
    W32T("SetFileAttributesA");

    if (name == NULL)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    attr &= 0x27;
    w32_zero(&r, sizeof r);
    r.edx = (DWORD)name;
    r.eax = 0x7143;
    r.ebx = 1;
    r.ecx = attr;
    dos21(&r);
    if (r.cf && (r.eax & 0xFFFF) == 0x7100)
    {
        w32_zero(&r, sizeof r);
        r.edx = (DWORD)name;
        r.eax = 0x4301;
        r.ecx = attr;
        dos21(&r);
    }
    return r.cf ? dosfail(&r) : TRUE;
}

BOOL WINAPI DeleteFileA(const char *name)
{
    W32T("DeleteFileA");
    return byname(name, 0x7141, 0x4100);
}

BOOL WINAPI CreateDirectoryA(const char *name, void *sa)
{
    W32T("CreateDirectoryA");
    if (byname(name, 0x7139, 0x3900))
        return TRUE;
    if (GetFileAttributesA(name) != 0xFFFFFFFFUL)
        w32_seterr(ERROR_ALREADY_EXISTS);
    return FALSE;
}

BOOL WINAPI RemoveDirectoryA(const char *name)
{
    W32T("RemoveDirectoryA");
    return byname(name, 0x713A, 0x3A00);
}

BOOL WINAPI MoveFileA(const char *from, const char *to)
{
    struct dregs r;
    W32T("MoveFileA");

    if (from == NULL || to == NULL)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    if (GetFileAttributesA(to) != 0xFFFFFFFFUL)
    {
        w32_seterr(ERROR_ALREADY_EXISTS);
        return FALSE;
    }
    w32_zero(&r, sizeof r);
    r.edx = (DWORD)from;
    r.edi = (DWORD)to;
    w32_dos2(&r, 0x7156, 0x5600);
    return r.cf ? dosfail(&r) : TRUE;
}

BOOL WINAPI CopyFileA(const char *from, const char *to,
                      BOOL noover)
{
    char buf[4096];
    HANDLE in, out;
    DWORD got, put;
    BOOL ok = TRUE;
    W32T("CopyFileA");

    in = CreateFileA(from, 0x80000000UL, 1, NULL, 3, 0, NULL);
    if (in == INVALID_HANDLE_VALUE)
        return FALSE;
    out = CreateFileA(to, 0x40000000UL, 0, NULL, noover ? 1 : 2,
                      0, NULL);
    if (out == INVALID_HANDLE_VALUE)
    {
        CloseHandle(in);
        return FALSE;
    }
    while (ok && ReadFile(in, buf, sizeof buf, &got, NULL) &&
            got)
        ok = WriteFile(out, buf, got, &put, NULL) && put == got;
    CloseHandle(in);
    CloseHandle(out);
    return ok;
}

DWORD WINAPI GetCurrentDirectoryA(DWORD size, char *buf)
{
    struct dregs r;
    char path[70];
    DWORD n;
    W32T("GetCurrentDirectoryA");

    w32_zero(&r, sizeof r);
    r.eax = 0x1900;
    dos21(&r);
    path[0] = (char)('A' + (r.eax & 0xFF));
    path[1] = ':';
    path[2] = '\\';
    w32_zero(&r, sizeof r);
    r.esi = (DWORD)path + 3;
    r.edx = 0;
    w32_dos2(&r, 0x7147, 0x4700);
    if (r.cf)
    {
        dosfail(&r);
        return 0;
    }
    n = slen(path);
    if (n >= size)
        return n + 1;
    w32_copy(buf, path, n + 1);
    return n;
}

BOOL WINAPI SetCurrentDirectoryA(const char *name)
{
    struct dregs r;
    W32T("SetCurrentDirectoryA");

    if (name == NULL)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    if (name[0] && name[1] == ':')
    {
        char d = name[0] >= 'a' ? (char)(name[0] - 32) : name[0];

        w32_zero(&r, sizeof r);
        r.eax = 0x0E00;
        r.edx = (DWORD)(d - 'A');
        dos21(&r);
    }
    return byname(name, 0x713B, 0x3B00);
}

DWORD WINAPI GetFullPathNameA(const char *name, DWORD size,
                              char *buf,
                              char **part)
{
    struct dregs r;
    char path[270];
    DWORD n, i, last;
    W32T("GetFullPathNameA");

    if (name == NULL)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    w32_zero(&r, sizeof r);
    r.esi = (DWORD)name;
    r.edi = (DWORD)path;
    r.ecx = 0;
    w32_dos2(&r, 0x7160, 0x6000);
    if (r.cf)
    {
        dosfail(&r);
        return 0;
    }
    n = slen(path);
    if (n >= size)
        return n + 1;
    w32_copy(buf, path, n + 1);
    if (part)
    {
        last = 0;
        for (i = 0; i < n; i++)
            if (path[i] == '\\')
                last = i + 1;
        *part = last < n ? buf + last : NULL;
    }
    return n;
}

DWORD WINAPI GetTempPathA(DWORD size, char *buf)
{
    char t[130];
    DWORD n;
    W32T("GetTempPathA");

    n = GetEnvironmentVariableA("TEMP", t, 120);
    if (n == 0 || n >= 120)
        n = GetEnvironmentVariableA("TMP", t, 120);
    if (n == 0 || n >= 120)
    {
        struct dregs r;

        w32_zero(&r, sizeof r);
        r.eax = 0x1900;
        dos21(&r);
        t[0] = (char)('A' + (r.eax & 0xFF));
        t[1] = ':';
        n = 2;
    }
    if (t[n - 1] != '\\')
        t[n++] = '\\';
    t[n] = 0;
    if (n >= size)
        return n + 1;
    w32_copy(buf, t, n + 1);
    return n;
}
