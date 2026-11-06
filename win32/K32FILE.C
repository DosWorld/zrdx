/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: file handles and I/O over DOS (LFN first).
 * A HANDLE is 4 * (DOS handle + 1); kernel objects >= 10000h.
 */
#include "w32.h"

#define GENERIC_READ 0x80000000UL
#define GENERIC_WRITE 0x40000000UL
#define GENERIC_ALL 0x10000000UL
#define IOCHUNK 4096UL

static HANDLE stdh[3];

HANDLE w32_hdos(int dos)
{
    return (HANDLE)(((DWORD)dos + 1) << 2);
}

int w32_dosh(HANDLE h)
{
    DWORD v = (DWORD)h;

    if ((v & 3) || v < 4 || v > 4UL * 256)
        return -1;
    return (int)(v >> 2) - 1;
}

DWORD w32_doserr(const struct dregs *r)
{
    return r->eax & 0xFFFF;
}

HANDLE WINAPI GetStdHandle(DWORD which)
{
    DWORD i = 0xFFFFFFF6UL - which;
    W32T("GetStdHandle");

    if (which > 0xFFFFFFF6UL || i > 2)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return INVALID_HANDLE_VALUE;
    }
    i = 2 - i;
    return stdh[i] ? stdh[i] : w32_hdos((int)i);
}

BOOL WINAPI SetStdHandle(DWORD which, HANDLE h)
{
    DWORD i = 0xFFFFFFF6UL - which;
    W32T("SetStdHandle");

    if (which > 0xFFFFFFF6UL || i > 2)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    stdh[2 - i] = h;
    return TRUE;
}

static int same(const char *a, const char *b)
{
    while (*a && *b)
    {
        char x = *a++, y = *b++;

        if (x >= 'a' && x <= 'z')
            x -= 32;
        if (x != y)
            return 0;
    }
    return *a == *b;
}

HANDLE WINAPI CreateFileA(const char *name, DWORD access,
                          DWORD share,
                          void *sa, DWORD disp, DWORD flags,
                          HANDLE tmpl)
{
    struct dregs r;
    DWORD action, mode, taken;
    int rd, wr;
    W32T("CreateFileA");

    if (name == NULL || disp < 1 || disp > 5)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return INVALID_HANDLE_VALUE;
    }
    action = disp == 1 ? 0x10 : disp == 2 ? 0x12 :
             disp == 3 ? 0x01 : disp == 4 ? 0x11 : 0x02;
    rd = (access & (GENERIC_READ | GENERIC_ALL | 1)) != 0;
    wr = (access & (GENERIC_WRITE | GENERIC_ALL | 2)) != 0;
    mode = rd && wr ? 2 : wr ? 1 : 0;
    mode |= (share & 3) == 0 ? 0x10 : (share & 3) == 1 ? 0x20 :
            (share & 3) == 2 ? 0x30 : 0x40;
    if (same(name, "CONOUT$") || same(name, "CONIN$"))
        name = "CON";
    w32_zero(&r, sizeof r);
    r.eax = 0x716C;
    r.ebx = mode;
    r.ecx = flags & 0x27;
    r.edx = action;
    r.esi = (DWORD)name;
    dos21(&r);
    if (r.cf && (r.eax & 0xFFFF) == 0x7100)
    {
        r.eax = 0x6C00;
        r.ebx = mode;
        r.ecx = flags & 0x27;
        r.edx = action;
        r.esi = (DWORD)name;
        dos21(&r);
    }
    if (r.cf)
    {
        w32_seterr(w32_doserr(&r));
        return INVALID_HANDLE_VALUE;
    }
    taken = r.ecx & 0xFFFF;
    if ((disp == 2 || disp == 4) && (taken == 1 || taken == 3))
        w32_seterr(ERROR_ALREADY_EXISTS);
    return w32_hdos((int)(r.eax & 0xFFFF));
}

BOOL WINAPI CloseHandle(HANDLE h)
{
    struct dregs r;
    int d;
    W32T("CloseHandle");

    if ((DWORD)h >= 0x10000UL)
        return w32_objclose(h);
    d = w32_dosh(h);
    if (d < 0)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    w32_zero(&r, sizeof r);
    r.eax = 0x3E00;
    r.ebx = (DWORD)d;
    dos21(&r);
    if (r.cf)
        w32_seterr(w32_doserr(&r));
    return !r.cf;
}

/* one DOS read (ah = 3Fh) or write (ah = 40h) in pieces */
static BOOL transfer(DWORD ah, HANDLE h, char *buf, DWORD n,
                     DWORD *done)
{
    struct dregs r;
    DWORD got, part;
    int d = w32_dosh(h);

    *done = 0;
    if (d < 0)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    while (*done < n)
    {
        part = n - *done > IOCHUNK ? IOCHUNK : n - *done;
        w32_zero(&r, sizeof r);
        r.eax = ah << 8;
        r.ebx = (DWORD)d;
        r.ecx = part;
        r.edx = (DWORD)(buf + *done);
        dos21(&r);
        if (r.cf)
        {
            w32_seterr(w32_doserr(&r));
            return FALSE;
        }
        got = r.eax & 0xFFFF;
        *done += got;
        if (got < part)
        {
            if (ah == 0x40)
            {
                w32_seterr(112);
                return FALSE;
            }
            break;
        }
    }
    return TRUE;
}

BOOL WINAPI WriteFile(HANDLE h, const void *buf, DWORD n,
                      DWORD *written, void *overlapped)
{
    DWORD done;
    BOOL ok = transfer(0x40, h, (char *)buf, n, &done);
    W32T("WriteFile");

    if (written)
        *written = done;
    return ok;
}

BOOL WINAPI ReadFile(HANDLE h, void *buf, DWORD n, DWORD *got,
                     void *overlapped)
{
    DWORD done;
    BOOL ok = transfer(0x3F, h, (char *)buf, n, &done);
    W32T("ReadFile");

    if (got)
        *got = done;
    return ok;
}

DWORD WINAPI GetFileType(HANDLE h)
{
    struct dregs r;
    int d = w32_dosh(h);
    W32T("GetFileType");

    if (d < 0)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return 0;
    }
    w32_zero(&r, sizeof r);
    r.eax = 0x4400;
    r.ebx = (DWORD)d;
    dos21(&r);
    if (r.cf)
    {
        w32_seterr(w32_doserr(&r));
        return 0;
    }
    return (r.edx & 0x80) ? 2 : 1;
}

DWORD WINAPI SetFilePointer(HANDLE h, long lo, long *hi,
                            DWORD method)
{
    struct dregs r;
    int d = w32_dosh(h);
    W32T("SetFilePointer");

    if (d < 0 || method > 2)
    {
        w32_seterr(d < 0 ? ERROR_INVALID_HANDLE :
                   ERROR_INVALID_PARAMETER);
        return 0xFFFFFFFFUL;
    }
    if (hi && *hi != (method != 0 && lo < 0 ? -1L : 0L))
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0xFFFFFFFFUL;
    }
    w32_zero(&r, sizeof r);
    r.eax = 0x4200 | method;
    r.ebx = (DWORD)d;
    r.ecx = ((DWORD)lo) >> 16;
    r.edx = ((DWORD)lo) & 0xFFFF;
    dos21(&r);
    if (r.cf)
    {
        w32_seterr(w32_doserr(&r));
        return 0xFFFFFFFFUL;
    }
    if (hi)
        *hi = 0;
    return ((r.edx & 0xFFFF) << 16) | (r.eax & 0xFFFF);
}

BOOL WINAPI FlushFileBuffers(HANDLE h)
{
    struct dregs r;
    int d = w32_dosh(h);
    W32T("FlushFileBuffers");

    if (d < 0)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    w32_zero(&r, sizeof r);
    r.eax = 0x6800;
    r.ebx = (DWORD)d;
    dos21(&r);
    return TRUE;
}
