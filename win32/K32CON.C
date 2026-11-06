/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: the console. A console handle is a DOS handle of
 * a character device; a redirected handle is not a console (the
 * mode calls fail with ERROR_INVALID_HANDLE, as programs expect
 * when they test for redirection).
 */
#include "w32.h"

typedef struct Csbi
{
    WORD cols, rows;
    WORD curx, cury;
    WORD attr;
    WORD left, top, right, bottom;
    WORD maxcols, maxrows;
} Csbi;

extern BOOL WINAPI WriteFile(HANDLE, const void *, DWORD,
                             DWORD *,
                             void *);
extern BOOL WINAPI ReadFile(HANDLE, void *, DWORD, DWORD *,
                            void *);
extern int WINAPI WideCharToMultiByte(UINT, DWORD, const WORD *,
                                      int,
                                      char *, int, const char *,
                                      BOOL *);
extern int WINAPI MultiByteToWideChar(UINT, DWORD, const char *,
                                      int,
                                      WORD *, int);
extern UINT WINAPI GetOEMCP(void);

static DWORD modes[8];
static DWORD attrs[8];
static UINT incp, outcp;

/* DOS handle of a console device, -1 if h is not one */
static int console(HANDLE h)
{
    struct dregs r;
    int d = w32_dosh(h);

    if (d < 0 || d > 7)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return -1;
    }
    w32_zero(&r, sizeof r);
    r.eax = 0x4400;
    r.ebx = (DWORD)d;
    dos21(&r);
    if (r.cf || !(r.edx & 0x80))
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return -1;
    }
    return d;
}

BOOL WINAPI GetConsoleMode(HANDLE h, DWORD *mode)
{
    int d = console(h);
    W32T("GetConsoleMode");

    if (d < 0 || mode == NULL)
        return FALSE;
    if (modes[d] == 0)
        modes[d] = d == 0 ? 7 : 3;
    *mode = modes[d];
    return TRUE;
}

BOOL WINAPI SetConsoleMode(HANDLE h, DWORD mode)
{
    int d = console(h);
    W32T("SetConsoleMode");

    if (d < 0)
        return FALSE;
    modes[d] = mode ? mode : 0x80000000UL;
    return TRUE;
}

BOOL WINAPI GetConsoleScreenBufferInfo(HANDLE h, Csbi *info)
{
    int d = console(h);
    W32T("GetConsoleScreenBufferInfo");

    if (d < 0 || info == NULL)
        return FALSE;
    info->cols = *(WORD *)0x44A;
    info->rows = (WORD)(*(BYTE *)0x484 + 1);
    if (info->cols == 0)
        info->cols = 80;
    if (info->rows < 2)
        info->rows = 25;
    info->curx = *(BYTE *)0x450;
    info->cury = *(BYTE *)0x451;
    info->attr = (WORD)(attrs[d] ? attrs[d] : 7);
    info->left = 0;
    info->top = 0;
    info->right = (WORD)(info->cols - 1);
    info->bottom = (WORD)(info->rows - 1);
    info->maxcols = info->cols;
    info->maxrows = info->rows;
    return TRUE;
}

BOOL WINAPI SetConsoleTextAttribute(HANDLE h, WORD attr)
{
    int d = console(h);
    W32T("SetConsoleTextAttribute");

    if (d < 0)
        return FALSE;
    attrs[d] = attr ? attr : 0x8000UL;
    return TRUE;
}

BOOL WINAPI SetConsoleCursorPosition(HANDLE h, DWORD pos)
{
    struct dregs r;
    W32T("SetConsoleCursorPosition");

    if (console(h) < 0)
        return FALSE;
    w32_zero(&r, sizeof r);
    r.eax = 0x0200;
    r.edx = ((pos >> 16) << 8) | (pos & 0xFF);
    {
        extern void int10(struct dregs * r);
#pragma aux int10 = \
    "push ebp" \
    "mov ebp, eax" \
    "mov eax, [ebp]" \
    "mov ebx, [ebp + 4]" \
    "mov edx, [ebp + 12]" \
    "int 10h" \
    "pop ebp" \
    parm [eax] \
    modify [eax ebx ecx edx esi edi];
        int10(&r);
    }
    return TRUE;
}

UINT WINAPI GetConsoleCP(void)
{
    W32T("GetConsoleCP");
    return incp ? incp : GetOEMCP();
}

UINT WINAPI GetConsoleOutputCP(void)
{
    W32T("GetConsoleOutputCP");
    return outcp ? outcp : GetOEMCP();
}

BOOL WINAPI SetConsoleCP(UINT cp)
{
    W32T("SetConsoleCP");
    if (w32_cptab(cp) == NULL && cp != 28591 && cp != 65001)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    incp = cp;
    return TRUE;
}

BOOL WINAPI SetConsoleOutputCP(UINT cp)
{
    W32T("SetConsoleOutputCP");
    if (w32_cptab(cp) == NULL && cp != 28591 && cp != 65001)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    outcp = cp;
    return TRUE;
}

BOOL WINAPI SetConsoleTitleA(const char *title)
{
    W32T("SetConsoleTitleA");
    return title != NULL;
}

BOOL WINAPI FlushConsoleInputBuffer(HANDLE h)
{
    W32T("FlushConsoleInputBuffer");
    return console(h) >= 0;
}

BOOL WINAPI WriteConsoleA(HANDLE h, const void *buf, DWORD n,
                          DWORD *written, void *reserved)
{
    W32T("WriteConsoleA");
    if (console(h) < 0)
        return FALSE;
    return WriteFile(h, buf, n, written, NULL);
}

BOOL WINAPI WriteConsoleW(HANDLE h, const WORD *buf, DWORD n,
                          DWORD *written, void *reserved)
{
    char tmp[256];
    DWORD done = 0, part, got, put;
    int k;
    W32T("WriteConsoleW");

    if (console(h) < 0)
        return FALSE;
    while (done < n)
    {
        part = n - done > 128 ? 128 : n - done;
        k = WideCharToMultiByte(GetConsoleOutputCP(), 0, buf
            + done,
                                (int)part, tmp, sizeof tmp, NULL,
                                    NULL);
        if (k == 0 || !WriteFile(h, tmp, (DWORD)k, &put, NULL))
            return FALSE;
        done += part;
    }
    if (written)
        *written = done;
    (void)got;
    return TRUE;
}

BOOL WINAPI ReadConsoleA(HANDLE h, void *buf, DWORD n,
                         DWORD *got,
                         void *ctl)
{
    W32T("ReadConsoleA");
    if (console(h) < 0)
        return FALSE;
    return ReadFile(h, buf, n, got, NULL);
}

BOOL WINAPI ReadConsoleW(HANDLE h, WORD *buf, DWORD n,
                         DWORD *got,
                         void *ctl)
{
    char tmp[256];
    DWORD k;
    W32T("ReadConsoleW");

    if (console(h) < 0)
        return FALSE;
    if (n > sizeof tmp)
        n = sizeof tmp;
    if (!ReadFile(h, tmp, n, &k, NULL))
        return FALSE;
    if (k)
        MultiByteToWideChar(GetConsoleCP(), 0, tmp, (int)k, buf,
                            (int)n);
    if (got)
        *got = k;
    return TRUE;
}
