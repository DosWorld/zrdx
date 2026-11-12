/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * USER32.DLL: MessageBox (text on the console, no window),
 * key state, system metrics of the text screen and the OEM
 * character conversions (through the code pages of KERNEL32).
 */
#include "w32.h"

#define CP_ACP 0
#define CP_OEMCP 1
#define IDOK 1
#define IDCANCEL 2
#define IDABORT 3
#define IDRETRY 4
#define IDYES 6
#define SM_CXSCREEN 0
#define SM_CYSCREEN 1

extern HANDLE WINAPI GetStdHandle(DWORD id);
extern BOOL WINAPI WriteFile(HANDLE h, const void *buf, DWORD n,
                             DWORD *done, void *ov);
extern int WINAPI MultiByteToWideChar(UINT, DWORD, const char *,
                                      int, WORD *, int);
extern int WINAPI WideCharToMultiByte(UINT, DWORD, const WORD *,
                                      int, char *, int,
                                      const char *, int *);

W32_ANCHOR;

/* default button of a MB_* type: IDOK, IDABORT, IDYES, ... */
static int first_button(UINT type)
{
    static const BYTE first[7] = {
        IDOK, IDOK, IDABORT, IDYES, IDYES, IDRETRY, IDCANCEL
    };

    return first[(type & 0xF) < 7 ? type & 0xF : 0];
}

static void out(const char *s, int n)
{
    DWORD done;

    WriteFile(GetStdHandle((DWORD) - 11), s, (DWORD)n, &done, 0);
}

static int alen(const char *s)
{
    int n = 0;

    while (s[n])
        n++;
    return n;
}

static int box(const char *text, const char *caption, UINT type)
{
    out("[", 1);
    out(caption, alen(caption));
    out("] ", 2);
    out(text, alen(text));
    out("\r\n", 2);
    return first_button(type);
}

int WINAPI MessageBoxA(HANDLE wnd, const char *text,
                       const char *caption, UINT type)
{
    return box(text ? text : "", caption ? caption : "Error",
               type);
}

int WINAPI MessageBoxExA(HANDLE wnd, const char *text,
                         const char *caption, UINT type,
                         WORD lang)
{
    return MessageBoxA(wnd, text, caption, type);
}

/* wide text to the OEM code page, at most 1023 bytes */
static void to_oem(const WORD *w, char *buf)
{
    int n = 0;

    if (w)
        n = WideCharToMultiByte(CP_OEMCP, 0, w, -1, buf, 1024, 0,
                                0);
    if (n <= 0)
        buf[0] = 0;
}

int WINAPI MessageBoxW(HANDLE wnd, const WORD *text,
                       const WORD *caption, UINT type)
{
    char t[1024], c[1024];

    to_oem(text, t);
    to_oem(caption, c);
    return box(t, caption ? c : "Error", type);
}

int WINAPI MessageBoxExW(HANDLE wnd, const WORD *text,
                         const WORD *caption, UINT type,
                         WORD lang)
{
    return MessageBoxW(wnd, text, caption, type);
}

short WINAPI GetKeyState(int key)
{
    return 0;
}

short WINAPI GetAsyncKeyState(int key)
{
    return 0;
}

/* the 80x25 text screen as 640x400 pixels, the rest is 0 */
int WINAPI GetSystemMetrics(int index)
{
    if (index == SM_CXSCREEN)
        return 640;
    return index == SM_CYSCREEN ? 400 : 0;
}

/* converts n bytes one by one through UTF-16 */
static void convert(const char *src, char *dst, DWORD n,
                    UINT from, UINT to)
{
    WORD w;

    while (n--) {
        if (MultiByteToWideChar(from, 0, src, 1, &w, 1) != 1 ||
                WideCharToMultiByte(to, 0, &w, 1, dst, 1, 0,
                                    0) != 1)
            *dst = *src;
        src++;
        dst++;
    }
}

BOOL WINAPI CharToOemBuffA(const char *src, char *dst, DWORD n)
{
    convert(src, dst, n, CP_ACP, CP_OEMCP);
    return TRUE;
}

BOOL WINAPI OemToCharBuffA(const char *src, char *dst, DWORD n)
{
    convert(src, dst, n, CP_OEMCP, CP_ACP);
    return TRUE;
}

BOOL WINAPI CharToOemA(const char *src, char *dst)
{
    return CharToOemBuffA(src, dst, (DWORD)alen(src) + 1);
}

BOOL WINAPI OemToCharA(const char *src, char *dst)
{
    return OemToCharBuffA(src, dst, (DWORD)alen(src) + 1);
}

BOOL WINAPI CharToOemBuffW(const WORD *src, char *dst, DWORD n)
{
    return WideCharToMultiByte(CP_OEMCP, 0, src, (int)n, dst,
                               (int)n, 0, 0) != 0 || n == 0;
}

BOOL WINAPI OemToCharBuffW(const char *src, WORD *dst, DWORD n)
{
    return MultiByteToWideChar(CP_OEMCP, 0, src, (int)n, dst,
                               (int)n) != 0 || n == 0;
}

BOOL WINAPI CharToOemW(const WORD *src, char *dst)
{
    int n = 0;

    while (src[n])
        n++;
    return CharToOemBuffW(src, dst, (DWORD)n + 1);
}

BOOL WINAPI OemToCharW(const char *src, WORD *dst)
{
    return OemToCharBuffW(src, dst, (DWORD)alen(src) + 1);
}
