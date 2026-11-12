/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 213: ADVAPI32.DLL (imports through TADV.LNK). Prints
 * "ADV: ok" or "ADV: fail NN".
 */
#include "tw32.h"

typedef unsigned short WCH;

#define HKLM ((HANDLE) 0x80000002UL)

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

extern int W AdUserA(char *, DWORD *);
extern int W AdUserW(WCH *, DWORD *);
extern long W AdOpenA(HANDLE, const char *, DWORD, DWORD,
                      HANDLE *);
extern long W AdOpenW(HANDLE, const WCH *, DWORD, DWORD,
                      HANDLE *);
extern long W AdQueryA(HANDLE, const char *, DWORD *, DWORD *,
                       unsigned char *, DWORD *);
extern long W AdClose(HANDLE);
extern int W AdToken(HANDLE, DWORD, HANDLE *);
extern int W AdInfo(HANDLE, DWORD, void *, DWORD, DWORD *);
extern unsigned char W AdRandom(void *, DWORD);

static int step;

static void fail(void)
{
    static char text[] = "\r\nADV: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static void tests(void)
{
    char a[40];
    WCH w[40];
    DWORD n, i, same = 0;
    HANDLE h = (HANDLE) 1;
    unsigned char r[32];

    SetEnvironmentVariableA("USERNAME", "tester");
    n = sizeof a;
    CHECK(AdUserA(a, &n) && n == 7);
    CHECK(a[0] == 't' && a[5] == 'r');
    n = 3;
    SetLastError(0);
    CHECK(!AdUserA(a, &n) && n == 7 && GetLastError() == 122);
    n = 40;
    CHECK(AdUserW(w, &n) && n == 7 && w[0] == 't' && w[6] == 0);
    SetEnvironmentVariableA("USERNAME", 0);
    n = sizeof a;
    CHECK(AdUserA(a, &n) && n == 5 && a[0] == 'u' && a[4] == 0);
    CHECK(AdOpenA(HKLM, "Software", 0, 1, &h) == 2);
    CHECK(h == 0);
    CHECK(AdOpenW(HKLM, L"Software", 0, 1, &h) == 2);
    CHECK(AdQueryA(HKLM, "x", 0, 0, 0, 0) == 2);
    CHECK(AdClose(HKLM) == 0);
    SetLastError(0);
    CHECK(!AdToken((HANDLE) - 1L, 8, &h) && GetLastError() == 5);
    SetLastError(0);
    CHECK(!AdInfo(0, 1, a, 4, &n) && GetLastError() == 5);
    for (i = 0; i < sizeof r; i++)
        r[i] = 0;
    CHECK(AdRandom(r, sizeof r) == 1);
    for (i = 1; i < sizeof r; i++)
        same += r[i] == r[i - 1];
    CHECK(same < 4);
}

void mainCRTStartup(void)
{
    tests();
    dos_print("\r\nADV: ok\r\n$");
    ExitProcess(0);
}
