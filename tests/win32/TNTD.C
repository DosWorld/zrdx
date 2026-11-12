/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 212: NTDLL.DLL (imports through TNTD.LNK). Prints
 * "NTD: ok" or "NTD: fail NN".
 */
#include "tw32.h"

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

extern DWORD W NtGetLastError(void);
extern void W NtSetLastError(DWORD);
extern void W NtFill(void *, DWORD, unsigned char);
extern void W NtZero(void *, DWORD);
extern void W NtMove(void *, const void *, DWORD);
extern void *W NtAlloc(HANDLE, DWORD, DWORD);
extern unsigned char W NtFree(HANDLE, DWORD, void *);
extern void *W NtRealloc(HANDLE, DWORD, void *, DWORD);
extern long W NtGetVersion(void *);
extern long W NtTerminate(HANDLE, long);
extern void W NtUnwind(void *, void *, void *, DWORD);

static int step;

static void fail(void)
{
    static char text[] = "\r\nNTD: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static void tests(void)
{
    unsigned char b[16];
    DWORD ver[71];
    HANDLE heap = GetProcessHeap();
    char *p;
    int i;

    NtSetLastError(77);
    CHECK(NtGetLastError() == 77 && GetLastError() == 77);
    NtFill(b, 16, 0x5A);
    CHECK(b[0] == 0x5A && b[15] == 0x5A);
    NtZero(b, 8);
    CHECK(b[7] == 0 && b[8] == 0x5A);
    for (i = 0; i < 16; i++)
        b[i] = (unsigned char)i;
    NtMove(b + 2, b, 8);
    CHECK(b[2] == 0 && b[9] == 7 && b[10] == 10);
    NtMove(b, b + 2, 8);
    CHECK(b[0] == 0 && b[7] == 7);
    p = NtAlloc(heap, 0, 32);
    CHECK(p != 0);
    p[31] = 'x';
    p = NtRealloc(heap, 0, p, 64);
    CHECK(p != 0 && p[31] == 'x');
    CHECK(NtFree(heap, 0, p) == 1);
    ver[0] = 276;
    CHECK(NtGetVersion(ver) == 0 && ver[0] == 276);
    CHECK(ver[1] == 10);
    CHECK(ver[3] == 10240 && ver[4] == 2);
    ver[0] = 10;
    CHECK(NtGetVersion(ver) == (long)0xC000000DUL);
    CHECK(NtTerminate((HANDLE) 5, 1) == (long)0xC0000008UL);
    NtUnwind(0, 0, 0, 0);
}

void mainCRTStartup(void)
{
    tests();
    dos_print("\r\nNTD: ok\r\n$");
    NtTerminate((HANDLE) - 1, 0);
}
