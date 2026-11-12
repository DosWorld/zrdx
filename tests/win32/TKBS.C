/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 211: KERNELBASE.DLL forwards to KERNEL32.DLL. The calls
 * go through the import directive of TKBS.LNK. Prints "KBS: ok"
 * or "KBS: fail NN".
 */
#include "tw32.h"

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

extern DWORD W KbGetLastError(void);
extern void W KbSetLastError(DWORD);
extern DWORD W KbGetCurrentProcessId(void);
extern int W KbLstrlenA(const char *);
extern HANDLE W KbGetProcessHeap(void);

static int step;

static void fail(void)
{
    static char text[] = "\r\nKBS: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

void mainCRTStartup(void)
{
    KbSetLastError(0x1234);
    CHECK(KbGetLastError() == 0x1234);
    CHECK(GetLastError() == 0x1234);
    CHECK(KbGetCurrentProcessId() == GetCurrentProcessId());
    CHECK(KbLstrlenA("forward") == 7);
    CHECK(KbGetProcessHeap() == GetProcessHeap());
    dos_print("\r\nKBS: ok\r\n$");
    ExitProcess(0);
}
