/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 204: TEB and PEB through FS. Prints "TEB: ok" or
 * "TEB: fail NN".
 */
#include "tw32.h"

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

extern DWORD fs_get(DWORD off);
#pragma aux fs_get = \
    "mov eax, fs:[eax]" \
    parm [eax] \
    value [eax];

extern void fs_put(DWORD off, DWORD v);
#pragma aux fs_put = \
    "mov fs:[eax], edx" \
    parm [eax] [edx] \
    modify [];

extern DWORD fs_sel(void);
#pragma aux fs_sel = \
    "xor eax, eax" \
    "mov ax, fs" \
    value [eax];

static int step;

static void fail(void)
{
    static char text[] = "\r\nTEB: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static void teb_tests(void)
{
    DWORD *peb, *slots, i, t, local = 0;
    DWORD out, self;

    CHECK(fs_sel() != 0);
    self = fs_get(0x18);
    CHECK(self != 0);
    CHECK(fs_get(0x30) == fs_get(0x18) + 0x1000);
    CHECK(fs_get(0) == 0xFFFFFFFFUL);
    CHECK(fs_get(4) > (DWORD)&local);
    CHECK(fs_get(8) < (DWORD)&local);
    CHECK(fs_get(0x20) == GetCurrentProcessId());
    CHECK(fs_get(0x24) == GetCurrentThreadId());
    peb = (DWORD *)fs_get(0x30);
    CHECK(peb != 0);
    CHECK(peb[2] == (DWORD)GetModuleHandleA(0));
    CHECK(peb[6] == (DWORD)GetProcessHeap());
    CHECK(peb[41] == 10);
    SetLastError(0x1234);
    CHECK(fs_get(0x34) == 0x1234);
    fs_put(0x34, 77);
    CHECK(GetLastError() == 77);
    i = TlsAlloc();
    CHECK(i != 0xFFFFFFFFUL);
    CHECK(i < 64);
    CHECK(TlsSetValue(i, (void *)5));
    CHECK(fs_get(0xE10 + 4 * i) == 5);
    CHECK(fs_get(0x2C) != 0);
    t = GetTickCount();
    do {
        CHECK(WriteFile(GetStdHandle((DWORD) - 11), "", 0, &out,
                        0) || 1);
    } while (GetTickCount() - t < 100);
    CHECK(fs_sel() != 0);
    CHECK(fs_get(0x18) == self);
}

void mainCRTStartup(void)
{
    teb_tests();
    dos_print("\r\nTEB: ok\r\n$");
    ExitProcess(0);
}
