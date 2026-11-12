/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 209: the TLS block of TTLSD.DLL. Prints "TLD: ok" or
 * "TLD: fail NN".
 */
#include "tw32.h"

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

extern char *W DlData(void);
extern DWORD *W DlLog(void);
extern DWORD W DlIndex(void);
extern DWORD W DlLen(void);

static int step;

static void fail(void)
{
    static char text[] = "\r\nTLD: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

void mainCRTStartup(void)
{
    char *p = DlData();
    DWORD *log = DlLog();
    int i;

    CHECK(p != 0 && DlIndex() != 0xFFFFUL);
    for (i = 0; i < 8; i++)
        CHECK(p[i] == "dll-tls"[i]);
    for (i = 0; i < 12; i++)
        CHECK(p[DlLen() + i] == 0);
    p[0] = 'X';
    CHECK(DlData()[0] == 'X');
    CHECK(log[0] == 0x101 && log[1] == 1);
    dos_print("\r\nTLD: ok\r\n$");
    ExitProcess(0);
}
