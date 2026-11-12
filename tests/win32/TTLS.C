/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 207: static TLS of the main program. ADDTLS.EXE adds the
 * directory below to the PE file. Prints "TLS: ok" or
 * "TLS: fail NN".
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

#define RAWLEN 9
#define ZEROLEN 20

static char raw[RAWLEN] = "tls-data";
static char rawend;
static DWORD tls_index = 0xFFFFUL;
static int cbseen = -1;

/* runs before the entry point (task 208) */
static void __stdcall tls_cb(void *h, DWORD reason, void *rsv)
{
    if (reason == 1)
        cbseen = 1;
}

static void *callbacks[2] = {(void *)tls_cb, 0};

/* IMAGE_TLS_DIRECTORY32 after the mark of ADDTLS */
static struct {
    char mark[8];
    DWORD start, end, index, callbacks, zero, flags;
} dir = {
    "TLSDIR01", (DWORD)raw, (DWORD)&rawend, (DWORD)&tls_index,
    (DWORD)callbacks, ZEROLEN, 0
};

static int step;

static void fail(void)
{
    static char text[] = "\r\nTLS: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static void tests(void)
{
    char *blk;
    DWORD i, len = (DWORD)&rawend - (DWORD)raw;

    CHECK(cbseen == 1);
    CHECK(tls_index == 0);
    CHECK(fs_get(0x2C) != 0);
    blk = (char *)((DWORD *)fs_get(0x2C))[tls_index];
    CHECK(blk != raw && blk != 0);
    for (i = 0; i < RAWLEN; i++)
        CHECK(blk[i] == raw[i]);
    for (i = 0; i < ZEROLEN; i++)
        CHECK(blk[len + i] == 0);
    blk[0] = 'X';
    CHECK(raw[0] == 't');
}

void mainCRTStartup(void)
{
    tests();
    dos_print("\r\nTLS: ok\r\n$");
    ExitProcess(0);
}
