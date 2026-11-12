/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 210: wsprintf, MessageBox, key state, system metrics
 * and the OEM conversions of USER32.DLL. Prints "USR: ok" or
 * "USR: fail NN".
 */
#include "tw32.h"

typedef unsigned short WCH;

extern int __cdecl wsprintfA(char *, const char *, ...);
extern int __cdecl wsprintfW(WCH *, const WCH *, ...);
extern int W wvsprintfA(char *, const char *, const DWORD *);
extern int W MessageBoxA(void *, const char *, const char *,
                         unsigned);
extern int W MessageBoxW(void *, const WCH *, const WCH *,
                         unsigned);
extern short W GetKeyState(int);
extern short W GetAsyncKeyState(int);
extern int W GetSystemMetrics(int);
extern int W CharToOemA(const char *, char *);
extern int W OemToCharA(const char *, char *);
extern int W CharToOemBuffA(const char *, char *, DWORD);
extern int W CharToOemW(const WCH *, char *);
extern int W OemToCharW(const char *, WCH *);

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

static int step;

static void fail(void)
{
    static char text[] = "\r\nUSR: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static int same(const char *a, const char *b)
{
    while (*a && *a == *b) {
        a++;
        b++;
    }
    return *a == *b;
}

static int wsame(const WCH *a, const WCH *b)
{
    while (*a && *a == *b) {
        a++;
        b++;
    }
    return *a == *b;
}

static void fmt_tests(void)
{
    char b[1200];
    WCH w[64];
    DWORD args[2];
    int i;

    CHECK(wsprintfA(b, "%d|%5d|%-5d|%05d", -42, 7, 7, 42) == 21);
    CHECK(same(b, "-42|    7|7    |00042"));
    wsprintfA(b, "%u %x %X %#x %lu", 7, 255, 255, 255,
              3000000000UL);
    CHECK(same(b, "7 ff FF 0xff 3000000000"));
    wsprintfA(b, "%s|%10s|%-6s|%.2s", "abc", "abc", "abc",
              "abc");
    CHECK(same(b, "abc|       abc|abc   |ab"));
    wsprintfA(b, "%c%c%%%s", 'A', 'B', (char *)0);
    CHECK(same(b, "AB%(null)"));
    wsprintfA(b, "%hd %08X %p", 65535, 0xBEEF, 0x1234);
    CHECK(same(b, "-1 0000BEEF 00001234"));
    args[0] = 5;
    args[1] = (DWORD)"x";
    CHECK(wvsprintfA(b, "%d %s", args) == 3 && same(b, "5 x"));
    for (i = 0; i < 1100; i++)
        b[i] = 'z';
    b[1100] = 0;
    CHECK(wsprintfA(b + 0, "%s", "q") == 1);
    {
        static char big[1200];

        for (i = 0; i < 1100; i++)
            big[i] = 'z';
        CHECK(wsprintfA(b, "%s", big) == 1023);
    }
    CHECK(wsprintfW(w, L"%s=%d", L"ab", 12) == 5);
    CHECK(wsame(w, L"ab=12"));
    wsprintfW(w, L"%S|%c", "xy", L'W');
    CHECK(wsame(w, L"xy|W"));
}

static void misc_tests(void)
{
    char o[16], b[16];
    WCH w[16];

    CHECK(MessageBoxA(0, "text", "cap", 0) == 1);
    CHECK(MessageBoxA(0, "text", "cap", 4) == 6);
    CHECK(MessageBoxA(0, "text", "cap", 2) == 3);
    CHECK(MessageBoxA(0, "text", "cap", 5) == 4);
    CHECK(MessageBoxW(0, L"wtext", L"wcap", 1) == 1);
    CHECK(GetKeyState(0x41) == 0 && GetAsyncKeyState(0x41) == 0);
    CHECK(GetSystemMetrics(0) == 640);
    CHECK(GetSystemMetrics(1) == 400);
    CHECK(GetSystemMetrics(99) == 0);
    CHECK(CharToOemA("AbC1", o) && same(o, "AbC1"));
    CHECK(CharToOemBuffA("xyz", o, 3) && o[2] == 'z');
    b[0] = (char)0xE9;
    b[1] = 0;
    CHECK(CharToOemA(b, o) && o[0] != 0);
    CHECK(OemToCharA(o, b) && (unsigned char)b[0] == 0xE9);
    CHECK(CharToOemW(L"Ab", o) && same(o, "Ab"));
    CHECK(OemToCharW("Ab", w) && wsame(w, L"Ab"));
}

void mainCRTStartup(void)
{
    fmt_tests();
    misc_tests();
    dos_print("\r\nUSR: ok\r\n$");
    ExitProcess(0);
}
