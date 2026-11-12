/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 188: one check per exported function of the console, text
 * and character groups (KERNEL32.DLL and USER32.DLL). Prints
 * "TXT: ok" or "TXT: fail NN". The standard output is redirected
 * by the test script, so the console calls use CONOUT$.
 */
#include "tw32.h"

typedef unsigned short W16;

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

static int step;

static void fail(void)
{
    static char text[] = "\r\nTXT: fail 00\r\n$";

    text[13] = (char)('0' + step / 10);
    text[14] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static void console_tests(void)
{
    HANDLE out, in;
    DWORD mode, n;
    unsigned short info[11];
    W16 wide[4];

    out = CreateFileA("CONOUT$", 0xC0000000UL, 3, 0, 3, 0, 0);
    CHECK(out != (HANDLE) - 1L);
    CHECK(GetConsoleMode(out, &mode) && (mode & 1));
    CHECK(SetConsoleMode(out, mode));
    CHECK(!GetConsoleMode(GetStdHandle(0xFFFFFFF5UL), &mode));
    CHECK(GetLastError() == 6);
    CHECK(GetConsoleScreenBufferInfo(out, info));
    CHECK(info[0] >= 40 && info[0] <= 255 && info[1] >= 20);
    CHECK(info[7] == info[0] - 1 && info[8] == info[1] - 1);
    CHECK(SetConsoleTextAttribute(out, 0x0F));
    CHECK(SetConsoleCursorPosition(out, 0));
    CHECK(WriteConsoleA(out, "A", 1, &n, 0) && n == 1);
    wide[0] = 'B';
    wide[1] = 0xE9;
    CHECK(WriteConsoleW(out, wide, 2, &n, 0) && n == 2);
    CHECK(SetConsoleTitleA("zrdx"));
    in = CreateFileA("CONIN$", 0xC0000000UL, 3, 0, 3, 0, 0);
    CHECK(in != (HANDLE) - 1L && FlushConsoleInputBuffer(in));
    CHECK(GetConsoleMode(in, &mode) && (mode & 2));
    CHECK(SetConsoleCP(437) && GetConsoleCP() == 437);
    CHECK(SetConsoleOutputCP(850)
          && GetConsoleOutputCP() == 850);
    CHECK(!SetConsoleOutputCP(99999) && GetLastError() == 87);
    CHECK(CloseHandle(in) && CloseHandle(out));
}

static void convert_tests(void)
{
    W16 w[8], e9 = 0xE9, euro = 0x20AC, smile[2];
    char b[8];
    BOOL used;

    smile[0] = 0xD83D;
    smile[1] = 0xDE00;
    CHECK(WideCharToMultiByte(65001, 0, &e9, 1, b, 8, 0,
                              0) == 2);
    CHECK((unsigned char)b[0] == 0xC3 &&
          (unsigned char)b[1] == 0xA9);
    CHECK(WideCharToMultiByte(65001, 0, &euro, 1, b, 8, 0,
                              0) == 3);
    CHECK((unsigned char)b[0] == 0xE2 &&
          (unsigned char)b[2] == 0xAC);
    CHECK(WideCharToMultiByte(65001, 0, smile, 2, b, 8, 0,
                              0) == 4);
    CHECK((unsigned char)b[0] == 0xF0 &&
          (unsigned char)b[3] == 0x80);
    CHECK(WideCharToMultiByte(437, 0, &e9, 1, b, 8, 0,
                              &used) == 1);
    CHECK((unsigned char)b[0] == 0x82 && !used);
    CHECK(WideCharToMultiByte(437, 0, &euro, 1, b, 8, 0,
                              &used) == 1);
    CHECK(b[0] == '?' && used);
    CHECK(WideCharToMultiByte(1252, 0, &euro, 1, b, 8, 0,
                              0) == 1);
    CHECK((unsigned char)b[0] == 0x80);
    CHECK(WideCharToMultiByte(1252, 0, smile, 2, b, 0, 0,
                              0) == 1);
    CHECK(WideCharToMultiByte(65001, 0, &euro, 1, b, 2, 0,
                              0) == 0);
    CHECK(GetLastError() == 122);
    CHECK(MultiByteToWideChar(437, 0, "\x82", 1, w, 8) == 1 &&
          w[0] == 0xE9);
    CHECK(MultiByteToWideChar(1252, 0, "\x80", 1, w, 8) == 1 &&
          w[0] == 0x20AC);
    CHECK(MultiByteToWideChar(1251, 0, "\xC0", 1, w, 8) == 1 &&
          w[0] == 0x410);
    CHECK(MultiByteToWideChar(1, 0, "A", 1, w, 8) == 1 &&
          w[0] == 'A');
}

static void class_tests(void)
{
    W16 w[8], t[8];
    char s[8];

    w[0] = 'a';
    w[1] = 'A';
    w[2] = '5';
    w[3] = ' ';
    w[4] = '!';
    w[5] = 0xE9;
    w[6] = 0x1F;
    w[7] = 'f';
    CHECK(GetStringTypeW(1, w, 8, t));
    CHECK(t[0] == 0x182 && t[1] == 0x181);
    CHECK(t[2] == 0x84 && t[3] == 0x48 && t[4] == 0x10);
    CHECK(t[5] == 0x102 && t[6] == 0x20);
    CHECK(t[7] == 0x182);
    CHECK(!GetStringTypeW(2, w, 1, t));
    s[0] = 'z';
    s[1] = 'Z';
    s[2] = '7';
    CHECK(GetStringTypeA(0, 1, s, 3, t));
    CHECK(t[0] == 0x102 && t[1] == 0x101 && t[2] == 0x84);
}

static void map_tests(void)
{
    W16 w[8], o[8], greek = 0x3B1, cyr = 0x44F, yd = 0xFF;
    char s[8], r[8];

    w[0] = 0xE4;
    w[1] = 'q';
    w[2] = 0;
    CHECK(LCMapStringW(0, 0x200, w, 3, o, 8) == 3);
    CHECK(o[0] == 0xC4 && o[1] == 'Q' && o[2] == 0);
    CHECK(LCMapStringW(0, 0x200, w, -1, 0, 0) == 3);
    CHECK(LCMapStringW(0, 0x200, w, 3, o, 2) == 0);
    CHECK(GetLastError() == 122);
    CHECK(LCMapStringW(0, 0x200, &greek, 1, o, 8) == 1 &&
          o[0] == 0x391);
    CHECK(LCMapStringW(0, 0x200, &cyr, 1, o, 8) == 1 &&
          o[0] == 0x42F);
    CHECK(LCMapStringW(0, 0x200, &yd, 1, o, 8) == 1 &&
          o[0] == 0x178);
    CHECK(LCMapStringW(0, 0x100, o, 1, w, 8) == 1
          && w[0] == 0xFF);
    CHECK(!LCMapStringW(0, 0x300, w, 1, o, 8));
    s[0] = 'a';
    s[1] = (char)0xE9;
    s[2] = 0;
    CHECK(LCMapStringA(0, 0x200, s, 3, r, 8) == 3);
    CHECK(r[0] == 'A' && (unsigned char)r[1] == 0xC9);
    CHECK(LCMapStringA(0, 0x100, r, 3, r, 8) == 3
          && r[0] == 'a');
}

static void compare_tests(void)
{
    W16 a[4], b[4];
    char d[8];

    a[0] = 'a';
    a[1] = 0;
    b[0] = 'B';
    b[1] = 0;
    CHECK(CompareStringW(0, 0, a, -1, b, -1) == 3);
    CHECK(CompareStringW(0, 1, a, -1, b, -1) == 1);
    b[0] = 'A';
    CHECK(CompareStringW(0, 1, a, -1, b, -1) == 2);
    CHECK(CompareStringW(0, 0, a, 1, b, 1) == 3);
    CHECK(CompareStringA(0, 0, "abc", -1, "abd", -1) == 1);
    CHECK(CompareStringA(0, 1, "ABC", 3, "abc", -1) == 2);
    CHECK(CompareStringA(0, 0, "", 0, "x", 1) == 1);
    CHECK(lstrlenA("hello") == 5 && lstrlenW(a) == 1);
    CHECK(lstrlenA(0) == 0);
    lstrcpyA(d, "abc");
    lstrcatA(d, "de");
    CHECK(lstrlenA(d) == 5 && lstrcmpA(d, "abcde") == 0);
    CHECK(lstrcmpA("a", "b") < 0 && lstrcmpA("b", "a") > 0);
    lstrcpynA(d, "123456", 4);
    CHECK(lstrlenA(d) == 3 && d[2] == '3');
    CHECK(lstrcmpiA("ABC", "abc") == 0 && lstrcmpiA("a",
            "B") < 0);
    CHECK(lstrcmpW(a, b) > 0 && lstrcmpiW(a, b) == 0);
    lstrcpyW(b, a);
    CHECK(b[0] == 'a' && b[1] == 0);
    CHECK(!IsDBCSLeadByte(0x81));
}

static void user_tests(void)
{
    char s[8];
    W16 w[4];

    s[0] = 'A';
    s[1] = (char)0xC9;
    s[2] = 'b';
    s[3] = 0;
    CHECK(CharLowerA(s) == s && s[0] == 'a');
    CHECK((unsigned char)s[1] == 0xE9);
    CHECK(CharUpperA(s) == s && s[2] == 'B');
    CHECK((DWORD)CharLowerA((char *)'Q') == 'q');
    w[0] = 0x430;
    w[1] = 'z';
    w[2] = 0;
    CHECK(CharUpperW(w) == w && w[0] == 0x410 && w[1] == 'Z');
    CHECK(CharLowerW(w) == w && w[0] == 0x430);
    CHECK((DWORD)CharUpperW((W16 *) 0x61) == 0x41);
    CHECK(CharUpperBuffA(s, 3) == 3 && CharLowerBuffA(s,
            3) == 3);
    CHECK(CharUpperBuffW(w, 2) == 2 && w[1] == 'Z');
    CHECK(CharLowerBuffW(w, 2) == 2 && w[1] == 'z');
    CHECK(CharUpperBuffA(s, 0) == 0);
    CHECK(IsCharAlphaA('a') && !IsCharAlphaA('1'));
    CHECK(IsCharAlphaW(0x430) && !IsCharAlphaW(' '));
    CHECK(IsCharAlphaNumericA('7') && !IsCharAlphaNumericA('-'));
    CHECK(IsCharAlphaNumericW('x') && !IsCharAlphaNumericW('.'));
    CHECK(IsCharUpperA('Q') && !IsCharUpperA('q'));
    CHECK(IsCharLowerA('q') && !IsCharLowerA('Q'));
    CHECK(IsCharUpperW(0x410) && !IsCharUpperW(0x430));
    CHECK(IsCharLowerW(0x430) && !IsCharLowerW(0x410));
    CHECK(CharNextA(s) == s + 1 && CharNextA(s + 3) == s + 3);
    CHECK(CharPrevA(s, s + 2) == s + 1 && CharPrevA(s, s) == s);
}

void mainCRTStartup(void)
{
    console_tests();
    convert_tests();
    class_tests();
    map_tests();
    compare_tests();
    user_tests();
    dos_print("\r\nTXT: ok\r\n$");
    ExitProcess(0);
}
