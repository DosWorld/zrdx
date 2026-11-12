/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 214: SHELL32.DLL (imports through TSHL.LNK). Prints
 * "SHL: ok" or "SHL: fail NN".
 */
#include "tw32.h"

typedef unsigned short WCH;

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

extern WCH **W ShArgv(const WCH *, int *);
extern long W ShFolderA(HANDLE, int, HANDLE, DWORD, char *);
extern long W ShFolderW(HANDLE, int, HANDLE, DWORD, WCH *);
extern int W ShSpecialA(HANDLE, char *, int, int);
extern HANDLE W ShExecA(HANDLE, const char *, const char *,
                        const char *, const char *, int);
extern HANDLE W ShExecW(HANDLE, const WCH *, const WCH *,
                        const WCH *, const WCH *, int);
extern DWORD W GetCurrentDirectoryA(DWORD, char *);
extern HANDLE W LocalFree(HANDLE);

static int step;

static void fail(void)
{
    static char text[] = "\r\nSHL: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static int same(const WCH *a, const char *b)
{
    while (*a && *a == (unsigned char)*b) {
        a++;
        b++;
    }
    return *a == (unsigned char)*b;
}

/* parses cmd and checks the arguments (up to 4) */
static int args(const WCH *cmd, int want, const char *a0,
                const char *a1, const char *a2)
{
    int n = 0, ok;
    WCH **v = ShArgv(cmd, &n);

    ok = v != 0 && n == want && v[n] == 0;
    ok = ok && (want < 1 || same(v[0], a0));
    ok = ok && (want < 2 || same(v[1], a1));
    ok = ok && (want < 3 || same(v[2], a2));
    LocalFree(v);
    return ok;
}

static void tests(void)
{
    char a[300], b[300];
    WCH w[300];
    WCH **v;
    int n;

    CHECK(args(L"prog a b", 3, "prog", "a", "b"));
    CHECK(args(L"\"c:\\my dir\\x.exe\" \"a b\" c", 3,
               "c:\\my dir\\x.exe", "a b", "c"));
    CHECK(args(L"p a\\\\\\b d\"e f\"g h", 4, "p", "a\\\\\\b",
               "de fg"));
    CHECK(args(L"p a\\\\\\\"b c d", 4, "p", "a\\\"b", "c"));
    CHECK(args(L"p a\\\\\\\\\"b c\" d e", 4, "p", "a\\\\b c",
               "d"));
    CHECK(args(L"  x   ", 1, "x", 0, 0));
    v = ShArgv(L"", &n);
    CHECK(v != 0 && n == 1 && v[0][0] != 0);
    LocalFree(v);
    CHECK(ShArgv(0, &n) == 0);
    CHECK(ShFolderA(0, 5, 0, 0, a) == 0);
    GetCurrentDirectoryA(300, b);
    CHECK(a[0] == b[0] && a[1] == b[1]);
    CHECK(ShFolderA(0, 0x7F, 0, 0, a) == (long)0x80070057UL);
    CHECK(ShFolderW(0, 0x1A, 0, 0, w) == 0 && w[0] == (WCH)b[0]);
    CHECK(ShSpecialA(0, a, 0x28, 0) == 1);
    CHECK((DWORD)ShExecA(0, "open", "x.exe", 0, 0, 1) <= 32);
    CHECK((DWORD)ShExecW(0, L"open", L"x.exe", 0, 0, 1) <= 32);
}

void mainCRTStartup(void)
{
    tests();
    dos_print("\r\nSHL: ok\r\n$");
    ExitProcess(0);
}
