/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 179: one check per exported function of KERNEL32.DLL and
 * USER32.DLL, written from the documented behaviour. Prints
 * "API: ok" or "API: fail X" (X = the number of the check).
 * Built without a C runtime.
 */
#include "tw32.h"

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

extern void dos_del(const char *name);
#pragma aux dos_del = \
    "mov ah, 41h" \
    "int 21h" \
    parm [edx] \
    modify [eax];

static int step;

static int same(const char *a, const char *b)
{
    while (*a && *a == *b)
    {
        a++;
        b++;
    }
    return *a == *b;
}

static int len(const char *s)
{
    int n = 0;

    while (s[n])
        n++;
    return n;
}

static void fail(void)
{
    static char text[] = "\r\nAPI: fail 00\r\n$";

    text[12] = (char)('0' + step / 10);
    text[13] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static void process_tests(void)
{
    char buf[200], small[4];
    unsigned short wbuf[200];
    HANDLE h, k;
    DWORD n;
    void *old;
    DWORD(W * gv)(void);

    SetLastError(1234);
    CHECK(GetLastError() == 1234);
    CHECK((GetVersion() & 0xFF) == 5 &&
          !(GetVersion() & 0x80000000UL));
    CHECK(GetCurrentThreadId() == 1);
    old = SetUnhandledExceptionFilter((void *)0x1000);
    CHECK(SetUnhandledExceptionFilter(old) == (void *)0x1000);
    CHECK(UnhandledExceptionFilter(0) == 1);
    CHECK(SetConsoleCtrlHandler(0, 1));
    h = GetModuleHandleA(0);
    CHECK(h != 0 && *(unsigned short *)h == 0x5A4D);
    k = GetModuleHandleA("KERNEL32");
    CHECK(k != 0 && k == GetModuleHandleA("kernel32.dll"));
    CHECK(GetModuleHandleA("NOSUCH.DLL") == 0 &&
          GetLastError() == 126);
    CHECK(GetProcAddress(k, "GetVersion") != 0);
    gv = (DWORD(W *)(void))GetProcAddress(k, "GetVersion");
    CHECK(gv() == GetVersion());
    CHECK(GetProcAddress(k, "NoSuchFunction") == 0 &&
          GetLastError() == 127);
    CHECK(LoadLibraryA("KERNEL32") == k);
    CHECK(LoadLibraryA("NOSUCH") == 0 && GetLastError() == 126);
    CHECK(FreeLibrary(k));
    n = GetModuleFileNameA(0, buf, 200);
    CHECK(n > 7 && n == (DWORD)len(buf) && buf[n - 1] == 'E');
    CHECK(GetModuleFileNameA(0, small, 4) == 4 &&
          GetLastError() == 122);
    CHECK(GetModuleFileNameW(0, wbuf, 200) == n &&
          wbuf[0] == buf[0]);
    CHECK(len(GetCommandLineA()) >= 7);
    CHECK(GetCommandLineW()[0] == (unsigned char)
          GetCommandLineA()[0]);
    CHECK(GetACP() == 1252 && GetOEMCP() > 0);
}

static void file_tests(void)
{
    HANDLE f, o;
    DWORD n;
    char b[16];
    long hi = 0;

    f = CreateFileA("K32.TMP", 0xC0000000UL, 0, 0, 2, 0x80, 0);
    CHECK(f != (HANDLE) - 1L);
    CHECK(GetFileType(f) == 1);
    CHECK(WriteFile(f, "hello", 5, &n, 0) && n == 5);
    CHECK(FlushFileBuffers(f));
    CHECK(SetFilePointer(f, 0, 0, 2) == 5);
    CHECK(SetFilePointer(f, 1, &hi, 0) == 1);
    CHECK(ReadFile(f, b, 16, &n, 0) && n == 4 && b[0] == 'e');
    CHECK(ReadFile(f, b, 16, &n, 0) && n == 0);
    CHECK(CloseHandle(f));
    CHECK(!CloseHandle(f) && GetLastError() == 6);
    CHECK(CreateFileA("K32.TMP", 0x40000000UL, 0, 0, 1, 0, 0) ==
          (HANDLE) - 1L && GetLastError() == 80);
    o = CreateFileA("K32NONE.TMP", 0x80000000UL, 0, 0, 3, 0, 0);
    CHECK(o == (HANDLE) - 1L && GetLastError() == 2);
    o = CreateFileA("K32.TMP", 0x80000000UL, 1, 0, 4, 0, 0);
    CHECK(o != (HANDLE) - 1L && GetLastError() == 183);
    CHECK(CloseHandle(o));
    dos_del("K32.TMP");
    o = GetStdHandle(0xFFFFFFF5UL);
    CHECK(o != (HANDLE) - 1L);
    CHECK(SetStdHandle(0xFFFFFFF5UL, o));
    CHECK(GetStdHandle(0xFFFFFFF5UL) == o);
    CHECK(GetStdHandle(7) == (HANDLE) - 1L);
    CHECK(WriteFile(o, "", 0, &n, 0) && n == 0);
}

static void memory_tests(void)
{
    char *p, *q;
    DWORD info[7], i, n;
    HANDLE e;

    p = VirtualAlloc(0, 5000, 0x3000UL, 4);
    CHECK(p != 0 && ((DWORD)p & 0xFFF) == 0);
    for (i = 0; i < 5000; i++)
        if (p[i])
            fail();
    p[0] = 1;
    p[4999] = 2;
    CHECK(VirtualQuery(p + 100, info, 28) == 28);
    CHECK(info[0] == (DWORD)p && info[1] == (DWORD)p &&
          info[3] == 8192 && info[4] == 0x1000UL);
    q = VirtualAlloc(p, 100, 0x1000UL, 4);
    CHECK(q == p);
    CHECK(VirtualAlloc(0, 0, 0x1000UL, 4) == 0 &&
          GetLastError() == 87);
    CHECK(!VirtualFree(p, 8192, 0x8000UL));
    CHECK(VirtualFree(p, 0, 0x8000UL));
    CHECK(!VirtualFree(p, 0, 0x8000UL) && GetLastError() == 487);
    e = CreateEventA(0, 1, 0, 0);
    CHECK((DWORD)e >= 0x10000UL);
    CHECK(CloseHandle(e));
    CHECK(!CloseHandle(e) && GetLastError() == 6);
    CHECK(SetEnvironmentVariableA("ZRDXT", "1"));
    CHECK(SetEnvironmentVariableA("ZRDXT", 0));
    CHECK(!SetEnvironmentVariableA("ZRDXT", 0) &&
          GetLastError() == 203);
    CHECK(!SetEnvironmentVariableA("A=B", "1") &&
          GetLastError() == 87);
    n = 0;
    (void)n;
}

static void text_tests(void)
{
    unsigned short w[16];
    DWORD cp[5];
    char s[8];
    int i;

    CHECK(GetCPInfo(437, cp) && cp[0] == 1);
    CHECK(GetCPInfo(65001, cp) && cp[0] == 4);
    CHECK(!GetCPInfo(99999, cp) && GetLastError() == 87);
    CHECK(MultiByteToWideChar(0, 0, "abc", -1, 0, 0) == 4);
    CHECK(MultiByteToWideChar(0, 0, "abc", -1, w, 16) == 4 &&
          w[0] == 'a' && w[3] == 0);
    CHECK(MultiByteToWideChar(65001, 0, "\xC3\xA9", 2, w, 16) ==
          1 && w[0] == 0xE9);
    CHECK(MultiByteToWideChar(65001, 0, "\xC3", 1, w, 16) == 1 &&
          w[0] == 0xFFFD);
    CHECK(MultiByteToWideChar(65001, 8, "\xC3", 1, w, 16) == 0 &&
          GetLastError() == 1113);
    CHECK(MultiByteToWideChar(0, 0, "abcdef", -1, w, 2) == 0 &&
          GetLastError() == 122);
    for (i = 0; i < 4; i++)
        s[i] = "ab1\xE9"[i];
    s[4] = 0;
    CHECK(CharUpperA(s) == s && same(s, "AB1\xC9"));
    CHECK((DWORD)CharUpperA((char *)'q') == 'Q');
}

void mainCRTStartup(void)
{
    process_tests();
    file_tests();
    memory_tests();
    text_tests();
    dos_print("\r\nAPI: ok\r\n$");
    ExitProcess(0);
}
