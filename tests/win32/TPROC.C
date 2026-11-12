/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 185: one check per exported function of the environment,
 * time and process groups. Prints "PROC: ok" or "PROC: fail NN".
 */
#include "tw32.h"

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

static int step;

static void fail(void)
{
    static char text[] = "\r\nPROC: fail 00\r\n$";

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

static int in_block(const char *blk, const char *entry)
{
    while (*blk) {
        if (same(blk, entry))
            return 1;
        while (*blk)
            blk++;
        blk++;
    }
    return 0;
}

static void env_tests(void)
{
    char buf[400], big[300];
    unsigned short wn[8], wv[8], wb[8];
    char *blk;
    unsigned short *wblk;
    int i;

    CHECK(SetEnvironmentVariableA("ZRDXA", "hello"));
    CHECK(GetEnvironmentVariableA("zrdxa", buf, 400) == 5);
    CHECK(same(buf, "hello"));
    CHECK(GetEnvironmentVariableA("ZRDXA", buf, 3) == 6);
    CHECK(GetEnvironmentVariableA("ZRDXNONE", buf, 400) == 0 &&
          GetLastError() == 203);
    for (i = 0; i < 299; i++)
        big[i] = (char)('a' + i % 26);
    big[299] = 0;
    CHECK(SetEnvironmentVariableA("ZRDXB", big));
    CHECK(GetEnvironmentVariableA("ZRDXB", buf, 400) == 299);
    CHECK(same(buf, big));
    blk = GetEnvironmentStringsA();
    CHECK(blk != 0 && in_block(blk, "ZRDXA=hello"));
    CHECK(SetEnvironmentVariableA("ZRDXA", "again"));
    CHECK(in_block(blk, "ZRDXA=hello"));
    CHECK(FreeEnvironmentStringsA(blk));
    blk = GetEnvironmentStringsA();
    CHECK(in_block(blk, "ZRDXA=again") &&
          !in_block(blk, "ZRDXA=hello"));
    CHECK(FreeEnvironmentStringsA(blk));
    wn[0] = 'Z';
    wn[1] = 'W';
    wn[2] = 0;
    wv[0] = 'x';
    wv[1] = 'y';
    wv[2] = 0;
    CHECK(SetEnvironmentVariableW(wn, wv));
    CHECK(GetEnvironmentVariableW(wn, wb, 8) == 2);
    CHECK(wb[1] == 'y');
    wblk = GetEnvironmentStringsW();
    CHECK(wblk != 0);
    CHECK(FreeEnvironmentStringsW(wblk));
    CHECK(SetEnvironmentVariableA("ZRDXA", 0));
    CHECK(SetEnvironmentVariableA("ZRDXB", 0));
    CHECK(SetEnvironmentVariableW(wn, 0));
    CHECK(GetEnvironmentVariableA("ZRDXA", buf, 400) == 0);
    CHECK(!SetEnvironmentVariableA("ZRDXA", 0) &&
          GetLastError() == 203);
}

static void time_tests(void)
{
    unsigned short st[8], back[8];
    DWORD ft[2], a[2], b[2], t0, t1;

    GetLocalTime(st);
    CHECK(st[0] >= 2020 && st[0] < 2100 && st[1] >= 1 &&
          st[1] <= 12);
    CHECK(st[3] >= 1 && st[3] <= 31 && st[4] < 24 && st[5] < 60);
    GetSystemTime(back);
    CHECK(back[0] == st[0]);
    GetSystemTimeAsFileTime(ft);
    CHECK(ft[1] > 0x01D00000UL);
    st[0] = 2000;
    st[1] = 1;
    st[2] = 6;
    st[3] = 1;
    st[4] = 0;
    st[5] = 0;
    st[6] = 0;
    st[7] = 0;
    CHECK(SystemTimeToFileTime(st, ft));
    CHECK(ft[1] == 0x01BF53EBUL && ft[0] == 0x256D4000UL);
    st[0] = 2024;
    st[1] = 2;
    st[2] = 4;
    st[3] = 29;
    st[4] = 12;
    st[5] = 34;
    st[6] = 56;
    st[7] = 789;
    CHECK(SystemTimeToFileTime(st, ft));
    CHECK(ft[1] == 0x01DA6B0BUL && ft[0] == 0xB3E67C50UL);
    CHECK(FileTimeToSystemTime(ft, back));
    CHECK(back[0] == 2024 && back[1] == 2 && back[2] == 4 &&
          back[3] == 29 && back[4] == 12 && back[5] == 34 &&
          back[6] == 56 && back[7] == 789);
    st[3] = 30;
    CHECK(!SystemTimeToFileTime(st, ft) && GetLastError() == 87);
    CHECK(QueryPerformanceFrequency(a) && a[0] == 1193182UL &&
          !a[1]);
    t0 = GetTickCount();
    QueryPerformanceCounter(a);
    Sleep(150);
    t1 = GetTickCount();
    QueryPerformanceCounter(b);
    CHECK(t1 - t0 >= 120 && t1 - t0 < 2000);
    CHECK(b[1] > a[1] || (b[1] == a[1] && b[0] > a[0]));
}

static void proc_tests(void)
{
    DWORD si[17], ms[8], sys[9];

    CHECK(GetCurrentProcessId() != 0);
    CHECK(GetCurrentProcess() == (HANDLE) - 1L);
    CHECK(!IsDebuggerPresent());
    OutputDebugStringA("zrdx");
    si[0] = 5;
    GetStartupInfoA(si);
    CHECK(si[0] == 68 && si[1] == 0);
    GetSystemInfo(sys);
    CHECK(sys[1] == 4096 && sys[5] == 1);
    ms[0] = 0;
    GlobalMemoryStatus(ms);
    CHECK(ms[0] == 32 && ms[3] > 0);
    CHECK(!TerminateProcess((HANDLE) 4, 0));
    CHECK(GetLastError() == 6);
}

void mainCRTStartup(void)
{
    env_tests();
    time_tests();
    proc_tests();
    dos_print("\r\nPROC: ok\r\n$");
    ExitProcess(0);
}
