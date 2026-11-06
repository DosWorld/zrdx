/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 186: one check per exported function of the heap,
 * Global/Local, TLS, critical section, Interlocked and event
 * groups. Prints "MEM: ok" or "MEM: fail NN".
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
    static char text[] = "\r\nMEM: fail 00\r\n$";

    text[13] = (char)('0' + step / 10);
    text[14] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static void heap_tests(void)
{
    HANDLE h, ph;
    unsigned char *a, *b, *c, *big;
    DWORD i;

    h = HeapCreate(0, 0, 0);
    CHECK(h != 0);
    ph = GetProcessHeap();
    CHECK(ph != 0 && ph == GetProcessHeap() && ph != h);
    a = HeapAlloc(h, 8, 100);
    CHECK(a != 0 && ((DWORD)a & 15) == 0);
    for (i = 0; i < 100; i++)
        if (a[i])
            fail();
    CHECK(HeapSize(h, 0, a) >= 100);
    b = HeapAlloc(h, 0, 5000);
    CHECK(b != 0 && b > a + 100);
    for (i = 0; i < 100; i++)
        a[i] = (unsigned char)i;
    CHECK(HeapValidate(h, 0, a) && HeapValidate(h, 0, 0));
    a = HeapReAlloc(h, 0, a, 20000);
    CHECK(a != 0 && a[99] == 99 && HeapSize(h, 0, a) >= 20000);
    c = HeapReAlloc(h, 8, a, 30000);
    CHECK(c != 0 && c[99] == 99);
    for (i = 20000; i < 30000; i++)
        if (c[i])
            fail();
    CHECK(HeapFree(h, 0, b));
    CHECK(!HeapFree(h, 0, b));
    b = HeapAlloc(h, 0, 4000);
    CHECK(b != 0);
    big = HeapAlloc(h, 0, 300000UL);
    CHECK(big != 0);
    big[299999UL] = 7;
    CHECK(HeapCompact(h, 0) > 0);
    CHECK(HeapLock(h) && HeapUnlock(h));
    CHECK(HeapAlloc((HANDLE) 4, 0, 10) == 0);
    CHECK(GetLastError() == 6);
    CHECK(HeapReAlloc(h, 0x10, big, 900000UL) == 0);
    CHECK(HeapFree(h, 0, c) && HeapFree(h, 0, b) &&
          HeapFree(h, 0, big));
    CHECK(HeapDestroy(h));
    CHECK(!HeapDestroy(ph));
    CHECK(HeapAlloc(h, 0, 10) == 0);
}

static void global_tests(void)
{
    HANDLE g, l;
    char *p;

    g = GlobalAlloc(0x40, 64);
    CHECK(g != 0 && GlobalSize(g) >= 64);
    p = GlobalLock(g);
    CHECK(p == (char *)g && p[63] == 0);
    p[0] = 'x';
    CHECK(!GlobalUnlock(g));
    g = GlobalReAlloc(g, 5000, 0x40);
    CHECK(g != 0 && ((char *)g)[0] == 'x');
    CHECK(((char *)g)[4999] == 0);
    CHECK(GlobalFree(g) == 0);
    CHECK(GlobalFree(g) == g);
    l = LocalAlloc(0x40, 10);
    CHECK(l != 0 && LocalSize(l) >= 10);
    l = LocalReAlloc(l, 300, 0);
    CHECK(l != 0 && LocalSize(l) >= 300);
    CHECK(LocalFree(l) == 0);
}

static void tls_tests(void)
{
    DWORD a, b;

    a = TlsAlloc();
    b = TlsAlloc();
    CHECK(a != 0xFFFFFFFFUL && b != 0xFFFFFFFFUL && a != b);
    CHECK(TlsGetValue(a) == 0 && GetLastError() == 0);
    CHECK(TlsSetValue(a, (void *)0x1234));
    CHECK(TlsSetValue(b, (void *)0x5678));
    CHECK(TlsGetValue(a) == (void *)0x1234);
    CHECK(TlsGetValue(b) == (void *)0x5678);
    CHECK(TlsFree(a));
    CHECK(!TlsFree(a) && GetLastError() == 87);
    CHECK(TlsGetValue(a) == 0 && GetLastError() == 87);
    CHECK(TlsAlloc() == a);
    CHECK(TlsGetValue(a) == 0);
    CHECK(TlsFree(a) && TlsFree(b));
    CHECK(!TlsSetValue(5000, 0));
}

static void sync_tests(void)
{
    DWORD cs[6];
    long v = 5;
    HANDLE e;

    InitializeCriticalSection(cs);
    EnterCriticalSection(cs);
    CHECK(TryEnterCriticalSection(cs));
    LeaveCriticalSection(cs);
    LeaveCriticalSection(cs);
    DeleteCriticalSection(cs);
    CHECK(InterlockedIncrement(&v) == 6 && v == 6);
    CHECK(InterlockedDecrement(&v) == 5);
    CHECK(InterlockedExchange(&v, 9) == 5 && v == 9);
    CHECK(InterlockedExchangeAdd(&v, 3) == 9 && v == 12);
    CHECK(InterlockedCompareExchange(&v, 1, 7) == 12 && v == 12);
    CHECK(InterlockedCompareExchange(&v, 1, 12) == 12 && v == 1);
    e = CreateEventA(0, 1, 0, 0);
    CHECK(WaitForSingleObject(e, 0) == 0x102);
    CHECK(SetEvent(e));
    CHECK(WaitForSingleObject(e, 0) == 0);
    CHECK(WaitForSingleObject(e, 0) == 0);
    CHECK(ResetEvent(e));
    CHECK(WaitForSingleObject(e, 30) == 0x102);
    CHECK(WaitForSingleObject(e, 0xFFFFFFFFUL) == 0xFFFFFFFFUL);
    CHECK(CloseHandle(e));
    e = CreateEventA(0, 0, 1, 0);
    CHECK(WaitForSingleObject(e, 0) == 0);
    CHECK(WaitForSingleObject(e, 0) == 0x102);
    CHECK(CloseHandle(e));
    CHECK(!SetEvent((HANDLE) 4));
    CHECK(WaitForSingleObject((HANDLE) 4, 0) == 0xFFFFFFFFUL);
}

void mainCRTStartup(void)
{
    heap_tests();
    global_tests();
    tls_tests();
    sync_tests();
    dos_print("\r\nMEM: ok\r\n$");
    ExitProcess(0);
}
