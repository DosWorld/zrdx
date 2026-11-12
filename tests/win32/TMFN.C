/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Review fixes: prints the module file name, checks the
 * standard handles (DOS handle + 1, times 4) and the size guard
 * of HeapReAlloc. Run unbound and bound.
 */
#include "tw32.h"

extern DWORD W GetModuleFileNameA(HANDLE, char *, DWORD);

void mainCRTStartup(void)
{
    char b[300];
    DWORD n = GetModuleFileNameA(0, b, 300);
    void *h = GetProcessHeap();

    WriteFile(GetStdHandle(0xFFFFFFF5UL), b, n, &n, 0);
    WriteFile(GetStdHandle(0xFFFFFFF5UL), "|\r\n", 3, &n, 0);
    if (GetStdHandle(0xFFFFFFF6UL) != (HANDLE) 4 ||
            GetStdHandle(0xFFFFFFF5UL) != (HANDLE) 8 ||
            GetStdHandle(0xFFFFFFF4UL) != (HANDLE) 12)
        ExitProcess(1);
    if (HeapReAlloc(h, 0, HeapAlloc(h, 0, 16), 0xFFFFFFFFUL) != 0)
        ExitProcess(2);
    ExitProcess(0);
}
