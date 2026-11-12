/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: process and system information.
 */
#include "w32.h"

extern void WINAPI ExitProcess(UINT code);

typedef struct SysInfo {
    WORD arch, reserved;
    DWORD pagesize, minaddr, maxaddr, mask, cpus, type;
    DWORD granularity;
    WORD level, revision;
} SysInfo;

typedef struct MemStatus {
    DWORD length, load, totalphys, availphys, totalpage;
    DWORD availpage, totalvirt, availvirt;
} MemStatus;

HANDLE WINAPI GetCurrentProcess(void)
{
    W32T("GetCurrentProcess");
    return (HANDLE) - 1L;
}

DWORD WINAPI GetCurrentProcessId(void)
{
    struct dregs r;
    W32T("GetCurrentProcessId");

    w32_zero(&r, sizeof r);
    r.eax = 0x6200;
    dos21(&r);
    return r.ebx & 0xFFFF;
}

BOOL WINAPI TerminateProcess(HANDLE h, UINT code)
{
    W32T("TerminateProcess");
    if (h != (HANDLE) - 1L) {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    ExitProcess(code);
    return TRUE;
}

BOOL WINAPI IsDebuggerPresent(void)
{
    W32T("IsDebuggerPresent");
    return FALSE;
}

void WINAPI OutputDebugStringA(const char *text)
{
    W32T("OutputDebugStringA");
}

void WINAPI GetStartupInfoA(DWORD *si)
{
    W32T("GetStartupInfoA");
    w32_zero(si, 68);
    si[0] = 68;
}

void WINAPI GetSystemInfo(SysInfo *si)
{
    W32T("GetSystemInfo");
    w32_zero(si, sizeof(SysInfo));
    si->pagesize = 4096;
    si->minaddr = 0x10000UL;
    si->maxaddr = 0x7FFEFFFFUL;
    si->mask = 1;
    si->cpus = 1;
    si->type = 586;
    si->granularity = 0x10000UL;
    si->level = 5;
}

void WINAPI GlobalMemoryStatus(MemStatus *ms)
{
    struct dregs r;
    DWORD info[12];
    DWORD free;
    W32T("GlobalMemoryStatus");

    w32_zero(info, sizeof info);
    w32_zero(&r, sizeof r);
    r.eax = 0x0500;
    r.edi = (DWORD)info;
    dpmi31(&r);
    free = r.cf ? 0 : info[0];
    w32_zero(ms, sizeof(MemStatus));
    ms->length = sizeof(MemStatus);
    ms->availphys = free;
    ms->totalphys = free;
    ms->availvirt = free;
    ms->totalvirt = free;
    ms->availpage = free;
    ms->totalpage = free;
}
