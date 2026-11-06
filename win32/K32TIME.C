/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: time. The DOS clock is the local time; the
 * time equals it (no time zone information). No 64-bit helpers
 * of a C runtime: values are 16-bit limbs.
 */
#include "w32.h"

#define TICKS_DAY 1573040UL

typedef struct SysTime
{
    WORD year, month, wday, day, hour, minute, second, msec;
} SysTime;

typedef struct FileTime
{
    DWORD lo, hi;
} FileTime;

/* unsigned 64-bit number: four 16-bit limbs, lowest first */
typedef struct U64
{
    DWORD l[4];
} U64;

static void u_set(U64 *u, DWORD v)
{
    u->l[0] = v & 0xFFFF;
    u->l[1] = v >> 16;
    u->l[2] = 0;
    u->l[3] = 0;
}

/* u = u * m + a */
static void u_muladd(U64 *u, DWORD m, DWORD a)
{
    DWORD m0 = m & 0xFFFF, m1 = m >> 16, r[4], t, i, j;

    for (i = 0; i < 4; i++)
        r[i] = 0;
    for (j = 0; j < 2; j++)
    {
        DWORD carry = 0, mj = j ? m1 : m0;

        for (i = 0; i + j < 4; i++)
        {
            t = u->l[i] * mj + r[i + j] + carry;
            r[i + j] = t & 0xFFFF;
            carry = t >> 16;
        }
    }
    t = r[0] + (a & 0xFFFF);
    r[0] = t & 0xFFFF;
    t = r[1] + (a >> 16) + (t >> 16);
    r[1] = t & 0xFFFF;
    t = r[2] + (t >> 16);
    r[2] = t & 0xFFFF;
    r[3] = (r[3] + (t >> 16)) & 0xFFFF;
    for (i = 0; i < 4; i++)
        u->l[i] = r[i];
}

/* u = u / d (d < 65536), returns the remainder */
static DWORD u_div(U64 *u, DWORD d)
{
    DWORD rem = 0, cur;
    int i;

    for (i = 3; i >= 0; i--)
    {
        cur = (rem << 16) | u->l[i];
        u->l[i] = cur / d;
        rem = cur % d;
    }
    return rem;
}

static int leap(DWORD y)
{
    return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0;
}

static DWORD mdays(DWORD y, DWORD m)
{
    static const BYTE d[12] = {31, 28, 31, 30, 31, 30, 31, 31,
                               30,
                               31, 30, 31
                              };

    return m == 2 && leap(y) ? 29 : d[m - 1];
}

/* days since 1601-01-01 of a valid date */
static DWORD days_of(DWORD y, DWORD m, DWORD d)
{
    DWORD n, i, p = y - 1;

    n = (y - 1601) * 365 + p / 4 - p / 100 + p / 400 - 388;
    for (i = 1; i < m; i++)
        n += mdays(y, i);
    return n + d - 1;
}

BOOL WINAPI SystemTimeToFileTime(const SysTime *st, FileTime *ft)
{
    U64 u;
    DWORD days;
    W32T("SystemTimeToFileTime");

    if (st == NULL || ft == NULL || st->year < 1601 ||
            st->year > 30827 || st->month < 1 ||
            st->month > 12 || st->day < 1 ||
            st->day > mdays(st->year, st->month) ||
            st->hour > 23 || st->minute > 59 ||
            st->second > 59 ||
            st->msec > 999)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    days = days_of(st->year, st->month, st->day);
    u_set(&u, days);
    u_muladd(&u, 86400UL, st->hour * 3600UL + st->minute * 60UL +
             st->second);
    u_muladd(&u, 1000UL, st->msec);
    u_muladd(&u, 10000UL, 0);
    ft->lo = u.l[0] | (u.l[1] << 16);
    ft->hi = u.l[2] | (u.l[3] << 16);
    return TRUE;
}

BOOL WINAPI FileTimeToSystemTime(const FileTime *ft, SysTime *st)
{
    U64 u;
    DWORD days, y, m;
    W32T("FileTimeToSystemTime");

    if (ft == NULL || st == NULL || ft->hi >= 0x7FFFFFFFUL)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    u.l[0] = ft->lo & 0xFFFF;
    u.l[1] = ft->lo >> 16;
    u.l[2] = ft->hi & 0xFFFF;
    u.l[3] = ft->hi >> 16;
    u_div(&u, 10000);
    st->msec = (WORD)u_div(&u, 1000);
    st->second = (WORD)u_div(&u, 60);
    st->minute = (WORD)u_div(&u, 60);
    st->hour = (WORD)u_div(&u, 24);
    days = u.l[0] | (u.l[1] << 16);
    st->wday = (WORD)((days + 1) % 7);
    y = 1601 + days / 365;
    while (days_of(y, 1, 1) > days)
        y--;
    while (days_of(y + 1, 1, 1) <= days)
        y++;
    days -= days_of(y, 1, 1);
    for (m = 1; days >= mdays(y, m); m++)
        days -= mdays(y, m);
    st->year = (WORD)y;
    st->month = (WORD)m;
    st->day = (WORD)(days + 1);
    return TRUE;
}

void WINAPI GetLocalTime(SysTime *st)
{
    struct dregs d, t, d2;
    W32T("GetLocalTime");

    do
    {
        w32_zero(&d, sizeof d);
        d.eax = 0x2A00;
        dos21(&d);
        w32_zero(&t, sizeof t);
        t.eax = 0x2C00;
        dos21(&t);
        w32_zero(&d2, sizeof d2);
        d2.eax = 0x2A00;
        dos21(&d2);
    }
    while ((d.edx & 0xFFFF) != (d2.edx & 0xFFFF));
    st->year = (WORD)(d.ecx & 0xFFFF);
    st->month = (WORD)((d.edx >> 8) & 0xFF);
    st->day = (WORD)(d.edx & 0xFF);
    st->wday = (WORD)(d.eax & 0xFF);
    st->hour = (WORD)((t.ecx >> 8) & 0xFF);
    st->minute = (WORD)(t.ecx & 0xFF);
    st->second = (WORD)((t.edx >> 8) & 0xFF);
    st->msec = (WORD)((t.edx & 0xFF) * 10);
}

void WINAPI GetSystemTime(SysTime *st)
{
    W32T("GetSystemTime");
    GetLocalTime(st);
}

void WINAPI GetSystemTimeAsFileTime(FileTime *ft)
{
    SysTime st;
    W32T("GetSystemTimeAsFileTime");

    GetLocalTime(&st);
    SystemTimeToFileTime(&st, ft);
}

extern void int1a(struct dregs *r);
#pragma aux int1a = \
    "push ebp" \
    "mov ebp, eax" \
    "mov eax, [ebp]" \
    "int 1Ah" \
    "mov [ebp], eax" \
    "mov [ebp + 8], ecx" \
    "mov [ebp + 12], edx" \
    "pop ebp" \
    parm [eax] \
    modify [eax ebx ecx edx esi edi];

extern void int2f(struct dregs *r);
#pragma aux int2f = \
    "push ebp" \
    "mov ebp, eax" \
    "mov eax, [ebp]" \
    "int 2Fh" \
    "pop ebp" \
    parm [eax] \
    modify [eax ebx ecx edx esi edi];

/* BIOS ticks (18.2 per second), midnight wraps are added */
static DWORD bios_ticks(void)
{
    static DWORD wraps;
    struct dregs r;

    w32_zero(&r, sizeof r);
    int1a(&r);
    if (r.eax & 0xFF)
        wraps++;
    return (((r.ecx & 0xFFFF) << 16) | (r.edx & 0xFFFF)) +
           wraps * TICKS_DAY;
}

DWORD WINAPI GetTickCount(void)
{
    DWORD t = bios_ticks();
    W32T("GetTickCount");

    return (t / 1000) * 54925UL + (t % 1000) * 54925UL / 1000;
}

BOOL WINAPI QueryPerformanceFrequency(DWORD *f)
{
    W32T("QueryPerformanceFrequency");
    f[0] = 1193182UL;
    f[1] = 0;
    return TRUE;
}

/* the BIOS tick is 65536 counts of the 1193182 Hz timer */
BOOL WINAPI QueryPerformanceCounter(DWORD *c)
{
    DWORD t = bios_ticks();
    W32T("QueryPerformanceCounter");

    c[0] = t << 16;
    c[1] = t >> 16;
    return TRUE;
}

void WINAPI Sleep(DWORD ms)
{
    DWORD start = GetTickCount();
    struct dregs r;
    W32T("Sleep");

    while (GetTickCount() - start < ms)
    {
        w32_zero(&r, sizeof r);
        r.eax = 0x1680;
        int2f(&r);
    }
}
