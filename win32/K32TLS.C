/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: TLS slots, critical sections and the Interlocked
 * functions of a single thread program.
 */
#include "w32.h"

#define TLS_SLOTS 1088UL
#define TLS_OUT 0xFFFFFFFFUL

typedef struct CritSect {
    void *debug;
    long lockcount;
    long recursion;
    HANDLE owner;
    HANDLE semaphore;
    DWORD spin;
} CritSect;

/* TEB: slots 0-63 at E10h, a pointer to the others at F94h,
   the static TLS pointer (2Ch) is set by the loader table */
#define TEB_SLOTS (0xE10 / 4)
#define TEB_EXPAND (0xF94 / 4)
#define TEB_DIRECT 64

/* "used" marks of all slots, then the values of slots 64.. */
static DWORD *tls;
static DWORD tlshandle;

static int tls_init(void)
{
    DWORD *teb = w32_tebp();

    if (tls == NULL && teb != NULL) {
        tls = w32_pages(2 * TLS_SLOTS * sizeof(DWORD),
                        &tlshandle);
        if (tls)
            teb[TEB_EXPAND] = (DWORD)(tls + TLS_SLOTS);
    }
    if (tls == NULL)
        w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
    return tls != NULL;
}

/* the value of slot i (the slot is in use) */
static DWORD *value(DWORD i)
{
    DWORD *teb = w32_tebp();

    return i < TEB_DIRECT ? teb + TEB_SLOTS + i :
           (DWORD *)teb[TEB_EXPAND] + (i - TEB_DIRECT);
}

/* mark of slot i at tls[i] */
DWORD WINAPI TlsAlloc(void)
{
    DWORD i;
    W32T("TlsAlloc");

    if (!tls_init())
        return TLS_OUT;
    for (i = 0; i < TLS_SLOTS; i++)
        if (!tls[i]) {
            tls[i] = 1;
            *value(i) = 0;
            return i;
        }
    w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
    return TLS_OUT;
}

static int slot_ok(DWORD i)
{
    if (i >= TLS_SLOTS || !tls_init() || !tls[i]) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    return 1;
}

BOOL WINAPI TlsFree(DWORD i)
{
    W32T("TlsFree");
    if (!slot_ok(i))
        return FALSE;
    tls[i] = 0;
    *value(i) = 0;
    return TRUE;
}

void *WINAPI TlsGetValue(DWORD i)
{
    W32T("TlsGetValue");
    if (!slot_ok(i))
        return NULL;
    w32_seterr(0);
    return (void *)*value(i);
}

BOOL WINAPI TlsSetValue(DWORD i, void *v)
{
    W32T("TlsSetValue");
    if (!slot_ok(i))
        return FALSE;
    *value(i) = (DWORD)v;
    return TRUE;
}

void w32_tlsfree(void)
{
    if (tls)
        w32_freepages(tlshandle);
    tls = NULL;
}

void WINAPI InitializeCriticalSection(CritSect *cs)
{
    W32T("InitializeCriticalSection");
    w32_zero(cs, sizeof(CritSect));
    cs->lockcount = -1;
}

BOOL WINAPI InitializeCriticalSectionAndSpinCount(CritSect *cs,
        DWORD spin)
{
    W32T("InitializeCriticalSectionAndSpinCount");
    InitializeCriticalSection(cs);
    cs->spin = spin;
    return TRUE;
}

void WINAPI EnterCriticalSection(CritSect *cs)
{
    W32T("EnterCriticalSection");
    cs->lockcount++;
    cs->recursion++;
}

BOOL WINAPI TryEnterCriticalSection(CritSect *cs)
{
    W32T("TryEnterCriticalSection");
    cs->lockcount++;
    cs->recursion++;
    return TRUE;
}

void WINAPI LeaveCriticalSection(CritSect *cs)
{
    W32T("LeaveCriticalSection");
    cs->lockcount--;
    cs->recursion--;
}

void WINAPI DeleteCriticalSection(CritSect *cs)
{
    W32T("DeleteCriticalSection");
    w32_zero(cs, sizeof(CritSect));
}

long WINAPI InterlockedIncrement(long *p)
{
    return ++*p;
}

long WINAPI InterlockedDecrement(long *p)
{
    return --*p;
}

long WINAPI InterlockedExchange(long *p, long v)
{
    long old = *p;

    *p = v;
    return old;
}

long WINAPI InterlockedExchangeAdd(long *p, long v)
{
    long old = *p;

    *p += v;
    return old;
}

long WINAPI InterlockedCompareExchange(long *p, long v, long cmp)
{
    long old = *p;

    if (old == cmp)
        *p = v;
    return old;
}
