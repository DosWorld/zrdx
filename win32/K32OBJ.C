/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: kernel objects (events) in chunks of small
 * records.
 */
#include "w32.h"

#define KO_PER 680
#define OBJBASE 0x10000UL

typedef struct KObj
{
    DWORD type;
    DWORD signaled;
    DWORD manual;
} KObj;

typedef struct KChunk
{
    KObj rec[KO_PER];
    DWORD handle;
    struct KChunk *next;
    DWORD unused;
} KChunk;

static KChunk *khead;

HANDLE WINAPI CreateEventA(void *sa, BOOL manual, BOOL initial,
                           const char *name)
{
    KChunk *c, *last = NULL;
    DWORD i = 0, n = 0, h;
    int found = 0;
    W32T("CreateEventA");

    for (c = khead; c && !found; c = found ? c : c->next)
    {
        for (i = 0; i < KO_PER && !found; i++)
            found = c->rec[i].type == 0;
        if (found)
            break;
        last = c;
        n++;
    }
    if (!found)
    {
        c = w32_pages(sizeof(KChunk), &h);
        if (c == NULL)
        {
            w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
            return NULL;
        }
        c->handle = h;
        if (last)
            last->next = c;
        else
            khead = c;
        i = 1;
    }
    i--;
    c->rec[i].type = 1;
    c->rec[i].signaled = initial != 0;
    c->rec[i].manual = manual != 0;
    return (HANDLE)(OBJBASE + 4 * (n * KO_PER + i));
}

BOOL w32_objclose(HANDLE h)
{
    KChunk *c;
    DWORD idx = ((DWORD)h - OBJBASE) / 4, n = idx / KO_PER;

    for (c = khead; c && n; c = c->next)
        n--;
    if (c == NULL || c->rec[idx % KO_PER].type == 0)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    c->rec[idx % KO_PER].type = 0;
    return TRUE;
}

void w32_objfree(void)
{
    KChunk *k = khead, *kn;

    khead = NULL;
    while (k)
    {
        kn = k->next;
        w32_freepages(k->handle);
        k = kn;
    }
}

/* the event of handle h, or NULL */
static KObj *event(HANDLE h)
{
    KChunk *c;
    DWORD idx, n;

    if ((DWORD)h < OBJBASE || ((DWORD)h & 3))
        return NULL;
    idx = ((DWORD)h - OBJBASE) / 4;
    for (c = khead, n = idx / KO_PER; c && n; c = c->next)
        n--;
    if (c == NULL || c->rec[idx % KO_PER].type != 1)
        return NULL;
    return &c->rec[idx % KO_PER];
}

static BOOL setstate(HANDLE h, DWORD signaled)
{
    KObj *e = event(h);

    if (e == NULL)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    e->signaled = signaled;
    return TRUE;
}

BOOL WINAPI SetEvent(HANDLE h)
{
    W32T("SetEvent");
    return setstate(h, 1);
}

BOOL WINAPI ResetEvent(HANDLE h)
{
    W32T("ResetEvent");
    return setstate(h, 0);
}

extern void WINAPI Sleep(DWORD ms);

/* one thread: a wait that could end only by another thread
   fails after the timeout (INFINITE: at once, WAIT_FAILED) */
DWORD WINAPI WaitForSingleObject(HANDLE h, DWORD ms)
{
    KObj *e = event(h);
    W32T("WaitForSingleObject");

    if (e == NULL)
    {
        w32_seterr(ERROR_INVALID_HANDLE);
        return 0xFFFFFFFFUL;
    }
    if (e->signaled)
    {
        if (!e->manual)
            e->signaled = 0;
        return 0;
    }
    if (ms == 0xFFFFFFFFUL)
    {
        w32_seterr(1359);
        return 0xFFFFFFFFUL;
    }
    Sleep(ms);
    return 0x102;
}
