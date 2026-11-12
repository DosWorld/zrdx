/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: zero filled DPMI blocks and the Virtual* calls.
 * Blocks are kept in chunks of small records (8 KB each) and are
 * released when the DLL detaches.
 */
#include "w32.h"

#define MEM_COMMIT 0x1000UL
#define MEM_RESERVE 0x2000UL
#define MEM_DECOMMIT 0x4000UL
#define MEM_RELEASE 0x8000UL
#define PAGE 0x1000UL
#define VB_PER 680

typedef struct VBlock {
    DWORD base;
    DWORD size;
    DWORD handle;
} VBlock;

typedef struct VChunk {
    VBlock rec[VB_PER];
    DWORD handle;
    struct VChunk *next;
    DWORD unused;
} VChunk;

typedef struct MemInfo {
    DWORD base, allocbase, allocprot, size, state, prot, type;
} MemInfo;

static VChunk *vhead;

void *w32_pages(DWORD bytes, DWORD *handle)
{
    struct dregs r;
    DWORD base;

    w32_zero(&r, sizeof r);
    r.eax = 0x0501;
    r.ebx = bytes >> 16;
    r.ecx = bytes & 0xFFFF;
    dpmi31(&r);
    if (r.cf) {
        *handle = 0;
        return NULL;
    }
    base = ((r.ebx & 0xFFFF) << 16) | (r.ecx & 0xFFFF);
    *handle = ((r.esi & 0xFFFF) << 16) | (r.edi & 0xFFFF);
    w32_zero((void *)base, bytes);
    return (void *)base;
}

void w32_freepages(DWORD handle)
{
    struct dregs r;

    w32_zero(&r, sizeof r);
    r.eax = 0x0502;
    r.esi = handle >> 16;
    r.edi = handle & 0xFFFF;
    dpmi31(&r);
}

static VBlock *vfind(DWORD addr)
{
    VChunk *c;
    DWORD i;

    for (c = vhead; c; c = c->next)
        for (i = 0; i < VB_PER; i++)
            if (c->rec[i].base && addr >= c->rec[i].base &&
                    addr - c->rec[i].base < c->rec[i].size)
                return &c->rec[i];
    return NULL;
}

static VBlock *vnew(void)
{
    VChunk *c;
    DWORD i, h;

    for (c = vhead; c; c = c->next)
        for (i = 0; i < VB_PER; i++)
            if (c->rec[i].base == 0)
                return &c->rec[i];
    c = w32_pages(sizeof(VChunk), &h);
    if (c == NULL)
        return NULL;
    c->handle = h;
    c->next = vhead;
    vhead = c;
    return &c->rec[0];
}

LPVOID WINAPI VirtualAlloc(LPVOID addr, DWORD size, DWORD type,
                           DWORD prot)
{
    VBlock *b;
    DWORD rs, h, base;
    char *p;
    W32T("VirtualAlloc");
    W32V("a", addr);
    W32V("s", size);
    W32V("t", type);

    if (size == 0 || !(type & (MEM_COMMIT | MEM_RESERVE))) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return NULL;
    }
    if (addr != NULL) {
        b = vfind((DWORD)addr);
        if (b == NULL || (DWORD)addr + size < (DWORD)addr ||
                (DWORD)addr + size > b->base + b->size) {
            w32_seterr(ERROR_INVALID_ADDRESS);
            return NULL;
        }
        return (LPVOID)((DWORD)addr & ~(PAGE - 1));
    }
    rs = (size + PAGE - 1) & ~(PAGE - 1);
    if (rs < size || rs > 0x7FFFF000UL) {
        w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
    }
    b = vnew();
    p = b ? w32_pages(rs, &h) : NULL;
    if (p != NULL && ((DWORD)p & (PAGE - 1))) {
        w32_freepages(h);
        p = w32_pages(rs + PAGE, &h);
        base = ((DWORD)p + PAGE - 1) & ~(PAGE - 1);
    } else
        base = (DWORD)p;
    if (p == NULL) {
        w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
    }
    b->base = base;
    b->size = rs;
    b->handle = h;
    W32V("r", base);
    return (LPVOID)base;
}

BOOL WINAPI VirtualFree(LPVOID addr, DWORD size, DWORD type)
{
    VBlock *b = vfind((DWORD)addr);
    W32T("VirtualFree");

    if (b == NULL ||
            (type != MEM_RELEASE && type != MEM_DECOMMIT) ||
            (type == MEM_RELEASE && (size != 0 ||
                                     b->base != (DWORD)addr))) {
        w32_seterr(b == NULL ? ERROR_INVALID_ADDRESS :
                   ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    if (type == MEM_RELEASE) {
        w32_freepages(b->handle);
        b->base = 0;
    }
    return TRUE;
}

DWORD WINAPI VirtualQuery(const void *addr, MemInfo *mi,
                          DWORD len)
{
    VBlock *b;
    DWORD a = (DWORD)addr & ~(PAGE - 1);
    W32T("VirtualQuery");
    W32V("q", addr);

    if (len < sizeof(MemInfo)) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    b = vfind(a);
    w32_zero(mi, sizeof(MemInfo));
    mi->base = a;
    if (b) {
        mi->allocbase = b->base;
        mi->allocprot = 4;
        mi->size = b->base + b->size - a;
        mi->state = MEM_COMMIT;
        mi->prot = 4;
        mi->type = 0x20000UL;
    } else {
        /* memory we did not allocate (stack, images): committed,
           the whole address space below 2 GB as one region */
        mi->allocbase = 0;
        mi->allocprot = 4;
        mi->size = 0x7FFFF000UL - a;
        mi->state = MEM_COMMIT;
        mi->prot = 4;
        mi->type = 0x20000UL;
    }
    return sizeof(MemInfo);
}

void w32_memfree(void)
{
    VChunk *c = vhead, *n;
    DWORD i;

    vhead = NULL;
    while (c) {
        for (i = 0; i < VB_PER; i++)
            if (c->rec[i].base)
                w32_freepages(c->rec[i].handle);
        n = c->next;
        w32_freepages(c->handle);
        c = n;
    }
}
