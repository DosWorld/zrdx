/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: heaps and the Global/Local memory calls. A heap
 * is a list of segments (DPMI blocks); blocks have a 16 byte
 * header and are split and merged on demand. All segments are
 * released when the DLL detaches. Moveable Global/Local memory
 * is fixed: the handle is the address.
 */
#include "w32.h"

#define HDR 16UL
#define MIN_BLOCK 32UL
#define SEG_MIN 65536UL
#define MAGIC_USED 0xA110C8EDUL
#define MAGIC_FREE 0xF4EEF4EEUL
#define HEAP_ZERO 8UL
#define REALLOC_IN_PLACE 0x10UL

typedef struct Seg
{
    struct Seg *next;
    DWORD size;
    DWORD handle;
    DWORD pad;
} Seg;

typedef struct Heap
{
    struct Heap *next;
    Seg *segs;
    DWORD flags;
    DWORD pad;
} Heap;

typedef struct Block
{
    DWORD size;
    DWORD magic;
    DWORD pad[2];
} Block;

static Heap *heaps;
static Heap *process;

static Block *first(Seg *s)
{
    return (Block *)((char *)s + sizeof(Seg));
}

static Block *after(Block *b)
{
    return (Block *)((char *)b + b->size);
}

static int inseg(Seg *s, void *p)
{
    return (char *)p >= (char *)first(s) &&
           (char *)p < (char *)s + s->size;
}

static Heap *valid(HANDLE h)
{
    Heap *p;

    for (p = heaps; p; p = p->next)
        if (p == (Heap *)h)
            return p;
    w32_seterr(ERROR_INVALID_HANDLE);
    return NULL;
}

/* new segment with room for need bytes of blocks */
static Seg *newseg(Heap *h, DWORD need)
{
    Seg *s;
    Block *b;
    DWORD total, handle;

    total = need + sizeof(Seg) + (h->segs == NULL ? sizeof(
                                      Heap) : 0);
    if (total < SEG_MIN)
        total = SEG_MIN;
    total = (total + 4095) & ~4095UL;
    s = w32_pages(total, &handle);
    if (s == NULL)
        return NULL;
    s->size = total;
    s->handle = handle;
    s->next = h->segs;
    h->segs = s;
    b = first(s);
    if (s->next == NULL)
        b = (Block *)((char *)b + sizeof(Heap));
    b->size = (DWORD)((char *)s + total - (char *)b);
    b->magic = MAGIC_FREE;
    return s;
}

/* merges free neighbours of b */
static void merge(Seg *s, Block *b)
{
    Block *n;

    for (n = after(b); (char *)n < (char *)s + s->size &&
            n->magic == MAGIC_FREE; n = after(b))
        b->size += n->size;
}

static void *take(Block *b, DWORD need, DWORD zero)
{
    DWORD rest = b->size - need;
    char *u;
    DWORD i, n;

    if (rest >= HDR + MIN_BLOCK)
    {
        Block *r = (Block *)((char *)b + need);

        r->size = rest;
        r->magic = MAGIC_FREE;
        b->size = need;
    }
    b->magic = MAGIC_USED;
    u = (char *)b + HDR;
    if (zero)
    {
        n = b->size - HDR;
        for (i = 0; i < n; i++)
            u[i] = 0;
    }
    return u;
}

static void *halloc(Heap *h, DWORD size, DWORD zero)
{
    Seg *s;
    Block *b;
    DWORD need;

    if (size > 0x7FFFF000UL)
    {
        w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
    }
    need = ((size + 15) & ~15UL) + HDR;
    if (need < HDR + MIN_BLOCK)
        need = HDR + MIN_BLOCK;
    for (s = h->segs; s; s = s->next)
    {
        b = first(s);
        if (s->next == NULL)
            b = (Block *)((char *)b + sizeof(Heap));
        while ((char *)b < (char *)s + s->size)
        {
            if (b->magic == MAGIC_FREE)
            {
                merge(s, b);
                if (b->size >= need)
                    return take(b, need, zero);
            }
            b = after(b);
        }
    }
    s = newseg(h, need);
    if (s == NULL)
    {
        w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
    }
    b = first(s);
    if (s->next == NULL)
        b = (Block *)((char *)b + sizeof(Heap));
    return take(b, need, zero);
}

/* the used block of user pointer p in heap h, or NULL */
static Block *lookup(Heap *h, void *p)
{
    Seg *s;
    Block *b = (Block *)((char *)p - HDR);

    for (s = h->segs; s; s = s->next)
        if (inseg(s, b))
        {
            if (((DWORD)b & 15) == 0 && b->magic == MAGIC_USED)
                return b;
            break;
        }
    w32_seterr(ERROR_INVALID_PARAMETER);
    return NULL;
}

HANDLE WINAPI HeapCreate(DWORD flags, DWORD initial, DWORD max)
{
    Heap *h;
    Seg *s;
    Heap tmp;
    W32T("HeapCreate");

    tmp.segs = NULL;
    s = newseg(&tmp, initial);
    if (s == NULL)
    {
        w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
    }
    h = (Heap *)((char *)first(s));
    h->segs = s;
    h->flags = flags;
    h->next = heaps;
    heaps = h;
    return h;
}

HANDLE WINAPI GetProcessHeap(void)
{
    W32T("GetProcessHeap");
    if (process == NULL)
        process = (Heap *)HeapCreate(0, 0, 0);
    return process;
}

void *WINAPI HeapAlloc(HANDLE heap, DWORD flags, DWORD size)
{
    Heap *h = valid(heap);
    W32T("HeapAlloc");

    return h ? halloc(h, size, flags & HEAP_ZERO) : NULL;
}

BOOL WINAPI HeapFree(HANDLE heap, DWORD flags, void *p)
{
    Heap *h = valid(heap);
    Block *b;
    Seg *s;
    W32T("HeapFree");

    if (h == NULL || p == NULL)
        return h != NULL;
    b = lookup(h, p);
    if (b == NULL)
        return FALSE;
    b->magic = MAGIC_FREE;
    for (s = h->segs; s; s = s->next)
        if (inseg(s, b))
            merge(s, b);
    return TRUE;
}

DWORD WINAPI HeapSize(HANDLE heap, DWORD flags, const void *p)
{
    Heap *h = valid(heap);
    Block *b = h ? lookup(h, (void *)p) : NULL;
    W32T("HeapSize");

    return b ? b->size - HDR : 0xFFFFFFFFUL;
}

void *WINAPI HeapReAlloc(HANDLE heap, DWORD flags, void *p,
                         DWORD size)
{
    Heap *h = valid(heap);
    Block *b, *n;
    Seg *s;
    DWORD need, have, i;
    char *q;
    W32T("HeapReAlloc");

    if (h == NULL || p == NULL || (b = lookup(h, p)) == NULL)
        return NULL;
    have = b->size - HDR;
    need = ((size + 15) & ~15UL) + HDR;
    if (need < HDR + MIN_BLOCK)
        need = HDR + MIN_BLOCK;
    for (s = h->segs; s && !inseg(s, b); s = s->next)
        ;
    if (need > b->size && s)
    {
        n = after(b);
        if ((char *)n < (char *)s + s->size &&
                n->magic == MAGIC_FREE)
        {
            merge(s, n);
            if (b->size + n->size >= need)
                b->size += n->size;
        }
    }
    if (need <= b->size)
    {
        if (b->size - need >= HDR + MIN_BLOCK)
        {
            n = (Block *)((char *)b + need);
            n->size = b->size - need;
            n->magic = MAGIC_FREE;
            b->size = need;
        }
        if ((flags & HEAP_ZERO) && size > have)
            for (i = have; i < size; i++)
                ((char *)p)[i] = 0;
        return p;
    }
    if (flags & REALLOC_IN_PLACE)
    {
        w32_seterr(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
    }
    q = halloc(h, size, flags & HEAP_ZERO);
    if (q == NULL)
        return NULL;
    w32_copy(q, p, have);
    HeapFree(heap, 0, p);
    return q;
}

BOOL WINAPI HeapDestroy(HANDLE heap)
{
    Heap *h = valid(heap), **pp;
    Seg *s, *n;
    W32T("HeapDestroy");

    if (h == NULL || h == process)
    {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    for (pp = &heaps; *pp != h; pp = &(*pp)->next)
        ;
    *pp = h->next;
    for (s = h->segs; s; s = n)
    {
        n = s->next;
        w32_freepages(s->handle);
    }
    return TRUE;
}

BOOL WINAPI HeapValidate(HANDLE heap, DWORD flags, const void *p)
{
    Heap *h = valid(heap);
    W32T("HeapValidate");

    return h && (p == NULL || lookup(h, (void *)p) != NULL);
}

DWORD WINAPI HeapCompact(HANDLE heap, DWORD flags)
{
    Heap *h = valid(heap);
    Seg *s;
    Block *b;
    DWORD best = 0;
    W32T("HeapCompact");

    if (h == NULL)
        return 0;
    for (s = h->segs; s; s = s->next)
    {
        b = first(s);
        if (s->next == NULL)
            b = (Block *)((char *)b + sizeof(Heap));
        while ((char *)b < (char *)s + s->size)
        {
            if (b->magic == MAGIC_FREE)
            {
                merge(s, b);
                if (b->size - HDR > best)
                    best = b->size - HDR;
            }
            b = after(b);
        }
    }
    return best;
}

BOOL WINAPI HeapLock(HANDLE heap)
{
    W32T("HeapLock");
    return valid(heap) != NULL;
}

BOOL WINAPI HeapSetInformation(HANDLE heap, DWORD cls,
                               void *info,
                               DWORD len)
{
    W32T("HeapSetInformation");
    return TRUE;
}

void w32_heapfree(void)
{
    Heap *h, *n;
    Seg *s, *sn;

    for (h = heaps; h; h = n)
    {
        n = h->next;
        for (s = h->segs; s; s = sn)
        {
            sn = s->next;
            w32_freepages(s->handle);
        }
    }
    heaps = NULL;
    process = NULL;
}

/* Global and Local memory: fixed blocks of the process heap */
HANDLE WINAPI GlobalAlloc(DWORD flags, DWORD size)
{
    W32T("GlobalAlloc");
    return halloc((Heap *)GetProcessHeap(), size,
                  flags & 0x40UL);
}

HANDLE WINAPI GlobalFree(HANDLE h)
{
    W32T("GlobalFree");
    return HeapFree(GetProcessHeap(), 0, h) ? NULL : h;
}

DWORD WINAPI GlobalSize(HANDLE h)
{
    DWORD n;
    W32T("GlobalSize");

    n = HeapSize(GetProcessHeap(), 0, h);
    return n == 0xFFFFFFFFUL ? 0 : n;
}

HANDLE WINAPI GlobalReAlloc(HANDLE h, DWORD size, DWORD flags)
{
    W32T("GlobalReAlloc");
    if (flags & 0x80UL)
        return h;
    return HeapReAlloc(GetProcessHeap(),
                       flags & 0x40UL ? HEAP_ZERO : 0,
                       h, size);
}

void *WINAPI GlobalLock(HANDLE h)
{
    W32T("GlobalLock");
    return HeapValidate(GetProcessHeap(), 0, h) ? h : NULL;
}

BOOL WINAPI GlobalUnlock(HANDLE h)
{
    W32T("GlobalUnlock");
    w32_seterr(0);
    return FALSE;
}

HANDLE WINAPI GlobalHandle(const void *p)
{
    W32T("GlobalHandle");
    return (HANDLE)p;
}
