/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: structured exception handling (docs/WIN32.MD,
 * task 205). DPMI exceptions become EXCEPTION_RECORD + CONTEXT
 * on the faulting stack (K32EXA.ASM), the FS:[0] chain is
 * walked, RaiseException and the unhandled exception filter.
 */
#include "w32.h"

#define EXC_COUNT 18
#define CTX_SIZE 0x2CC
#define EXC_NONCONT 1
#define EXC_UNWINDING 2
#define EXC_EXIT_UNWIND 4
#define DISP_CONTINUE 0
#define DISP_SEARCH 1
#define DISP_COLLIDED 3

typedef struct ExcRecord {
    DWORD code, flags;
    struct ExcRecord *next;
    void *address;
    DWORD nparams;
    DWORD info[15];
} ExcRecord;

typedef struct Context {
    DWORD flags, dr[6], fp[28];
    DWORD gs, fs, es, ds, edi, esi, ebx, edx, ecx, eax;
    DWORD ebp, eip, cs, eflags, esp, ss;
    BYTE ext[512];
} Context;

typedef struct ExcPointers {
    ExcRecord *rec;
    Context *ctx;
} ExcPointers;

typedef struct Handler {
    DWORD sel, off;
} Handler;

typedef int (__cdecl * SehProc)(ExcRecord *, void *, Context *,
                                void *);
typedef int (WINAPI * Filter)(ExcPointers *);

extern char w32_excstubs[];
extern void __cdecl w32_resume(Context *ctx);

extern DWORD get_cs(void);
#pragma aux get_cs = \
    "xor eax, eax" \
    "mov ax, cs" \
    value [eax];

/* exception number -> NT status, 0 = not hooked */
static const DWORD codes[EXC_COUNT] = {
    0xC0000094UL, 0, 0, 0, 0xC0000095UL, 0xC000008CUL,
    0xC000001DUL, 0, 0, 0, 0, 0,
    0xC00000FDUL, 0xC0000005UL, 0xC0000005UL, 0, 0xC0000090UL,
    0x80000002UL
};

static Filter filter;
static Handler *oldh;
static DWORD oldhandle;
static int hooked;

static void hex8(char *s, DWORD v)
{
    int i;

    for (i = 0; i < 8; i++)
        s[i] = "0123456789ABCDEF"[(v >> (28 - 4 * i)) & 15];
}

/* the process ends with the exception code (low byte) */
static void die(const ExcRecord *rec)
{
    static char msg[] =
        "\r\nUnhandled exception XXXXXXXX at XXXXXXXX\r\n$";
    struct dregs r;

    hex8(msg + 22, rec->code);
    hex8(msg + 34, (DWORD)rec->address);
    w32_zero(&r, sizeof r);
    r.eax = 0x0900;
    r.edx = (DWORD)msg;
    dos21(&r);
    ExitProcess(rec->code);
}

/* returns 1 when a frame handled the exception */
static int seh_walk(ExcRecord *rec, Context *ctx)
{
    DWORD *teb = w32_tebp();
    DWORD *fr, prev = 0, dc[2];
    ExcRecord again;
    int d;

    if (teb == NULL)
        return 0;
    for (fr = (DWORD *)teb[0]; (DWORD)fr != 0xFFFFFFFFUL;
            fr = (DWORD *)fr[0]) {
        if ((DWORD)fr & 3 || (DWORD)fr <= prev ||
                (DWORD)fr >= teb[1] || fr[1] == 0)
            break;
        prev = (DWORD)fr;
        d = ((SehProc)fr[1])(rec, fr, ctx, dc);
        if (d != DISP_CONTINUE)
            continue;
        if (!(rec->flags & EXC_NONCONT))
            return 1;
        w32_zero(&again, sizeof again);
        again.code = 0xC0000025UL;
        again.flags = EXC_NONCONT;
        again.next = rec;
        again.address = rec->address;
        if (!seh_walk(&again, ctx))
            die(&again);
        die(rec);
    }
    return 0;
}

/*
 * Calls the handlers of the frames below target (all of them for
 * NULL) with EH_UNWINDING and unlinks each frame; the target
 * stays the head of the chain.
 */
void WINAPI RtlUnwind(void *target, void *ip, void *record,
                      DWORD value)
{
    ExcRecord own, again, *rec = record;
    Context ctx;
    DWORD *teb = w32_tebp(), *fr, dc[2];
    W32T("RtlUnwind");

    if (teb == NULL)
        return;
    w32_zero(&ctx, sizeof ctx);
    ctx.flags = 0x10007UL;
    ctx.eax = value;
    ctx.eip = (DWORD)ip;
    if (rec == NULL) {
        w32_zero(&own, sizeof own);
        own.code = 0xC0000027UL;
        rec = &own;
    }
    rec->flags |= EXC_UNWINDING;
    if (target == NULL)
        rec->flags |= EXC_EXIT_UNWIND;
    for (fr = (DWORD *)teb[0]; (void *)fr != target &&
            (DWORD)fr != 0xFFFFFFFFUL; fr = (DWORD *)fr[0]) {
        if (target != NULL && (DWORD)fr > (DWORD)target)
            break;
        dc[0] = 0;
        if (((SehProc)fr[1])(rec, fr, &ctx, dc) == DISP_COLLIDED)
            fr = (DWORD *)dc[0];
        teb[0] = fr[0];
    }
    if (target == NULL || (void *)fr == target)
        return;
    w32_zero(&again, sizeof again);
    again.code = 0xC0000029UL;
    again.flags = EXC_NONCONT;
    again.next = rec;
    if (!seh_walk(&again, &ctx))
        die(&again);
    die(rec);
}

/* no frame handled it: the filter decides (-1 = resume) */
static int unhandled_ends(ExcRecord *rec, Context *ctx)
{
    ExcPointers p;

    p.rec = rec;
    p.ctx = ctx;
    return UnhandledExceptionFilter(&p) != -1;
}

/* entered on the faulting stack from a K32EXA.ASM stub */
void __cdecl w32_dispatch(ExcRecord *rec, Context *ctx, DWORD n)
{
    w32_zero(rec, sizeof *rec);
    rec->code = codes[n];
    rec->address = (void *)ctx->eip;
    if (n == 13 || n == 14)
        rec->nparams = 2;
    if (!seh_walk(rec, ctx) && unhandled_ends(rec, ctx))
        die(rec);
    w32_resume(ctx);
}

void WINAPI RaiseException(DWORD code, DWORD flags, DWORD n,
                           const DWORD *args)
{
    ExcRecord rec;
    Context ctx;
    DWORD i;
    W32T("RaiseException");

    w32_zero(&rec, sizeof rec);
    w32_zero(&ctx, sizeof ctx);
    rec.code = code;
    rec.flags = flags & EXC_NONCONT;
    rec.nparams = n > 15 ? 15 : n;
    for (i = 0; i < rec.nparams && args; i++)
        rec.info[i] = args[i];
    if (!seh_walk(&rec, &ctx) && unhandled_ends(&rec, &ctx))
        die(&rec);
}

void *WINAPI SetUnhandledExceptionFilter(void *f)
{
    void *old = filter;
    W32T("SetUnhandledExceptionFilter");

    filter = (Filter)f;
    return old;
}

/* EXCEPTION_EXECUTE_HANDLER (1) ends the process */
int WINAPI UnhandledExceptionFilter(void *info)
{
    W32T("UnhandledExceptionFilter");
    return filter ? filter((ExcPointers *)info) : 1;
}

void w32_excinit(void)
{
    struct dregs r;
    DWORD n;

    oldh = w32_pages(EXC_COUNT * sizeof(Handler),
                     &oldhandle);
    if (oldh == NULL)
        return;
    hooked = 1;
    for (n = 0; n < EXC_COUNT; n++) {
        if (!codes[n])
            continue;
        w32_zero(&r, sizeof r);
        r.eax = 0x0202;
        r.ebx = n;
        dpmi31(&r);
        oldh[n].sel = r.ecx & 0xFFFF;
        oldh[n].off = r.edx;
        w32_zero(&r, sizeof r);
        r.eax = 0x0203;
        r.ebx = n;
        r.ecx = get_cs();
        r.edx = (DWORD)w32_excstubs + 16 * n;
        dpmi31(&r);
    }
}

void w32_excfree(void)
{
    struct dregs r;
    DWORD n;

    if (!hooked)
        return;
    for (n = 0; n < EXC_COUNT; n++) {
        if (!codes[n])
            continue;
        w32_zero(&r, sizeof r);
        r.eax = 0x0203;
        r.ebx = n;
        r.ecx = oldh[n].sel;
        r.edx = oldh[n].off;
        dpmi31(&r);
    }
    w32_freepages(oldhandle);
    hooked = 0;
}
