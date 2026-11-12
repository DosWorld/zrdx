/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Shared pieces of the exception tests: console output, FS:[0]
 * access, the NT structures and hand made frames.
 */
#ifndef TSEH_H
#define TSEH_H

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

extern DWORD fs_get(DWORD off);
#pragma aux fs_get = \
    "mov eax, fs:[eax]" \
    parm [eax] \
    value [eax];

extern void fs_put(DWORD off, DWORD v);
#pragma aux fs_put = \
    "mov fs:[eax], edx" \
    parm [eax] [edx] \
    modify [];

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
    unsigned char ext[512];
} Context;

typedef struct ExcPointers {
    ExcRecord *rec;
    Context *ctx;
} ExcPointers;

typedef struct Frame {
    DWORD prev;
    void *handler;
} Frame;

extern void W RaiseException(DWORD, DWORD, DWORD, const DWORD *);

static void push_frame(Frame *f, void *h)
{
    f->prev = fs_get(0);
    f->handler = h;
    fs_put(0, (DWORD)f);
}

static void pop_frame(Frame *f)
{
    fs_put(0, f->prev);
}

#define EH(n) \
    static int __cdecl n(ExcRecord *r, void *f, Context *c, \
                         void *d)

#endif
