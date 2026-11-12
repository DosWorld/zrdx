/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: the TEB and the PEB of the single thread
 * (docs/WIN32.MD, task 204). FS selects the TEB; the last
 * error lives in it (offset 34h).
 */
#include "w32.h"

/* TEB offsets in DWORDs */
#define T_CHAIN 0
#define T_SBASE 1
#define T_SLIMIT 2
#define T_SELF 6
#define T_PID 8
#define T_TID 9
#define T_TLS 11
#define T_PEB 12
#define T_ERR 13

/* PEB offsets in DWORDs */
#define P_IMAGE 2
#define P_HEAP 6
#define P_OSMAJ 41
#define P_OSMIN 42
#define P_OSBLD 43

#define TEB_SIZE 0x1000UL
#define STACK_SPAN 0x100000UL
#define MAX_RESERVE 0x10000000UL

static DWORD *teb;
static DWORD tebhandle;
static DWORD tebsel;
static DWORD fallback;

extern DWORD get_esp(void);
#pragma aux get_esp = \
    "mov eax, esp" \
    value [eax];

extern void set_fs(DWORD sel);
#pragma aux set_fs = \
    "mov fs, ax" \
    parm [eax];

/* FS:[2Ch], the loader table of the static TLS blocks */
void w32_tebtls(DWORD table)
{
    if (teb)
        teb[T_TLS] = table;
}

/* the TEB, NULL before w32_tebinit */
DWORD *w32_tebp(void)
{
    return teb;
}

/* address of the last error of the current thread */
DWORD *w32_errp(void)
{
    return teb ? &teb[T_ERR] : &fallback;
}

/* selector with base and limit of the TEB, 0 on failure */
static DWORD teb_selector(DWORD base)
{
    struct dregs r;
    DWORD sel;

    w32_zero(&r, sizeof r);
    r.eax = 0;
    r.ecx = 1;
    dpmi31(&r);
    if (r.cf)
        return 0;
    sel = r.eax & 0xFFFF;
    w32_zero(&r, sizeof r);
    r.eax = 7;
    r.ebx = sel;
    r.ecx = base >> 16;
    r.edx = base & 0xFFFF;
    dpmi31(&r);
    if (!r.cf) {
        w32_zero(&r, sizeof r);
        r.eax = 8;
        r.ebx = sel;
        r.edx = TEB_SIZE - 1;
        dpmi31(&r);
    }
    if (!r.cf) {
        w32_zero(&r, sizeof r);
        r.eax = 9;
        r.ebx = sel;
        r.ecx = 0x40F2;
        dpmi31(&r);
    }
    if (r.cf) {
        r.eax = 1;
        r.ebx = sel;
        dpmi31(&r);
        return 0;
    }
    return sel;
}

/* SizeOfStackReserve of the image, or a default */
static DWORD stack_span(DWORD image)
{
    DWORD pe, span = 0;

    if (image) {
        pe = image + *(DWORD *)(image + 0x3C);
        span = *(DWORD *)(pe + 96);
    }
    return (span && span <= MAX_RESERVE) ? span : STACK_SPAN;
}

/* builds the TEB + PEB block, loads FS; returns the selector */
DWORD w32_tebinit(DWORD imagebase, DWORD pid, DWORD heap)
{
    DWORD *peb;
    DWORD esp = get_esp();

    teb = w32_pages(0x2000UL, &tebhandle);
    if (teb == NULL)
        return 0;
    tebsel = teb_selector((DWORD)teb);
    if (!tebsel) {
        w32_freepages(tebhandle);
        teb = NULL;
        return 0;
    }
    peb = teb + 0x1000 / 4;
    teb[T_CHAIN] = 0xFFFFFFFFUL;
    /* DllMain runs just below the page-aligned stack top */
    esp = (esp | 0xFFFUL) + 1;
    teb[T_SBASE] = esp;
    teb[T_SLIMIT] = esp - stack_span(imagebase);
    teb[T_SELF] = (DWORD)teb;
    teb[T_PID] = pid;
    teb[T_TID] = 1;
    teb[T_PEB] = (DWORD)peb;
    peb[P_IMAGE] = imagebase;
    peb[P_HEAP] = heap;
    peb[P_OSMAJ] = 10;
    peb[P_OSMIN] = 0;
    peb[P_OSBLD] = 0x2800;
    set_fs(tebsel);
    return tebsel;
}

void w32_tebfree(void)
{
    struct dregs r;

    if (teb == NULL)
        return;
    set_fs(0);
    w32_zero(&r, sizeof r);
    r.eax = 1;
    r.ebx = tebsel;
    dpmi31(&r);
    fallback = teb[T_ERR];
    w32_freepages(tebhandle);
    teb = NULL;
}
