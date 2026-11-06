/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Common definitions of the Win32 DLLs (docs/WIN32.MD): types,
 * the calling convention, DOS and DPMI calls and the internal
 * interfaces between the modules. Build rules are in
 * BUILD.BAT: wcc386 -s -ecc -bt=nt, no C runtime.
 */
#ifndef W32_H
#define W32_H

#define WINAPI __stdcall
#define NULL 0

typedef unsigned long DWORD;
typedef unsigned short WORD;
typedef unsigned char BYTE;
typedef int BOOL;
typedef void *HANDLE;
typedef void *LPVOID;
typedef unsigned UINT;

#define FALSE 0
#define TRUE 1
#define INVALID_HANDLE_VALUE ((HANDLE)-1L)

#define ERROR_FILE_NOT_FOUND 2
#define ERROR_ACCESS_DENIED 5
#define ERROR_INVALID_HANDLE 6
#define ERROR_NOT_ENOUGH_MEMORY 8
#define ERROR_INVALID_PARAMETER 87
#define ERROR_INSUFFICIENT_BUFFER 122
#define ERROR_ALREADY_EXISTS 183
#define ERROR_INVALID_ADDRESS 487
#define ERROR_MOD_NOT_FOUND 126
#define ERROR_PROC_NOT_FOUND 127
#define ERROR_ENVVAR_NOT_FOUND 203
#define ERROR_NO_UNICODE_TRANSLATION 1113

/* registers of one INT 21h or 31h call, cf is -1 on failure */
struct dregs
{
    DWORD eax, ebx, ecx, edx, esi, edi, cf;
};

extern void dos21(struct dregs *r);
#pragma aux dos21 = \
    "push ebp" \
    "mov ebp, eax" \
    "mov eax, [ebp]" \
    "mov ebx, [ebp + 4]" \
    "mov ecx, [ebp + 8]" \
    "mov edx, [ebp + 12]" \
    "mov esi, [ebp + 16]" \
    "mov edi, [ebp + 20]" \
    "int 21h" \
    "mov [ebp], eax" \
    "mov [ebp + 4], ebx" \
    "mov [ebp + 8], ecx" \
    "mov [ebp + 12], edx" \
    "mov [ebp + 16], esi" \
    "mov [ebp + 20], edi" \
    "sbb eax, eax" \
    "mov [ebp + 24], eax" \
    "pop ebp" \
    parm [eax] \
    modify [eax ebx ecx edx esi edi];

extern void dpmi31(struct dregs *r);
#pragma aux dpmi31 = \
    "push ebp" \
    "mov ebp, eax" \
    "mov eax, [ebp]" \
    "mov ebx, [ebp + 4]" \
    "mov ecx, [ebp + 8]" \
    "mov edx, [ebp + 12]" \
    "mov esi, [ebp + 16]" \
    "mov edi, [ebp + 20]" \
    "int 31h" \
    "mov [ebp], eax" \
    "mov [ebp + 4], ebx" \
    "mov [ebp + 8], ecx" \
    "mov [ebp + 12], edx" \
    "mov [ebp + 16], esi" \
    "mov [ebp + 20], edi" \
    "sbb eax, eax" \
    "mov [ebp + 24], eax" \
    "pop ebp" \
    parm [eax] \
    modify [eax ebx ecx edx esi edi];

/*
 * Debug build (-dW32TRACE): every exported function prints its
 * name before it runs, to find where a program stops.
 */
#ifdef W32TRACE
extern void w32_trace(const char *text);
#pragma aux w32_trace = \
    "push eax" \
    "mov ah, 9" \
    "int 21h" \
    "pop eax" \
    parm [edx];
#define W32T(n) w32_trace("<" n ">$")
#define W32V(tag, v) w32_val(tag "=$", (DWORD)(v))
extern void w32_val(const char *tag, DWORD v);
#else
#define W32T(n)
#define W32V(tag, v)
#endif

/*
 * wlink drops the relocation table of a DLL that has no fixup
 * and marks it "relocations stripped", which the ZRDX loader
 * cannot place. One initialised pointer to its own address in
 * every DLL forces a table (use W32_ANCHOR once per DLL).
 */
#define W32_ANCHOR static void *w32_anchor = &w32_anchor

/* kernel32.c */
extern void w32_seterr(DWORD error);
extern void w32_zero(void *p, DWORD bytes);
extern void w32_copy(void *dst, const void *src, DWORD bytes);
extern char *w32_psp(void);

/* k32file.c: HANDLE = 4 * (DOS handle + 1), objects >= 10000h */
extern HANDLE w32_hdos(int dos);
extern int w32_dosh(HANDLE h);
extern DWORD w32_doserr(const struct dregs *r);

/* k32mem.c: zero filled DPMI blocks, handle 0 = failure */
extern void *w32_pages(DWORD bytes, DWORD *handle);
extern void w32_freepages(DWORD handle);
extern void w32_memfree(void);

/* k32obj.c: kernel objects (events) */
extern BOOL w32_objclose(HANDLE h);
extern void w32_objfree(void);

/* k32env.c: the environment block */
extern void w32_envfree(void);

/* k32fs.c: DOS call with an LFN function first, a plain one when
   the LFN API is absent (AX = 7100h); r keeps its input */
extern void w32_dos2(struct dregs *r, DWORD lfnax,
                     DWORD plainax);

/* k32cp.c (generated): upper half of a single byte code page,
   NULL for Latin-1 (and for unknown pages) */
extern const WORD *w32_cptab(DWORD cp);

/* k32str.c / k32txt.c: text */
extern DWORD w32_realcp(DWORD cp);
extern DWORD w32_upper(DWORD c);
extern DWORD w32_lower(DWORD c);

/* k32heap.c: heaps */
extern void w32_heapfree(void);

/* k32tls.c: TLS slots */
extern void w32_tlsfree(void);

#endif
