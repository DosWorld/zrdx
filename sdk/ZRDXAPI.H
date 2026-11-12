/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * ZRDXAPI.H: Open Watcom C wrappers of the ZRDX vendor API
 * for a flat 32-bit program built with wcc386 -s -ecc (see
 * docs/SDK.MD). Call ZrdxInit once, then the others. A return
 * value 0 means failure for the first three (GetLastError
 * tells why); FreeLibrary returns 0 or an error code.
 */
#ifndef ZRDXAPI_H
#define ZRDXAPI_H

static unsigned long zrdx_vp[2];

/* INT 2Fh 168Ah: esi = name, ebx = far pointer storage; the
   call returns the entry selector in ES, which is restored */
extern unsigned zrdx_find(const char *name, unsigned long *vp);
#pragma aux zrdx_find = \
    "push es" \
    "mov ax, 168Ah" \
    "int 2Fh" \
    "mov [ebx], edi" \
    "mov [ebx + 4], es" \
    "pop es" \
    "movzx eax, al" \
    parm [esi] [ebx] \
    modify [edi] \
    value [eax];

/* far call of the entry, eax = 0 on failure */
extern unsigned long zrdx_cz(unsigned long fn, unsigned long ebx,
                             const void *edx, unsigned long *vp);
#pragma aux zrdx_cz = \
    "call fword ptr [esi]" \
    "sbb ecx, ecx" \
    "not ecx" \
    "and eax, ecx" \
    parm [eax] [ebx] [edx] [esi] \
    modify [ecx] \
    value [eax];

/* far call of the entry, eax as returned */
extern unsigned long zrdx_cr(unsigned long fn, unsigned long ebx,
                             const void *edx, unsigned long *vp);
#pragma aux zrdx_cr = \
    "call fword ptr [esi]" \
    parm [eax] [ebx] [edx] [esi] \
    value [eax];

/* fn 6: path of the main program to buf, eax = its image base,
   *rec = its module handle; eax = 0 on failure */
extern unsigned long zrdx_pinfo(char *buf, unsigned long *vp,
                                unsigned long *rec);
#pragma aux zrdx_pinfo = \
    "mov eax, 6" \
    "call fword ptr [esi]" \
    "mov [edi], ebx" \
    "sbb ecx, ecx" \
    "not ecx" \
    "and eax, ecx" \
    parm [edx] [esi] [edi] \
    modify [ebx ecx] \
    value [eax];

/* returns 0 if the service answers */
#define ZrdxInit() zrdx_find("ZRDX", zrdx_vp)
#define LoadLibrary(path) \
    zrdx_cz(1, 0, (path), zrdx_vp)
#define GetModuleHandle(name) \
    zrdx_cz(2, 0, (name), zrdx_vp)
#define GetProcAddress(h, name) \
    zrdx_cz(3, (h), (name), zrdx_vp)
#define FreeLibrary(h) zrdx_cr(4, (h), 0, zrdx_vp)
#define GetLastError() zrdx_cr(5, 0, 0, zrdx_vp)
/* fn 7: detach all DLLs like the exit stub */
#define ZrdxDetach() zrdx_cr(7, 0, 0, zrdx_vp)
/* fn 9: the array behind FS:[2Ch], one TLS block per module */
#define ZrdxTlsTable() zrdx_cr(9, 0, 0, zrdx_vp)
/* fn 8: FS selector the program starts with (default 0) */
#define ZrdxSetFS(sel) \
    zrdx_cr(8, 0, (const void *)(unsigned long)(sel), zrdx_vp)

#endif
