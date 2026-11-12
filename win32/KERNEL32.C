/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL for the ZRDX PE service, written from the public
 * API documentation only (docs/CLEANRM.MD, docs/WIN32.MD).
 * This file: errors, process, modules, command line.
 */
#include "w32.h"
#include "zrdxapi.h"

#undef LoadLibrary
#undef GetModuleHandle
#undef GetProcAddress
#undef FreeLibrary
#undef GetLastError

W32_ANCHOR;

static int vok;
static int minfo;
static char mpath[132];
static DWORD mbase, mrec;
static char cmdA[320];
static WORD cmdW[320];
static int cmdok;

#ifdef W32TRACE
void w32_val(const char *tag, DWORD v)
{
    char s[12];
    int i;

    w32_trace(tag);
    for (i = 0; i < 8; i++)
        s[i] = "0123456789ABCDEF"[(v >> (28 - 4 * i)) & 15];
    s[8] = ' ';
    s[9] = '$';
    w32_trace(s);
}
#endif

void w32_seterr(DWORD error)
{
    *w32_errp() = error;
}

void w32_zero(void *p, DWORD bytes)
{
    char *c = p;

    while (bytes--)
        *c++ = 0;
}

void w32_copy(void *dst, const void *src, DWORD bytes)
{
    char *d = dst;
    const char *s = src;

    while (bytes--)
        *d++ = *s++;
}

static DWORD slen(const char *s)
{
    DWORD n = 0;

    while (s[n])
        n++;
    return n;
}

static int vendor(void)
{
    if (!vok && ZrdxInit() == 0)
        vok = 1;
    return vok;
}

/* ABI error of the PE service -> Win32 error */
static DWORD abierr(DWORD code)
{
    switch (code) {
    case 2:
    case 5:
        return ERROR_MOD_NOT_FOUND;
    case 3:
        return 193;
    case 4:
        return ERROR_NOT_ENOUGH_MEMORY;
    case 6:
    case 12:
        return ERROR_PROC_NOT_FOUND;
    case 13:
        return 1114;
    case 14:
        return 120;
    }
    return 31;
}

static int pinfo(void)
{
    if (!minfo) {
        if (!vendor())
            return 0;
        mrec = 0;
        mbase = zrdx_pinfo(mpath, zrdx_vp, &mrec);
        if (!mbase)
            return 0;
        minfo = 1;
    }
    return 1;
}

DWORD WINAPI GetLastError(void)
{
    W32T("GetLastError");
    return *w32_errp();
}

void WINAPI SetLastError(DWORD error)
{
    W32T("SetLastError");
    *w32_errp() = error;
}

DWORD WINAPI GetVersion(void)
{
    W32T("GetVersion");
    return 0x0A280105UL;
}

DWORD WINAPI GetCurrentThreadId(void)
{
    W32T("GetCurrentThreadId");
    return 1;
}

void WINAPI ExitProcess(UINT code)
{
    struct dregs r;
    W32T("ExitProcess");

    if (vendor())
        zrdx_cr(7, 0, (const void *)(DWORD)code, zrdx_vp);
    r.eax = 0x4C00UL | (code & 0xFF);
    dos21(&r);
    for (;;)
        ;
}

BOOL WINAPI SetConsoleCtrlHandler(void *handler, BOOL add)
{
    W32T("SetConsoleCtrlHandler");
    return TRUE;
}

HANDLE WINAPI LoadLibraryA(const char *name)
{
    DWORD h;
    W32T("LoadLibraryA");

    if (name == NULL || !vendor()) {
        w32_seterr(name == NULL ? ERROR_INVALID_PARAMETER :
                   ERROR_MOD_NOT_FOUND);
        return NULL;
    }
    h = zrdx_cz(1, 0, name, zrdx_vp);
    if (!h)
        w32_seterr(abierr(zrdx_cr(5, 0, 0, zrdx_vp)));
    return (HANDLE)h;
}

BOOL WINAPI FreeLibrary(HANDLE mod)
{
    DWORD e;
    W32T("FreeLibrary");

    if (!vendor())
        return FALSE;
    if (pinfo() && (DWORD)mod == mbase)
        mod = (HANDLE)mrec;
    e = zrdx_cr(4, (DWORD)mod, 0, zrdx_vp);
    if (e)
        w32_seterr(abierr(e));
    return e == 0;
}

HANDLE WINAPI GetModuleHandleA(const char *name)
{
    DWORD h;
    W32T("GetModuleHandleA");

    if (name == NULL) {
        if (!pinfo()) {
            w32_seterr(ERROR_MOD_NOT_FOUND);
            return NULL;
        }
        return (HANDLE)mbase;
    }
    h = vendor() ?
        zrdx_cz(2, 0, name, zrdx_vp) : 0;
    if (!h) {
        w32_seterr(ERROR_MOD_NOT_FOUND);
        return NULL;
    }
    if (pinfo() && h == mrec)
        h = mbase;
    return (HANDLE)h;
}

void *WINAPI GetProcAddress(HANDLE mod, const char *name)
{
    DWORD a;
    W32T("GetProcAddress");

    if (name == NULL || !vendor()) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return NULL;
    }
    if (pinfo() && (DWORD)mod == mbase)
        mod = (HANDLE)mrec;
    a = zrdx_cz(3, (DWORD)mod, name, zrdx_vp);
    if (!a)
        w32_seterr(abierr(zrdx_cr(5, 0, 0, zrdx_vp)));
    return (void *)a;
}

/* copies the path of the main program, full name if known
   (the true name call may write 260 characters) */
static DWORD fullpath(char *out)
{
    struct dregs r;

    if (!pinfo())
        return 0;
    w32_zero(&r, sizeof r);
    r.esi = (DWORD)mpath;
    r.edi = (DWORD)out;
    r.ecx = 0;
    w32_dos2(&r, 0x7160, 0x6000);
    if (r.cf)
        w32_copy(out, mpath, slen(mpath) + 1);
    return slen(out);
}

DWORD WINAPI GetModuleFileNameA(HANDLE mod, char *buf,
                                DWORD size)
{
    char full[270];
    DWORD n;
    W32T("GetModuleFileNameA");

    if (mod != NULL && !(pinfo() && (DWORD)mod == mbase)) {
        w32_seterr(ERROR_MOD_NOT_FOUND);
        return 0;
    }
    n = fullpath(full);
    if (n == 0 || size == 0) {
        w32_seterr(n == 0 ? ERROR_MOD_NOT_FOUND :
                   ERROR_INSUFFICIENT_BUFFER);
        return 0;
    }
    if (n >= size) {
        w32_copy(buf, full, size);
        w32_seterr(ERROR_INSUFFICIENT_BUFFER);
        return size;
    }
    w32_copy(buf, full, n + 1);
    return n;
}

DWORD WINAPI GetModuleFileNameW(HANDLE mod, WORD *buf,
                                DWORD size)
{
    char tmp[270];
    DWORD n, i;
    W32T("GetModuleFileNameW");

    n = GetModuleFileNameA(mod, tmp, sizeof tmp);
    if (n == 0)
        return 0;
    if (n >= size) {
        n = size;
        w32_seterr(ERROR_INSUFFICIENT_BUFFER);
    }
    for (i = 0; i < n; i++)
        buf[i] = (BYTE)tmp[i];
    if (n < size)
        buf[n] = 0;
    return n;
}

/* linear address of the PSP of the program, 0 if unknown */
char *w32_psp(void)
{
    struct dregs r;

    w32_zero(&r, sizeof r);
    r.eax = 0x6200;
    dos21(&r);
    if (r.cf)
        return NULL;
    r.eax = 0x0006;
    r.ebx &= 0xFFFF;
    dpmi31(&r);
    if (r.cf)
        return NULL;
    return (char *)(((r.ecx & 0xFFFF) << 16) | (r.edx & 0xFFFF));
}

/* program path and the tail of the PSP (the loader removed
   its own name from the tail) */
static void buildcmd(void)
{
    char *psp;
    DWORD n = 0, i = 0, len;

    if (cmdok)
        return;
    cmdok = 1;
    if (pinfo()) {
        n = slen(mpath);
        w32_copy(cmdA, mpath, n);
    }
    cmdA[n] = 0;
    psp = w32_psp();
    if (psp == NULL)
        return;
    len = (BYTE)psp[0x80];
    while (i < len && psp[0x81 + i] == ' ')
        i++;
    if (i < len && psp[0x81 + i] != 13) {
        cmdA[n++] = ' ';
        while (i < len && psp[0x81 + i] != 13 &&
                n + 1 < sizeof cmdA)
            cmdA[n++] = psp[0x81 + i++];
    }
    cmdA[n] = 0;
}

char *WINAPI GetCommandLineA(void)
{
    W32T("GetCommandLineA");
    buildcmd();
    return cmdA;
}

WORD *WINAPI GetCommandLineW(void)
{
    DWORD i = 0;
    W32T("GetCommandLineW");

    buildcmd();
    do
        cmdW[i] = (BYTE)cmdA[i];
    while (cmdA[i++]);
    return cmdW;
}

/* not named DllMain: Watcom then references __DLLstart_ */
/* x87 control word of a Win32 process: 53 bit precision, all
   exceptions masked */
extern void fp_init(void);
#pragma aux fp_init = \
    "push 27Fh" \
    "fldcw [esp]" \
    "pop eax" \
    modify [eax];

BOOL WINAPI W32Main(HANDLE inst, DWORD reason, LPVOID reserved)
{
    DWORD sel, tls;

    if (reason == 1) {
        fp_init();
        sel = w32_tebinit(pinfo() ? mbase : 0,
                          GetCurrentProcessId(),
                          (DWORD)GetProcessHeap());
        if (sel && vendor())
            ZrdxSetFS(sel);
        if (vendor() && (tls = ZrdxTlsTable()) > 0xFFFFUL)
            w32_tebtls(tls);
        w32_excinit();
    }
    W32T("W32Main");
    if (reason == 0) {
        w32_objfree();
        w32_envfree();
        w32_heapfree();
        w32_tlsfree();
        w32_memfree();
        w32_excfree();
        w32_tebfree();
    }
    return TRUE;
}
