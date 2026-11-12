/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 209: a DLL with a TLS directory and TLS callbacks.
 * ADDTLS.EXE adds the directory. The log shows the order:
 * callback (100h + reason) before DllMain (reason).
 */
typedef unsigned long DWORD;

extern DWORD fs_get(DWORD off);
#pragma aux fs_get = \
    "mov eax, fs:[eax]" \
    parm [eax] \
    value [eax];

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

#define RAWLEN 8
#define ZEROLEN 12

static char raw[RAWLEN] = "dll-tls";
static char rawend;
static DWORD tls_index = 0xFFFFUL;
static DWORD log[8];
static DWORD nlog;

static void __stdcall tls_cb(void *h, DWORD reason, void *rsv)
{
    log[nlog++] = 0x100 + reason;
    if (reason == 0)
        dos_print("TLSD cb detach\r\n$");
}

static void *callbacks[2] = {(void *)tls_cb, 0};

static struct {
    char mark[8];
    DWORD start, end, index, callbacks, zero, flags;
} dir = {
    "TLSDIR01", (DWORD)raw, (DWORD)&rawend, (DWORD)&tls_index,
    (DWORD)callbacks, ZEROLEN, 0
};

int __stdcall DllEntry(void *h, DWORD reason, void *rsv)
{
    log[nlog++] = reason;
    if (reason == 0)
        dos_print("TLSD main detach\r\n$");
    return 1;
}

char *__stdcall TlsData(void)
{
    return ((char **)fs_get(0x2C))[tls_index];
}

DWORD *__stdcall TlsLog(void)
{
    return log;
}

DWORD __stdcall TlsLen(void)
{
    return (DWORD)&rawend - (DWORD)raw;
}

DWORD __stdcall TlsIndex(void)
{
    return tls_index;
}
