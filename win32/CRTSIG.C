/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: signal and raise (software signals only; no
 * hardware exception or Ctrl-C delivery).
 */
#include "crt.h"

#define SIG_DFL ((PVFV)0)
#define SIG_IGN ((PVFV)1)
#define SIG_ERR ((PVFV)-1)
#define SIGABRT 22

typedef void (__cdecl * SigFn)(int);

/* SIGINT 2, SIGILL 4, SIGFPE 8, SIGSEGV 11, SIGTERM 15,
   SIGBREAK 21, SIGABRT 22 */
static const unsigned char known[] = { 2, 4, 8, 11, 15, 21, 22 };
static SigFn handler[7];

static int slot(int sig)
{
    int i;

    for (i = 0; i < 7; i++)
        if (known[i] == sig)
            return i;
    return -1;
}

SigFn __cdecl signal(int sig, SigFn fn)
{
    int i = slot(sig);
    SigFn old;

    if (i < 0 || fn == (SigFn) SIG_ERR) {
        crt_errno = CRT_EINVAL;
        return (SigFn) SIG_ERR;
    }
    old = handler[i];
    handler[i] = fn;
    return old;
}

int __cdecl raise(int sig)
{
    int i = slot(sig);
    SigFn fn;

    if (i < 0)
        return -1;
    fn = handler[i];
    if (fn == (SigFn) SIG_IGN)
        return 0;
    if (fn != (SigFn) SIG_DFL) {
        handler[i] = (SigFn) SIG_DFL;
        fn(sig);
        return 0;
    }
    if (sig == SIGABRT) {
        crt_write_err("\r\nabnormal program termination\r\n");
        ExitProcess(3);
    }
    ExitProcess(3);
    return 0;
}
