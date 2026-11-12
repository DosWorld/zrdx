/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL for the ZRDX PE service, written from the ISO C
 * standard and the documented behaviour (docs/CLEANRM.MD,
 * docs/MSVCRT.MD). This file: start-up, termination, argument
 * and environment vectors, floating point control.
 */
#include "crt.h"

#define AT_PER 1000
#define MCW_EM 0x8001FUL
#define MCW_RC 0x300UL
#define MCW_PC 0x30000UL

W32_ANCHOR;

typedef struct AtChunk {
    PVFV fn[AT_PER];
    DWORD count;
    struct AtChunk *next;
} AtChunk;

char *_acmdln;
WORD *_wcmdln;
int _fmode;
int _commode;
static int app_type;
static AtChunk *atexits;
static char **argv_blk;
static char **env_blk;
static char **initenv;
static int argc_val;

static void *crt_alloc(DWORD n)
{
    return HeapAlloc(GetProcessHeap(), 8, n);
}

static void crt_free(void *p)
{
    if (p)
        HeapFree(GetProcessHeap(), 0, p);
}

void crt_write_err(const char *text)
{
    DWORD n = 0, done;

    while (text[n])
        n++;
    WriteFile(GetStdHandle(0xFFFFFFF4UL), text, n, &done, NULL);
}

int __cdecl atexit(PVFV fn)
{
    AtChunk *c = atexits;

    if (fn == NULL)
        return -1;
    if (c == NULL || c->count == AT_PER) {
        c = crt_alloc(sizeof(AtChunk));
        if (c == NULL)
            return -1;
        c->next = atexits;
        atexits = c;
    }
    c->fn[c->count++] = fn;
    return 0;
}

PVFV __cdecl _onexit(PVFV fn)
{
    return atexit(fn) ? NULL : fn;
}

/* appends fn to the table *pbegin .. *pend of a DLL */
PVFV *__cdecl __dllonexit(PVFV fn, PVFV **pbegin, PVFV **pend)
{
    DWORD n = (DWORD)(*pend - *pbegin);
    PVFV *t;

    t = *pbegin == NULL ? crt_alloc((n + 1) * sizeof(PVFV)) :
        HeapReAlloc(GetProcessHeap(), 8, *pbegin,
                    (n + 1) * sizeof(PVFV));
    if (t == NULL)
        return NULL;
    t[n] = fn;
    *pbegin = t;
    *pend = t + n + 1;
    return t;
}

/* runs the atexit handlers, newest first, each once */
static void run_atexit(void)
{
    AtChunk *c;
    PVFV fn;

    while ((c = atexits) != NULL) {
        if (c->count == 0) {
            atexits = c->next;
            crt_free(c);
            continue;
        }
        fn = c->fn[--c->count];
        fn();
    }
}

void __cdecl _cexit(void)
{
    run_atexit();
    crt_flush_streams();
}

void __cdecl _c_exit(void)
{
}

void __cdecl exit(int code)
{
    run_atexit();
    crt_flush_streams();
    ExitProcess((UINT)code);
}

void __cdecl _exit(int code)
{
    ExitProcess((UINT)code);
}

void crt_exit(int code)
{
    exit(code);
}

extern int __cdecl raise(int);

void __cdecl abort(void)
{
    raise(22);
    crt_write_err("\r\nabnormal program termination\r\n");
    ExitProcess(3);
}

void __cdecl _amsg_exit(int num)
{
    char msg[40] = "\r\nruntime error R6000\r\n";

    msg[17] = (char)('0' + (num / 100) % 10);
    msg[18] = (char)('0' + (num / 10) % 10);
    msg[19] = (char)('0' + num % 10);
    crt_write_err(msg);
    ExitProcess(255);
}

int __cdecl _XcptFilter(DWORD code, void *info)
{
    return 0;
}

void __cdecl _initterm(PVFV * begin, PVFV * end)
{
    for (; begin < end; begin++)
        if (*begin)
            (*begin)();
}

int __cdecl _initterm_e(PIFV * begin, PIFV * end)
{
    int r;

    for (; begin < end; begin++)
        if (*begin && (r = (*begin)()) != 0)
            return r;
    return 0;
}

void __cdecl __set_app_type(int type)
{
    app_type = type;
}

int *__cdecl __p__fmode(void)
{
    return &_fmode;
}

int *__cdecl __p__commode(void)
{
    return &_commode;
}

#define PUT(c) do { if (argv) *o = (c); o++; } while (0)

/* the command line is split by the documented quoting rules:
   2n backslashes + quote = n backslashes and a quote switch,
   2n+1 backslashes + quote = n backslashes and a quote; the
   program name in quotes has no escapes */
static int split(const char *s, char **argv, char *buf)
{
    int argc = 0, inq, n;
    char *o = buf;

    for (;;) {
        while (*s == ' ' || *s == '\t')
            s++;
        if (*s == 0)
            break;
        if (argv)
            argv[argc] = o;
        argc++;
        if (argc == 1 && *s == '"') {
            for (s++; *s && *s != '"'; s++)
                PUT(*s);
            if (*s)
                s++;
        } else {
            inq = 0;
            while (*s && (inq || (*s != ' ' && *s != '\t'))) {
                for (n = 0; *s == '\\'; s++)
                    n++;
                if (*s == '"') {
                    for (; n >= 2; n -= 2)
                        PUT('\\');
                    if (n)
                        PUT('"');
                    else
                        inq = !inq;
                    s++;
                } else {
                    for (; n > 0; n--)
                        PUT('\\');
                    if (*s) {
                        PUT(*s);
                        s++;
                    }
                }
            }
        }
        if (argv)
            *o = 0;
        o++;
    }
    if (argv)
        argv[argc] = NULL;
    return argc;
}

/* test hook (not a Microsoft export): the splitter */
int __cdecl __zrdx_split(const char *cmd, char **argv, char *buf)
{
    return split(cmd, argv, buf);
}

/* argument and environment vectors are built once and kept */
static int build_vectors(void)
{
    const char *cmd = _acmdln;
    DWORD len = 0, count = 0, i;
    char *blk, *e;

    if (argv_blk)
        return 1;
    while (cmd[len])
        len++;
    argv_blk = crt_alloc((len / 2 + 3) * sizeof(char *) + len
                         + 2);
    if (argv_blk == NULL)
        return 0;
    blk = (char *)(argv_blk + len / 2 + 3);
    argc_val = split(cmd, argv_blk, blk);
    e = GetEnvironmentStringsA();
    if (e == NULL)
        return 0;
    for (i = 0; e[i]; i += lstrlenA(e + i) + 1)
        count++;
    env_blk = crt_alloc((count + 1) * sizeof(char *));
    if (env_blk == NULL)
        return 0;
    for (count = 0, i = 0; e[i]; i += lstrlenA(e + i) + 1)
        env_blk[count++] = e + i;
    initenv = env_blk;
    return 1;
}

int __cdecl __getmainargs(int *argc, char ***argv, char ***env,
                          int wildcard, void *startinfo)
{
    if (!build_vectors())
        return -1;
    *argc = argc_val;
    *argv = argv_blk;
    *env = env_blk;
    return 0;
}

char ***__cdecl __p___initenv(void)
{
    build_vectors();
    return &initenv;
}

char ***__cdecl __p__environ(void)
{
    build_vectors();
    return &env_blk;
}

int *__cdecl __p___argc(void)
{
    build_vectors();
    return &argc_val;
}

char ***__cdecl __p___argv(void)
{
    build_vectors();
    return &argv_blk;
}

/* control word: Microsoft bits <-> x87 bits (_controlfp) */
static unsigned to_ms(unsigned cw)
{
    unsigned ms = 0;

    ms |= (cw & 0x01) ? 0x10 : 0;
    ms |= (cw & 0x02) ? 0x80000UL : 0;
    ms |= (cw & 0x04) ? 0x08 : 0;
    ms |= (cw & 0x08) ? 0x04 : 0;
    ms |= (cw & 0x10) ? 0x02 : 0;
    ms |= (cw & 0x20) ? 0x01 : 0;
    ms |= (cw >> 2) & MCW_RC;
    ms |= (cw & 0x300) == 0 ? 0x20000UL :
          (cw & 0x300) == 0x200 ? 0x10000UL : 0;
    return ms;
}

static unsigned from_ms(unsigned ms, unsigned cw)
{
    cw &= ~0x33FUL;
    cw |= (ms & 0x10) ? 0x01 : 0;
    cw |= (ms & 0x80000UL) ? 0x02 : 0;
    cw |= (ms & 0x08) ? 0x04 : 0;
    cw |= (ms & 0x04) ? 0x08 : 0;
    cw |= (ms & 0x02) ? 0x10 : 0;
    cw |= (ms & 0x01) ? 0x20 : 0;
    cw |= (ms & MCW_RC) << 2;
    cw |= (ms & MCW_PC) == 0x20000UL ? 0 :
          (ms & MCW_PC) == 0x10000UL ? 0x200 : 0x300;
    return cw;
}

unsigned __cdecl _controlfp(unsigned value, unsigned mask)
{
    unsigned ms = to_ms(crt_getcw());

    mask &= MCW_EM | MCW_RC | MCW_PC;
    if (mask) {
        ms = (ms & ~mask) | (value & mask);
        crt_setcw(from_ms(ms, crt_getcw()));
        ms = to_ms(crt_getcw());
    }
    return ms;
}

void __cdecl _fpreset(void)
{
    crt_setcw(0x27F);
}

BOOL WINAPI CrtMain(HANDLE inst, DWORD reason, LPVOID reserved)
{
    AtChunk *c, *n;

    if (reason == 1) {
        crt_ctype_init();
        crt_clock_start();
        _acmdln = GetCommandLineA();
        _wcmdln = GetCommandLineW();
    } else if (reason == 0) {
        for (c = atexits; c; c = n) {
            n = c->next;
            crt_free(c);
        }
        atexits = NULL;
        crt_io_free();
        crt_lib_free();
        crt_wide_free();
        crt_free(argv_blk);
        crt_free(env_blk);
        argv_blk = NULL;
        env_blk = NULL;
    }
    return TRUE;
}
