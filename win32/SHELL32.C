/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * SHELL32.DLL: the minimum for console programs: the command
 * line parser, folder paths (the current directory) and
 * ShellExecute that always fails (docs/WIN32.MD, task 214).
 */
#include "w32.h"

#define S_OK 0L
#define E_INVALIDARG 0x80070057UL
#define MAX_PATH 260
#define SE_ERR_FNF 2

W32_ANCHOR;

extern HANDLE WINAPI LocalAlloc(UINT flags, DWORD size);
extern DWORD WINAPI GetModuleFileNameW(HANDLE mod, WORD *buf,
                                       DWORD size);
extern DWORD WINAPI GetCurrentDirectoryA(DWORD size, char *buf);
extern int WINAPI MultiByteToWideChar(UINT, DWORD, const char *,
                                      int, WORD *, int);

/* output of the parser: only counts when p is NULL */
typedef struct Out {
    WORD *p;
    DWORD n;
} Out;

static int blank(WORD c)
{
    return c == ' ' || c == '\t';
}

static void put(Out *o, WORD c)
{
    if (o->p)
        o->p[o->n] = c;
    o->n++;
}

/* program name: up to the closing quote or the first blank */
static const WORD *first_arg(const WORD *s, Out *o)
{
    int inq = *s == '"';

    s += inq;
    while (*s && (inq ? *s != '"' : !blank(*s)))
        put(o, *s++);
    return s + (inq && *s == '"');
}

/* other arguments: 2n backslashes and a quote give n and a
   delimiter, 2n+1 give n and a quote character */
static const WORD *next_arg(const WORD *s, Out *o)
{
    int inq = 0;
    DWORD bs;

    while (*s && (inq || !blank(*s))) {
        for (bs = 0; *s == '\\'; s++)
            bs++;
        if (*s == '"') {
            for (; bs >= 2; bs -= 2)
                put(o, '\\');
            if (bs)
                put(o, '"');
            else
                inq = !inq;
            s++;
            continue;
        }
        for (; bs; bs--)
            put(o, '\\');
        if (*s && (inq || !blank(*s)))
            put(o, *s++);
    }
    return s;
}

/* one pass over the command line; argv gets the starts */
static int split(const WORD *s, Out *o, WORD **argv)
{
    int n = 0;

    for (;;) {
        while (blank(*s))
            s++;
        if (*s == 0)
            return n;
        if (argv)
            argv[n] = o->p + o->n;
        s = n == 0 ? first_arg(s, o) : next_arg(s, o);
        put(o, 0);
        n++;
    }
}

/* the block is the pointer array followed by the text */
WORD **WINAPI CommandLineToArgvW(const WORD *cmd, int *argc)
{
    static WORD none[1];
    WORD path[MAX_PATH], **argv;
    Out o;
    int n;

    if (cmd == NULL)
        return NULL;
    if (*cmd == 0) {
        GetModuleFileNameW(NULL, path, MAX_PATH);
        cmd = path[0] ? path : none;
    }
    o.p = NULL;
    o.n = 0;
    n = split(cmd, &o, NULL);
    argv = (WORD **)LocalAlloc(0, (DWORD)(n + 1) * 4 + o.n * 2);
    if (argv == NULL)
        return NULL;
    o.p = (WORD *)(argv + n + 1);
    o.n = 0;
    split(cmd, &o, argv);
    argv[n] = NULL;
    *argc = n;
    return argv;
}

/* the folders that exist: all are the current directory */
static long folder_ok(int csidl)
{
    switch (csidl & 0xFF) {
    case 0x00:
    case 0x05:
    case 0x10:
    case 0x1A:
    case 0x1C:
    case 0x23:
    case 0x24:
    case 0x25:
    case 0x26:
    case 0x28:
        return S_OK;
    }
    return (long)E_INVALIDARG;
}

long WINAPI SHGetFolderPathA(HANDLE wnd, int csidl, HANDLE token,
                             DWORD flags, char *path)
{
    long r = folder_ok(csidl);

    if (r == S_OK && GetCurrentDirectoryA(MAX_PATH, path) == 0)
        r = (long)E_INVALIDARG;
    return r;
}

long WINAPI SHGetFolderPathW(HANDLE wnd, int csidl, HANDLE token,
                             DWORD flags, WORD *path)
{
    char tmp[MAX_PATH];
    long r = SHGetFolderPathA(wnd, csidl, token, flags, tmp);

    if (r == S_OK)
        MultiByteToWideChar(0, 0, tmp, -1, path, MAX_PATH);
    return r;
}

BOOL WINAPI SHGetSpecialFolderPathA(HANDLE wnd, char *path,
                                    int csidl, BOOL create)
{
    return SHGetFolderPathA(wnd, csidl, NULL, 0, path) == S_OK;
}

/* nothing is started: SE_ERR_FNF (a result <= 32 is an error) */
HANDLE WINAPI ShellExecuteA(HANDLE wnd, const char *verb,
                            const char *file, const char *params,
                            const char *dir, int show)
{
    return (HANDLE) SE_ERR_FNF;
}

HANDLE WINAPI ShellExecuteW(HANDLE wnd, const WORD * verb,
                            const WORD * file,
                            const WORD * params,
                            const WORD * dir, int show)
{
    return (HANDLE) SE_ERR_FNF;
}
