/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * UCRTBASE.DLL additions: the entry points that programs built
 * with the Universal CRT of Visual Studio 2015+ call instead of
 * the MSVCRT ones. UCRTBASE.DLL is the same C runtime
 * (CRTCOM.LNK) plus this file.
 */
#include "crt.h"

typedef struct OnExitTable {
    PVFV *first, *last, *end;
} OnExitTable;

#define OPT_STD_SNPRINTF 2

extern int __cdecl vfprintf(FILE *, const char *, va_list);
extern int __cdecl _vsnprintf(char *, unsigned, const char *,
                              va_list);
extern int __cdecl _vscprintf(const char *, va_list);
extern int __cdecl vsprintf(char *, const char *, va_list);
extern int __cdecl atexit(PVFV);
extern unsigned __cdecl _controlfp(unsigned, unsigned);
extern void __cdecl abort(void);
extern int __cdecl __getmainargs(int *, char ***, char ***, int,
                                 void *);
extern char ***__cdecl __p___initenv(void);
extern char *_acmdln;
extern int _fmode;

int __cdecl __stdio_common_vfprintf(DWORD olo, DWORD ohi,
                                    FILE * f, const char *fmt,
                                    void *loc, va_list ap)
{
    return vfprintf(f, fmt, ap);
}

/* count -1: unbounded; OPT_STD_SNPRINTF: C99 snprintf result */
int __cdecl __stdio_common_vsprintf(DWORD olo, DWORD ohi,
                                    char *buf, DWORD count,
                                    const char *fmt, void *loc,
                                    va_list ap)
{
    int n;

    if (count == 0xFFFFFFFFUL)
        return vsprintf(buf, fmt, ap);
    if (!(olo & OPT_STD_SNPRINTF))
        return _vsnprintf(buf, count, fmt, ap);
    n = _vscprintf(fmt, ap);
    if (count) {
        _vsnprintf(buf, count - 1, fmt, ap);
        buf[(DWORD) n < count - 1 ? (DWORD) n : count - 1] = 0;
    }
    return n;
}

/* the _s form: an overflow empties the buffer and fails */
int __cdecl __stdio_common_vsprintf_s(DWORD olo, DWORD ohi,
                                      char *buf, DWORD count,
                                      const char *fmt, void *loc,
                                      va_list ap)
{
    int n = _vscprintf(fmt, ap);

    if (n < 0 || (DWORD) n >= count) {
        if (count)
            buf[0] = 0;
        return -1;
    }
    return vsprintf(buf, fmt, ap);
}

int __cdecl __stdio_common_vfscanf(DWORD olo, DWORD ohi,
                                   FILE * f,
                                   const char *fmt, void *loc,
                                   va_list ap)
{
    return crt_vfscanf(f, fmt, ap);
}

int __cdecl __stdio_common_vsscanf(DWORD olo, DWORD ohi,
                                   const char *s, DWORD len,
                                   const char *fmt, void *loc,
                                   va_list ap)
{
    return crt_vsscanf(s, fmt, ap);
}

FILE *__cdecl __acrt_iob_func(unsigned i)
{
    return &_iob[i];
}

int __cdecl _set_app_type(int type)
{
    return 0;
}

int __cdecl _configure_narrow_argv(int mode)
{
    int argc;
    char **argv, **env;

    return __getmainargs(&argc, &argv, &env, 0, 0) ? -1 : 0;
}

int __cdecl _initialize_narrow_environment(void)
{
    return 0;
}

char **__cdecl _get_initial_narrow_environment(void)
{
    return *__p___initenv();
}

int __cdecl _crt_atexit(PVFV fn)
{
    return atexit(fn);
}

int __cdecl _initialize_onexit_table(OnExitTable * t)
{
    if (!t)
        return -1;
    t->first = t->last = t->end = NULL;
    return 0;
}

int __cdecl _register_onexit_function(OnExitTable * t, PVFV fn)
{
    DWORD cap, used;
    PVFV *p;
    HANDLE heap = GetProcessHeap();

    if (!t)
        return -1;
    if (t->last == t->end) {
        used = (DWORD) (t->last - t->first);
        cap = used ? used * 2 : 16;
        p = t->first ? HeapReAlloc(heap, 8, t->first, cap * 4) :
            HeapAlloc(heap, 8, cap * 4);
        if (!p)
            return -1;
        t->first = p;
        t->last = p + used;
        t->end = p + cap;
    }
    *t->last++ = fn;
    return 0;
}

int __cdecl _execute_onexit_table(OnExitTable * t)
{
    if (!t)
        return -1;
    while (t->last > t->first)
        (*--t->last)();
    if (t->first)
        HeapFree(GetProcessHeap(), 0, t->first);
    t->first = t->last = t->end = NULL;
    return 0;
}

int __cdecl _seh_filter_exe(DWORD code, void *info)
{
    return 0;
}

int __cdecl _set_fmode(int mode)
{
    _fmode = mode;
    return 0;
}

int __cdecl _get_fmode(int *mode)
{
    if (!mode)
        return CRT_EINVAL;
    *mode = _fmode;
    return 0;
}

int __cdecl _set_new_mode(int mode)
{
    return 0;
}

int __cdecl __setusermatherr(void *fn)
{
    return 0;
}

char *__cdecl _get_narrow_winmain_command_line(void)
{
    return _acmdln;
}

char **__cdecl __p__acmdln(void)
{
    return &_acmdln;
}

int __cdecl _controlfp_s(unsigned *cur, unsigned value,
                         unsigned mask)
{
    unsigned r = _controlfp(value, mask);

    if (cur)
        *cur = r;
    return 0;
}

void __cdecl _invalid_parameter_noinfo(void)
{
    crt_errno = CRT_EINVAL;
}

void __cdecl _invalid_parameter_noinfo_noreturn(void)
{
    crt_errno = CRT_EINVAL;
    abort();
}

void *__cdecl _set_invalid_parameter_handler(void *fn)
{
    return NULL;
}

void __cdecl terminate(void)
{
    abort();
}
