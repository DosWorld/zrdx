/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 194: the Universal CRT entry points of UCRTBASE.DLL.
 * Prints "UCRT: ok" or "UCRT: fail NN".
 */
#include <stdarg.h>

typedef struct OnExit {
    void (__cdecl ** first)(void);
    void (__cdecl ** last)(void);
    void (__cdecl ** end)(void);
} OnExit;

#define IMP extern int __cdecl
IMP __stdio_common_vfprintf(unsigned long, unsigned long, void *,
                            const char *, void *, va_list);
IMP __stdio_common_vsprintf(unsigned long, unsigned long, char *,
                            unsigned, const char *, void *,
                            va_list);
IMP __stdio_common_vsprintf_s(unsigned long, unsigned long,
                              char *, unsigned, const char *,
                              void *, va_list);
IMP __stdio_common_vfscanf(unsigned long, unsigned long, void *,
                           const char *, void *, va_list);
IMP __stdio_common_vsscanf(unsigned long, unsigned long,
                           const char *, unsigned, const char *,
                           void *, va_list);
extern void *__cdecl __acrt_iob_func(unsigned);
IMP _configure_narrow_argv(int);
extern char **__cdecl _get_initial_narrow_environment(void);
IMP _crt_atexit(void (__cdecl *)(void));
IMP _initialize_onexit_table(OnExit *);
IMP _register_onexit_function(OnExit *, void (__cdecl *)(void));
IMP _execute_onexit_table(OnExit *);
IMP _controlfp_s(unsigned *, unsigned, unsigned);
IMP _set_fmode(int);
IMP _get_fmode(int *);
extern int *__cdecl __p___argc(void);
extern char ***__cdecl __p___argv(void);
extern char **__cdecl __p__acmdln(void);
extern void __cdecl exit(int);
extern void __cdecl free(void *);

int fltused_;
char _8087 = 3;

static int failed;
static int order;
static OnExit table;

#define CHECK(c) do { if (!(c)) { failed++; \
    say_line(__LINE__); } } while (0)

static void say_line(int n)
{
    char *p = "line 0000\r\n";

    p[8] = (char)('0' + n % 10);
    p[7] = (char)('0' + n / 10 % 10);
    p[6] = (char)('0' + n / 100 % 10);
    p[5] = (char)('0' + n / 1000);
    __stdio_common_vfprintf(0, 0, __acrt_iob_func(1), p, 0,
                            (va_list)0);
}

static int sfmt(char *b, unsigned n, unsigned long opt,
                const char *f, ...)
{
    va_list ap;
    int r;

    va_start(ap, f);
    r = __stdio_common_vsprintf(opt, 0, b, n, f, 0, ap);
    va_end(ap);
    return r;
}

static int pfmt(const char *f, ...)
{
    va_list ap;
    int r;

    va_start(ap, f);
    r = __stdio_common_vfprintf(0, 0, __acrt_iob_func(1), f, 0,
                                ap);
    va_end(ap);
    return r;
}

static int ssc(const char *s, const char *f, ...)
{
    va_list ap;
    int r;

    va_start(ap, f);
    r = __stdio_common_vsscanf(0, 0, s, 0xFFFFFFFFUL, f, 0, ap);
    va_end(ap);
    return r;
}

static int sfmt_s(char *b, unsigned n, const char *f, ...)
{
    va_list ap;
    int r;

    va_start(ap, f);
    r = __stdio_common_vsprintf_s(0, 0, b, n, f, 0, ap);
    va_end(ap);
    return r;
}

static void h1(void)
{
    order = order * 10 + 1;
}

static void h2(void)
{
    order = order * 10 + 2;
}

static void print_tests(void)
{
    char b[32];
    int a, c;

    CHECK(sfmt(b, 0xFFFFFFFFUL, 0, "%d-%s", 42, "xy") == 5);
    CHECK(b[0] == '4' && b[4] == 'y' && !b[5]);
    CHECK(sfmt(b, 4, 2, "%s", "abcdefgh") == 8);
    CHECK(b[3] == 0 && b[2] == 'c');
    CHECK(sfmt(b, 4, 0, "%s", "abcdefgh") == -1);
    CHECK(sfmt_s(b, 4, "%s", "abcdefgh") == -1 && !b[0]);
    CHECK(sfmt_s(b, 8, "%s", "abc") == 3 && b[2] == 'c');
    CHECK(pfmt("UCRT-out %d\r\n", 7) == 12);
    CHECK(ssc("12 ab", "%d %s", &a, b) == 2 && a == 12 &&
          b[1] == 'b');
    c = 0;
    CHECK(ssc("x", "%d", &c) == 0);
}

static void a_fmode_check(void);

static void start_tests(void)
{
    char *cl;

    CHECK(_configure_narrow_argv(1) == 0);
    CHECK(*__p___argc() >= 1);
    CHECK((*__p___argv())[0] != 0);
    CHECK(_get_initial_narrow_environment() != 0);
    cl = *__p__acmdln();
    CHECK(cl != 0);
    CHECK(_set_fmode(0x8000) == 0);
    a_fmode_check();
}

static void a_fmode_check(void)
{
    int m = 0;

    CHECK(_get_fmode(&m) == 0 && m == 0x8000);
    CHECK(_controlfp_s(0, 0, 0) == 0);
}

static void exit_tests(void)
{
    CHECK(_initialize_onexit_table(&table) == 0);
    CHECK(_register_onexit_function(&table, h1) == 0);
    CHECK(_register_onexit_function(&table, h2) == 0);
    CHECK(_execute_onexit_table(&table) == 0);
    CHECK(order == 21 && table.first == 0);
}

void mainCRTStartup(void)
{
    print_tests();
    start_tests();
    exit_tests();
    if (failed) {
        pfmt("UCRT: fail %d\r\n", failed);
        exit(1);
    }
    pfmt("UCRT: ok\r\n");
    exit(0);
}
