/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 189: a program that uses MSVCRT.DLL only for the start-up
 * contract. Run as TCRT.EXE a "b c" d\"e: checks argc/argv, the
 * environment, _initterm, _controlfp, then exits with code 7 and
 * the atexit handlers print C, B, A. Prints "CRT: ok" first.
 */
extern int __cdecl __getmainargs(int *, char ***, char ***, int,
                                 void *);
extern int __cdecl atexit(void (__cdecl *)(void));
extern void __cdecl exit(int);
extern void __cdecl _initterm(void (__cdecl **)(void),
                              void (__cdecl **)(void));
extern unsigned __cdecl _controlfp(unsigned, unsigned);
extern void __cdecl _fpreset(void);
extern int *__cdecl __p__fmode(void);
extern char ***__cdecl __p___initenv(void);
extern __declspec(dllimport) char *_acmdln;
extern int __cdecl __zrdx_split(const char *, char **, char *);

extern int __stdcall WriteFile(void *, const void *,
                               unsigned long,
                               unsigned long *, void *);
extern void *__stdcall GetStdHandle(unsigned long);

static void say(const char *s)
{
    unsigned long n = 0, w;

    while (s[n])
        n++;
    WriteFile(GetStdHandle(0xFFFFFFF5UL), s, n, &w, 0);
}

static int step;
static char order[8];

static void fail(void)
{
    char t[] = "\r\nCRT: fail 00\r\n";

    t[13] = (char)('0' + step / 10);
    t[14] = (char)('0' + step % 10);
    say(t);
    exit(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static void h_a(void)
{
    say("A\r\n");
}

static void h_b(void)
{
    say("B\r\n");
}

static void h_c(void)
{
    say("C\r\n");
}

static void t1(void)
{
    order[0] = (char)(order[0] + 1);
    order[1] = (char)(order[1] * 2 + 1);
}

static void t2(void)
{
    order[2] = order[1];
}

static int same(const char *a, const char *b)
{
    while (*a && *a == *b)
    {
        a++;
        b++;
    }
    return *a == *b;
}

/* the quoting rules, on strings the DOS shell would not pass */
static void split_tests(void)
{
    char *av[8], buf[80];

    CHECK(__zrdx_split("p a  b", av, buf) == 3);
    CHECK(same(av[0], "p") && same(av[2], "b") && av[3] == 0);
    CHECK(__zrdx_split("\"C:\\x y\\p.exe\" q", av, buf) == 2);
    CHECK(same(av[0], "C:\\x y\\p.exe") && same(av[1], "q"));
    CHECK(__zrdx_split("p \"a b\" c", av, buf) == 3);
    CHECK(same(av[1], "a b") && same(av[2], "c"));
    CHECK(__zrdx_split("p a\\\"b", av, buf) == 2);
    CHECK(same(av[1], "a\"b"));
    CHECK(__zrdx_split("p a\\\\\"b c\" d", av, buf) == 3);
    CHECK(same(av[1], "a\\b c") && same(av[2], "d"));
    CHECK(__zrdx_split("p a\\\\\\\"b", av, buf) == 2);
    CHECK(same(av[1], "a\\\"b"));
    CHECK(__zrdx_split("p \\\\x \\y", av, buf) == 3);
    CHECK(same(av[1], "\\\\x") && same(av[2], "\\y"));
    CHECK(__zrdx_split("", av, buf) == 0 && av[0] == 0);
    CHECK(__zrdx_split("   ", av, buf) == 0);
}

void mainCRTStartup(void)
{
    int argc;
    char **argv, **env;
    void (__cdecl * tab[4])(void);
    unsigned cw;

    CHECK(__getmainargs(&argc, &argv, &env, 0, 0) == 0);
    CHECK(argc == 4);
    CHECK(argv[0][0] != 0 && argv[4] == 0);
    CHECK(same(argv[1], "a") && same(argv[2], "b c"));
    CHECK(same(argv[3], "d\\\\e"));
    split_tests();
    CHECK(env != 0 && env[0] != 0 && *__p___initenv() == env);
    CHECK(_acmdln != 0 && _acmdln[0] != 0);
    tab[0] = t1;
    tab[1] = 0;
    tab[2] = t2;
    tab[3] = t1;
    _initterm(tab, tab + 4);
    CHECK(order[0] == 2 && order[1] == 3 && order[2] == 1);
    CHECK(_controlfp(0, 0) == 0x9001FUL);
    CHECK(_controlfp(0x100, 0x300) == 0x9011FUL);
    CHECK((_controlfp(0, 0) & 0x300) == 0x100);
    _fpreset();
    CHECK(_controlfp(0, 0) == 0x9001FUL);
    CHECK(_controlfp(0x20000UL, 0x30000UL) == 0xA001FUL);
    CHECK(_controlfp(0x10000UL, 0x30000UL) == 0x9001FUL);
    CHECK(*__p__fmode() == 0);
    atexit(h_a);
    atexit(h_b);
    atexit(h_c);
    say("\r\nCRT: ok\r\n");
    exit(7);
}
