/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 193: wide strings, setjmp/longjmp and signal/raise of
 * MSVCRT.DLL. Prints "WCS: ok" or "WCS: fail NN".
 */
typedef unsigned short W;
typedef void (__cdecl * SigFn)(int);

extern int __cdecl _setjmp3(int *, int);
extern void __cdecl longjmp(int *, int);
extern SigFn __cdecl signal(int, SigFn);
extern int __cdecl raise(int);
extern int __cdecl towupper(int);
extern int __cdecl towlower(int);
extern int __cdecl iswalpha(int);
extern int __cdecl iswupper(int);
extern int __cdecl iswlower(int);
extern int __cdecl iswdigit(int);
extern int __cdecl iswspace(int);
extern int __cdecl iswpunct(int);
extern unsigned __cdecl wcslen(const W *);
extern W *__cdecl wcscpy(W *, const W *);
extern W *__cdecl wcsncpy(W *, const W *, unsigned);
extern W *__cdecl wcscat(W *, const W *);
extern W *__cdecl wcsncat(W *, const W *, unsigned);
extern int __cdecl wcscmp(const W *, const W *);
extern int __cdecl wcsncmp(const W *, const W *, unsigned);
extern int __cdecl _wcsicmp(const W *, const W *);
extern W *__cdecl wcschr(const W *, W);
extern W *__cdecl wcsrchr(const W *, W);
extern W *__cdecl wcsstr(const W *, const W *);
extern unsigned __cdecl wcsspn(const W *, const W *);
extern unsigned __cdecl wcscspn(const W *, const W *);
extern W *__cdecl wcspbrk(const W *, const W *);
extern W *__cdecl wcstok(W *, const W *);
extern W *__cdecl _wcsdup(const W *);
extern W *__cdecl _wcslwr(W *);
extern W *__cdecl _wcsupr(W *);
extern W *__cdecl _wcsrev(W *);
extern double __cdecl wcstod(const W *, W **);
extern long __cdecl wcstol(const W *, W **, int);
extern unsigned long __cdecl wcstoul(const W *, W **, int);
extern int __cdecl _wtoi(const W *);
extern W *__cdecl _itow(int, W *, int);
extern unsigned __cdecl mbstowcs(W *, const char *, unsigned);
extern unsigned __cdecl wcstombs(char *, const W *, unsigned);
extern int __cdecl mbtowc(W *, const char *, unsigned);
extern int __cdecl wctomb(char *, W);
extern int __cdecl swprintf(W *, const W *, ...);
extern int __cdecl _snwprintf(W *, unsigned, const W *, ...);
extern int __cdecl _scwprintf(const W *, ...);
extern int __cdecl wprintf(const W *, ...);
extern int __cdecl fputws(const W *, void *);
extern int __cdecl swscanf(const W *, const W *, ...);
extern W *__cdecl fgetws(W *, int, void *);
extern void *__cdecl _wfopen(const W *, const W *);
extern int __cdecl fclose(void *);
extern int __cdecl __wgetmainargs(int *, W ***, W ***, int,
                                  void *);
extern void __cdecl free(void *);
extern void __cdecl exit(int);
extern int __cdecl printf(const char *, ...);

int fltused_;
char _8087 = 3;

static int failed;
static volatile int hits;

#define CHECK(c) do { if (!(c)) { failed++; \
    printf("line %d\n", __LINE__); } } while (0)

static void __cdecl on_sig(int s)
{
    hits += s;
}

static void deep(int *jb, int v)
{
    longjmp(jb, v);
}

static void jump_tests(void)
{
    int jb[16];
    volatile int n = 0;
    int r;

    r = _setjmp3(jb, 0);
    n++;
    if (r == 0)
        deep(jb, 7);
    CHECK(r == 7 && n == 2);
    r = _setjmp3(jb, 0);
    if (r == 0)
        longjmp(jb, 0);
    CHECK(r == 1);
}

static void signal_tests(void)
{
    CHECK(signal(2, on_sig) == 0);
    CHECK(raise(2) == 0 && hits == 2);
    CHECK(signal(2, (SigFn)1) == 0);
    CHECK(raise(2) == 0);
    CHECK(signal(77, on_sig) == (SigFn)-1);
    CHECK(raise(77) == -1);
}

static void class_tests(void)
{
    CHECK(towupper('a') == 'A' && towlower('Z') == 'z');
    CHECK(towupper(0xE9) == 0xC9 && towlower(0xC9) == 0xE9);
    CHECK(towupper(0xF7) == 0xF7 && towupper(0xFF) == 0x178);
    CHECK(towupper(0x3B1) == 0x391 && towlower(0x3A9) == 0x3C9);
    CHECK(towupper(0x430) == 0x410 && towlower(0x42F) == 0x44F);
    CHECK(towupper(0x451) == 0x401 && towlower(0x400) == 0x450);
    CHECK(iswalpha('x') && iswalpha(0x416) && !iswalpha('1'));
    CHECK(iswupper('X') && !iswupper('x') && iswlower('x'));
    CHECK(iswdigit('7') && iswspace(' ') && iswpunct('!'));
}

static void string_tests(void)
{
    W a[40], b[40], *p;
    static const W hello[] = {'h', 'e', 'l', 'l', 'o', 0};
    static const W ell[] = {'e', 'l', 'l', 0};
    static const W sep[] = {',', ' ', 0};
    static const W csv[] = {'a', ',', 'b', ' ', 'c', 0};
    static const W up[] = {'H', 'E', 'L', 'L', 'O', 0};
    char nb[16];

    CHECK(wcslen(hello) == 5);
    wcscpy(a, hello);
    CHECK(wcscmp(a, hello) == 0 && wcscmp(a, ell) > 0);
    wcscat(a, ell);
    CHECK(wcslen(a) == 8 && wcsncmp(a, hello, 5) == 0);
    wcsncpy(b, hello, 8);
    CHECK(b[5] == 0 && b[7] == 0);
    wcsncat(b, ell, 2);
    CHECK(wcslen(b) == 7 && b[6] == 'l');
    CHECK(_wcsicmp(hello, up) == 0 && wcscmp(hello, up) != 0);
    CHECK(wcschr(hello, 'l') == hello + 2);
    CHECK(wcsrchr(hello, 'l') == hello + 3);
    CHECK(wcschr(hello, 'z') == 0);
    CHECK(wcsstr(hello, ell) == hello + 1);
    CHECK(wcsspn(hello, ell) == 0 && wcscspn(hello, ell) == 1);
    CHECK(wcspbrk(hello, ell) == hello + 1);
    wcscpy(a, csv);
    p = wcstok(a, sep);
    CHECK(p && p[0] == 'a' && p[1] == 0);
    p = wcstok(0, sep);
    CHECK(p && p[0] == 'b');
    p = wcstok(0, sep);
    CHECK(p && p[0] == 'c');
    CHECK(wcstok(0, sep) == 0);
    p = _wcsdup(hello);
    CHECK(p && wcscmp(p, hello) == 0);
    _wcsupr(p);
    CHECK(wcscmp(p, up) == 0);
    _wcslwr(p);
    CHECK(wcscmp(p, hello) == 0);
    _wcsrev(p);
    CHECK(p[0] == 'o' && p[4] == 'h');
    free(p);
    CHECK(mbstowcs(a, "abc", 10) == 3 && a[2] == 'c' && !a[3]);
    CHECK(mbstowcs(0, "abcd", 0) == 4);
    CHECK(wcstombs(nb, hello, 16) == 5 && nb[4] == 'o');
    CHECK(mbtowc(a, "Z", 1) == 1 && a[0] == 'Z');
    CHECK(wctomb(nb, 'Q') == 1 && nb[0] == 'Q');
}

static void number_tests(void)
{
    static const W n1[] = {' ', '-', '4', '2', 'x', 0};
    static const W n2[] = {'1', '.', '5', 'e', '2', 0};
    static const W n3[] = {'f', 'f', 0};
    W buf[40], *e;

    CHECK(wcstol(n1, &e, 10) == -42 && e == n1 + 4);
    CHECK(wcstod(n2, &e) == 150.0 && e == n2 + 5);
    CHECK(wcstoul(n3, &e, 16) == 255 && e == n3 + 2);
    CHECK(_wtoi(n1) == -42);
    _itow(-1234, buf, 10);
    CHECK(buf[0] == '-' && buf[4] == '4' && !buf[5]);
    _itow(255, buf, 16);
    CHECK(buf[0] == 'f' && buf[1] == 'f' && !buf[2]);
}

static void format_tests(void)
{
    W b[40];

    CHECK(swprintf(b, L"%d|%s|%S|%c|%5.1f", 5, L"ab", "cd",
                   (int)'x', 2.5) == 15);
    CHECK(b[0] == '5' && b[2] == 'a' && b[5] == 'c' && !b[15]);
    CHECK(b[8] == 'x' && b[12] == '2' && b[14] == '5');
    CHECK(_snwprintf(b, 4, L"%s", L"abcdef") == -1);
    CHECK(_snwprintf(b, 6, L"%hs", "abcdef") == 6 &&
          b[5] == 'f');
    CHECK(_scwprintf(L"[%4d]", 12) == 6);
    CHECK(wprintf(L"wide-out %hs %s\r\n", "n", L"w") == 14);
}

static void scan_tests(void)
{
    W w[20], c, **av, **ev;
    char n[20];
    int a, argc;
    void *f;

    CHECK(swscanf(L"12 abc x", L"%d %s %c", &a, w, &c) == 3);
    CHECK(a == 12 && w[0] == 'a' && w[2] == 'c' && !w[3]);
    CHECK(c == 'x');
    CHECK(swscanf(L"q9", L"%[a-z]%d", w, &a) == 2 &&
          w[0] == 'q');
    CHECK(swscanf(L"zz", L"%hs", n) == 1 && n[1] == 'z');
    f = _wfopen(L"TWCS.TMP", L"w");
    CHECK(f != 0);
    fputws(L"one\ntwo\n", f);
    fclose(f);
    f = _wfopen(L"TWCS.TMP", L"r");
    CHECK(f && fgetws(w, 20, f) == w && w[0] == 'o' &&
          w[3] == 10);
    CHECK(fgetws(w, 20, f) == w && w[0] == 't');
    CHECK(fgetws(w, 20, f) == 0);
    fclose(f);
    CHECK(__wgetmainargs(&argc, &av, &ev, 0, 0) == 0);
    CHECK(argc >= 1 && av[0] && av[0][0] && !av[argc] && ev[0]);
}

void mainCRTStartup(void)
{
    jump_tests();
    signal_tests();
    class_tests();
    string_tests();
    number_tests();
    format_tests();
    scan_tests();
    if (failed) {
        printf("\r\nWCS: fail %d\r\n", failed);
        exit(1);
    }
    printf("\r\nWCS: ok\r\n");
    exit(0);
}
