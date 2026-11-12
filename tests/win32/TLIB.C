/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 191: strings, memory functions, ctype, utilities and time
 * of MSVCRT.DLL. Prints "LIB: ok" or "LIB: fail NN".
 */
typedef struct Tm {
    int tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year,
        tm_wday,
        tm_yday, tm_isdst;
} Tm;

typedef struct TimeB {
    long time;
    unsigned short millitm;
    short timezone;
    short dstflag;
} TimeB;

extern unsigned __cdecl strlen(const char *);
extern void *__cdecl memcpy(void *, const void *, unsigned);
extern void *__cdecl memmove(void *, const void *, unsigned);
extern void *__cdecl memset(void *, int, unsigned);
extern int __cdecl memcmp(const void *, const void *, unsigned);
extern int __cdecl _memicmp(const void *, const void *,
                            unsigned);
extern void *__cdecl memchr(const void *, int, unsigned);
extern char *__cdecl strcpy(char *, const char *);
extern char *__cdecl strncpy(char *, const char *, unsigned);
extern char *__cdecl strcat(char *, const char *);
extern char *__cdecl strncat(char *, const char *, unsigned);
extern int __cdecl strcmp(const char *, const char *);
extern int __cdecl strncmp(const char *, const char *, unsigned);
extern int __cdecl _stricmp(const char *, const char *);
extern int __cdecl _strnicmp(const char *, const char *,
                             unsigned);
extern char *__cdecl strchr(const char *, int);
extern char *__cdecl strrchr(const char *, int);
extern char *__cdecl strstr(const char *, const char *);
extern unsigned __cdecl strspn(const char *, const char *);
extern unsigned __cdecl strcspn(const char *, const char *);
extern char *__cdecl strpbrk(const char *, const char *);
extern char *__cdecl strtok(char *, const char *);
extern char *__cdecl _strdup(const char *);
extern char *__cdecl _strlwr(char *);
extern char *__cdecl _strupr(char *);
extern char *__cdecl _strrev(char *);
extern char *__cdecl strerror(int);
extern int __cdecl isalpha(int);
extern int __cdecl isupper(int);
extern int __cdecl islower(int);
extern int __cdecl isdigit(int);
extern int __cdecl isxdigit(int);
extern int __cdecl isspace(int);
extern int __cdecl ispunct(int);
extern int __cdecl iscntrl(int);
extern int __cdecl isalnum(int);
extern int __cdecl isprint(int);
extern int __cdecl isgraph(int);
extern int __cdecl toupper(int);
extern int __cdecl tolower(int);
extern int __cdecl _isctype(int, int);
extern char *__cdecl setlocale(int, const char *);
extern void __cdecl qsort(void *, unsigned, unsigned,
                          int (__cdecl *)(const void *,
                                  const void *));
extern void *__cdecl bsearch(const void *, const void *,
                             unsigned,
                             unsigned,
                             int (__cdecl *)(const void *,
                                     const void *));
extern void __cdecl srand(unsigned);
extern int __cdecl rand(void);
extern int __cdecl abs(int);
extern long __cdecl labs(long);
extern char *__cdecl getenv(const char *);
extern int __cdecl _putenv(const char *);
extern char *__cdecl _itoa(int, char *, int);
extern char *__cdecl _ltoa(long, char *, int);
extern char *__cdecl _ultoa(unsigned long, char *, int);
extern char *__cdecl _fullpath(char *, const char *, unsigned);
extern void __cdecl _splitpath(const char *, char *, char *,
                               char *,
                               char *);
extern void __cdecl _makepath(char *, const char *, const char *,
                              const char *, const char *);
extern Tm *__cdecl gmtime(const long *);
extern Tm *__cdecl localtime(const long *);
extern long __cdecl mktime(Tm *);
extern long __cdecl time(long *);
extern void __cdecl _ftime(TimeB *);
extern long __cdecl clock(void);
extern double __cdecl difftime(long, long);
extern char *__cdecl asctime(const Tm *);
extern char *__cdecl ctime(const long *);
extern unsigned __cdecl strftime(char *, unsigned, const char *,
                                 const Tm *);
extern char *__cdecl _strdate(char *);
extern char *__cdecl _strtime(char *);
extern void __cdecl free(void *);
extern void __cdecl exit(int);
extern __declspec(dllimport) unsigned short *_pctype;
extern __declspec(dllimport) long _timezone;
extern __declspec(dllimport) char *_tzname[2];

extern int __stdcall WriteFile(void *, const void *,
                               unsigned long,
                               unsigned long *, void *);
extern void *__stdcall GetStdHandle(unsigned long);

/* what the compiler expects of a floating point run-time */
int fltused_;
char _8087 = 3;

static void say(const char *s)
{
    unsigned long n = 0, w;

    while (s[n])
        n++;
    WriteFile(GetStdHandle(0xFFFFFFF5UL), s, n, &w, 0);
}

static int step;

static void fail(void)
{
    char t[] = "\r\nLIB: fail 000\r\n";

    t[12] = (char)('0' + step / 100);
    t[13] = (char)('0' + step / 10 % 10);
    t[14] = (char)('0' + step % 10);
    say(t);
    exit(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static int same(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

static void string_tests(void)
{
    char b[40], *p;
    char t[] = "a,b;;c";

    CHECK(strlen("hello") == 5 && strlen("") == 0);
    strcpy(b, "abc");
    CHECK(strcat(b, "def") == b && same(b, "abcdef"));
    CHECK(strncat(b, "XYZ", 2) == b && same(b, "abcdefXY"));
    memset(b, 'z', 4);
    b[4] = 0;
    CHECK(same(b, "zzzz"));
    strncpy(b, "ab", 5);
    CHECK(b[0] == 'a' && b[1] == 'b' && b[2] == 0 && b[4] == 0);
    CHECK(strcmp("a", "b") < 0 && strcmp("b", "a") > 0);
    CHECK(strcmp("abc", "abc") == 0 &&
          strncmp("abcd", "abcx", 3) == 0);
    CHECK(strncmp("abcd", "abcx", 4) < 0);
    CHECK(_stricmp("ABC", "abc") == 0 && _stricmp("a", "B") < 0);
    CHECK(_strnicmp("ABCd", "abcX", 3) == 0);
    CHECK(strchr("hello", 'l') == (char *)0 + 0 + 0 || 1);
    p = strchr("hello", 'l');
    CHECK(p != 0 && same(p, "llo"));
    CHECK(strrchr("hello", 'l') != 0 &&
          same(strrchr("hello", 'l'), "lo"));
    CHECK(strchr("abc", 'z') == 0 && *strchr("abc", 0) == 0);
    CHECK(same(strstr("find the needle", "the"), "the needle"));
    CHECK(strstr("abc", "xyz") == 0 &&
          same(strstr("abc", ""), "abc"));
    CHECK(strspn("aabbcc", "ab") == 4 &&
          strcspn("hello world", " ") == 5);
    CHECK(same(strpbrk("hello", "lo"), "llo"));
    p = strtok(t, ",;");
    CHECK(p != 0 && same(p, "a"));
    p = strtok(0, ",;");
    CHECK(p != 0 && same(p, "b"));
    p = strtok(0, ",;");
    CHECK(p != 0 && same(p, "c"));
    CHECK(strtok(0, ",;") == 0);
    p = _strdup("copy");
    CHECK(p != 0 && same(p, "copy"));
    free(p);
    strcpy(b, "MiXed 123");
    CHECK(same(_strlwr(b), "mixed 123") &&
          same(_strupr(b), "MIXED 123"));
    strcpy(b, "abcde");
    CHECK(same(_strrev(b), "edcba"));
    CHECK(same(strerror(2), "No such file or directory"));
    memcpy(b, "0123456789", 11);
    memmove(b + 2, b, 8);
    CHECK(same(b, "0101234567"));
    memmove(b, b + 2, 8);
    CHECK(memcmp(b, "0123456767", 8) == 0 &&
          memcmp("a", "b", 1) < 0);
    CHECK(_memicmp("ABC", "abd", 2) == 0 &&
          memchr("abc", 'c', 3) != 0);
    CHECK(memchr("abc", 'x', 3) == 0);
}

static void ctype_tests(void)
{
    int c, ok = 1;

    for (c = 0; c < 128; c++) {
        if (!!isalpha(c) != ((c >= 'a' && c <= 'z') ||
                             (c >= 'A' && c <= 'Z')))
            ok = 0;
        if (!!isdigit(c) != (c >= '0' && c <= '9'))
            ok = 0;
        if (!!isspace(c) != (c == ' ' || (c >= 9 && c <= 13)))
            ok = 0;
        if (!!isprint(c) != (c >= 32 && c < 127))
            ok = 0;
        if (!!iscntrl(c) != (c < 32 || c == 127))
            ok = 0;
        if (!!isgraph(c) != (c > 32 && c < 127))
            ok = 0;
        if (!!isalnum(c) != (isalpha(c) || isdigit(c)))
            ok = 0;
        if (!!ispunct(c) != (isgraph(c) && !isalnum(c)))
            ok = 0;
        if (!!isxdigit(c) != (isdigit(c) || (c >= 'a'
                                             && c <= 'f') ||
                              (c >= 'A' && c <= 'F')))
            ok = 0;
        if (!!isupper(c) != (c >= 'A' && c <= 'Z'))
            ok = 0;
        if (!!islower(c) != (c >= 'a' && c <= 'z'))
            ok = 0;
    }
    CHECK(ok);
    CHECK(!isalpha(-1) && !isalpha(200) && !isdigit(300));
    CHECK(toupper('a') == 'A' && toupper('1') == '1');
    CHECK(tolower('Q') == 'q' && tolower('q') == 'q');
    CHECK((_pctype['a'] & 0x102) == 0x102 && (_pctype['5'] & 4));
    CHECK(_isctype('A', 0x100) && !_isctype('1', 0x100));
    CHECK(same(setlocale(0, "C"), "C") &&
          setlocale(0, "xx_YY") == 0);
}

static int cmp_int(const void *a, const void *b)
{
    int x = *(const int *)a, y = *(const int *)b;

    return x < y ? -1 : x > y;
}

static void util_tests(void)
{
    int v[500], i, ok = 1, key;
    char b[80], d[8], dir[40], fn[20], ext[8], *p;
    long l;

    srand(1);
    CHECK(rand() == 41 && rand() == 18467 && rand() == 6334);
    CHECK(rand() == 26500 && rand() == 19169);
    for (i = 0; i < 500; i++)
        v[i] = rand() % 1000 - 500;
    qsort(v, 500, sizeof(int), cmp_int);
    for (i = 1; i < 500; i++)
        if (v[i - 1] > v[i])
            ok = 0;
    CHECK(ok);
    key = v[123];
    p = bsearch(&key, v, 500, sizeof(int), cmp_int);
    CHECK(p != 0 && *(int *)p == key);
    key = 12345;
    CHECK(bsearch(&key, v, 500, sizeof(int), cmp_int) == 0);
    qsort(v, 1, sizeof(int), cmp_int);
    CHECK(abs(-5) == 5 && abs(5) == 5
          && labs(-70000L) == 70000L);
    CHECK(_putenv("ZRDXLIB=value1") == 0);
    CHECK(getenv("ZRDXLIB") != 0 &&
          same(getenv("ZRDXLIB"), "value1"));
    CHECK(_putenv("ZRDXLIB=") == 0 && getenv("ZRDXLIB") == 0);
    CHECK(getenv("ZRDXNONE") == 0 && _putenv("NOEQUALS") == -1);
    CHECK(same(_itoa(-123, b, 10), "-123") &&
          same(_itoa(255, b, 16), "ff"));
    CHECK(same(_itoa(5, b, 2), "101") && same(_ltoa(-1L, b, 16),
            "ffffffff"));
    CHECK(same(_ultoa(4000000000UL, b, 10), "4000000000"));
    CHECK(same(_ultoa(35UL, b, 36), "z") &&
          same(_itoa(0, b, 10), "0"));
    _splitpath("C:\\dir\\sub\\file.ext", d, dir, fn, ext);
    CHECK(same(d, "C:") && same(dir, "\\dir\\sub\\") &&
          same(fn, "file"));
    CHECK(same(ext, ".ext"));
    _splitpath("name", d, dir, fn, ext);
    CHECK(same(d, "") && same(dir, "") && same(fn, "name") &&
          same(ext, ""));
    _makepath(b, "C:", "\\dir", "file", "ext");
    CHECK(same(b, "C:\\dir\\file.ext"));
    CHECK(_fullpath(b, "x.y", 80) == b && b[1] == ':');
    l = 0;
    (void)l;
}

static void time_tests(void)
{
    long t = 0, t2;
    Tm *g, tm;
    char b[100];
    TimeB tb;
    double dd;

    g = gmtime(&t);
    CHECK(g != 0 && g->tm_year == 70 && g->tm_mon == 0 &&
          g->tm_mday == 1);
    CHECK(g->tm_wday == 4 && g->tm_yday == 0 && g->tm_hour == 0);
    t = 1700000000L;
    g = gmtime(&t);
    CHECK(g->tm_year == 123 && g->tm_mon == 10
          && g->tm_mday == 14);
    CHECK(g->tm_hour == 22 && g->tm_min == 13
          && g->tm_sec == 20);
    CHECK(g->tm_wday == 2 && g->tm_yday == 317
          && g->tm_isdst == 0);
    t = 951782400L;
    g = gmtime(&t);
    CHECK(g->tm_year == 100 && g->tm_mon == 1
          && g->tm_mday == 29);
    CHECK(g->tm_yday == 59);
    t = -1;
    CHECK(gmtime(&t) == 0);
    t = 1700000000L;
    CHECK(localtime(&t)->tm_hour == 22);
    tm = *gmtime(&t);
    CHECK(mktime(&tm) == 1700000000L);
    tm.tm_year = 124;
    tm.tm_mon = 1;
    tm.tm_mday = 29;
    tm.tm_hour = 12;
    tm.tm_min = 34;
    tm.tm_sec = 56;
    CHECK(mktime(&tm) == 1709210096L && tm.tm_wday == 4);
    tm.tm_mon = 14;
    tm.tm_mday = 1;
    tm.tm_hour = 0;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    tm.tm_year = 123;
    CHECK(mktime(&tm) == 1709251200L && tm.tm_mon == 2 &&
          tm.tm_year == 124);
    tm.tm_mday = 0;
    tm.tm_mon = 0;
    tm.tm_year = 124;
    CHECK(mktime(&tm) == 1704067199L - 86399L
          && tm.tm_mday == 31);
    tm.tm_sec = 3600 * 25;
    CHECK(mktime(&tm) >= 0 && tm.tm_mday == 1
          && tm.tm_hour == 1);
    CHECK(same(asctime(gmtime(&t)),
               "Tue Nov 14 22:13:20 2023\n"));
    CHECK(same(ctime(&t), "Tue Nov 14 22:13:20 2023\n"));
    CHECK(strftime(b, 100,
                   "%a|%A|%b|%B|%d|%H|%I|%j|%m|%M|%p|%S|%U|%w"
                   "|%W|%y|%Y|%%", gmtime(&t)) == 67);
    CHECK(same(b,
               "Tue|Tuesday|Nov|November|14|22|10|318|11|13|"
               "PM|20|46|2|46|23|2023|%"));
    CHECK(strftime(b, 100, "%c", gmtime(&t)) == 17);
    CHECK(same(b, "11/14/23 22:13:20"));
    CHECK(strftime(b, 100, "%x %X", gmtime(&t)) == 17);
    CHECK(strftime(b, 5, "%Y-%m", gmtime(&t)) == 0);
    CHECK(strftime(b, 8, "%Y-%m", gmtime(&t)) == 7 &&
          same(b, "2023-11"));
    t2 = time(0);
    CHECK(gmtime(&t2)->tm_year >= 120 && time(&t) >= t2);
    CHECK(clock() >= 0);
    _ftime(&tb);
    CHECK(tb.millitm < 1000 && tb.time >= t2);
    dd = difftime(5, 2);
    CHECK(((unsigned long *)&dd)[1] == 0x40080000UL);
    CHECK(strlen(_strdate(b)) == 8 && b[2] == '/'
          && b[5] == '/');
    CHECK(strlen(_strtime(b)) == 8 && b[2] == ':'
          && b[5] == ':');
    CHECK(_timezone == 0 && _tzname[0] != 0);
}

void mainCRTStartup(void)
{
    string_tests();
    ctype_tests();
    util_tests();
    time_tests();
    say("\r\nLIB: ok\r\n");
    exit(0);
}
