"""Writes TSCAN.C: strtod cases whose answer is the correctly rounded
double of the host (including exact midpoints and their neighbours),
plus fixed strtol and sscanf cases.
Usage: python3 GENSCAN.PY > TSCAN.C
"""
import decimal
import struct
import sys

WIDTH = 65


def bits(x):
    return struct.unpack("<II", struct.pack("<d", x))


def lit(s, indent):
    esc = s.replace("\\", "\\\\").replace('"', '\\"')
    room = max(WIDTH - indent - 4, 20)
    parts = [esc[i:i + room] for i in range(0, len(esc), room)] or [""]
    return ('"\n' + " " * indent + '"').join(parts).join(['"', '"'])


FIXED = [
    "0", "-0", "1", "0.1", "123.456", "1e23", "1e22", "9007199254740993",
    "9007199254740992.5", "9007199254740991.5", "2.2250738585072011e-308",
    "2.2250738585072014e-308", "4.9406564584124654e-324", "2e-324",
    "3e-324", "2.4703282292062327e-324", "2.4703282292062328e-324",
    "1.7976931348623157e308", "1.7976931348623158e308",
    "1.7976931348623159e308", "1e309", "1e-400", ".5", "5.", "1e+5",
    "1E5", "  12", "+7.5", "3.141592653589793238462643383279",
    "0." + "0" * 300 + "1", "123456789012345678901234567890e-10",
    "1" + "0" * 400, "0.000000000000000000000000000000000001e36",
    "8.5e-5", "1.00000000000000011102230246251565404236316680908203125",
    "1.00000000000000011102230246251565404236316680908203126",
    "1.00000000000000011102230246251565404236316680908203124",
    "-1.5e300", "123456789e-300", "5e-324", "1e-323", "1.5e-323",
]


def midpoints():
    decimal.getcontext().prec = 1200
    out = []
    for x in (1.0, 1.5, 0.1, 3.14159, 1e22, 2.0 ** 52 + 1.0, 5e-324,
              1e-310, 2.2250738585072014e-308, 123456.789):
        n = struct.unpack("<Q", struct.pack("<d", x))[0] + 1
        y = struct.unpack("<d", struct.pack("<Q", n))[0]
        mid = (decimal.Decimal(x) + decimal.Decimal(y)) / 2
        s = format(mid, "f") if abs(mid) > 1e-30 else format(mid, "e")
        out.append(s)
        out.append(s + "1")
        out.append(s[:-1] + str((int(s[-1]) + 9) % 10) if s[-1].isdigit()
                   and s[-1] != "0" else s)
    return out


TAIL = r'''/* what the compiler expects of a floating point run-time */
int fltused_;
char _8087 = 3;

static void say(const char *s)
{
    unsigned long n = 0, w;

    while (s[n])
        n++;
    WriteFile(GetStdHandle(0xFFFFFFF5UL), s, n, &w, 0);
}

static int bad;
static int step;

static void num(int n)
{
    char t[8];

    t[0] = (char)('0' + (n / 1000) % 10);
    t[1] = (char)('0' + (n / 100) % 10);
    t[2] = (char)('0' + (n / 10) % 10);
    t[3] = (char)('0' + n % 10);
    t[4] = ' ';
    t[5] = 0;
    say(t);
}

#define CHECK(c) do { step++; if (!(c)) { \
    say("\r\nSCAN: fail "); num(step); bad = 1; } } while (0)

static int same(const char *a, const char *b)
{
    while (*a && *a == *b)
    {
        a++;
        b++;
    }
    return *a == *b;
}

static void strtod_tests(void)
{
    int i;
    unsigned long w[2];
    double d;
    char *e;

    for (i = 0; cases[i].text; i++)
    {
        d = strtod(cases[i].text, &e);
        w[0] = ((unsigned long *)&d)[0];
        w[1] = ((unsigned long *)&d)[1];
        step++;
        if (w[0] != cases[i].lo || w[1] != cases[i].hi)
        {
            say("\r\nSCAN: strtod ");
            num(i);
            say(cases[i].text);
            bad = 1;
        }
    }
    d = strtod("12abc", &e);
    CHECK(same(e, "abc"));
    CHECK(((unsigned long *)&d)[1] == 0x40280000UL);
    strtod("1e", &e);
    CHECK(same(e, "e"));
    strtod("1e+", &e);
    CHECK(same(e, "e+"));
    strtod(".", &e);
    CHECK(same(e, "."));
    strtod("  -.5x", &e);
    CHECK(same(e, "x"));
    d = atof("2.5");
    CHECK(((unsigned long *)&d)[1] == 0x40040000UL);
    *_errno() = 0;
    strtod("1e999", 0);
    CHECK(*_errno() == 34);
}

static void strtol_tests(void)
{
    char *e;

    CHECK(strtol("  -123xyz", &e, 10) == -123);
    CHECK(same(e, "xyz"));
    CHECK(strtol("0x1F", &e, 16) == 31 && *e == 0);
    CHECK(strtol("0x1F", &e, 0) == 31);
    CHECK(strtol("077", &e, 0) == 63);
    CHECK(strtol("z", &e, 36) == 35);
    CHECK(strtol("12", &e, 2) == 1 && same(e, "2"));
    CHECK(strtol("abc", &e, 10) == 0 && same(e, "abc"));
    *_errno() = 0;
    CHECK(strtol("2147483648", &e, 10) == 0x7FFFFFFFL);
    CHECK(*_errno() == 34);
    *_errno() = 0;
    CHECK(strtol("-2147483649", &e, 10) == (long)0x80000000UL);
    CHECK(*_errno() == 34);
    CHECK(strtol("-2147483648", &e, 10) == (long)0x80000000UL);
    CHECK(strtoul("4294967295", &e, 10) == 0xFFFFFFFFUL);
    *_errno() = 0;
    CHECK(strtoul("4294967296", &e, 10) == 0xFFFFFFFFUL);
    CHECK(*_errno() == 34);
    CHECK(strtoul("-1", &e, 10) == 0xFFFFFFFFUL);
    CHECK(atoi("  42abc") == 42 && atol("-7") == -7);
}

static void sscanf_tests(void)
{
    int a, b, n;
    unsigned u;
    short sh;
    char s[16], c1, c2;
    float f;
    double d;
    unsigned long q[2];

    CHECK(sscanf("12 -7", "%d %d", &a, &b) == 2);
    CHECK(a == 12 && b == -7);
    CHECK(sscanf("ff", "%x", &u) == 1 && u == 255);
    CHECK(sscanf("0x1f", "%i", &a) == 1 && a == 31);
    CHECK(sscanf("017", "%i", &a) == 1 && a == 15);
    CHECK(sscanf("17", "%o", &u) == 1 && u == 15);
    CHECK(sscanf("4000000000", "%u", &u) == 1);
    CHECK(u == 4000000000UL);
    CHECK(sscanf("xy", "%c%c", &c1, &c2) == 2);
    CHECK(c1 == 'x' && c2 == 'y');
    CHECK(sscanf("abcdefgh", "%5s", s) == 1);
    CHECK(same(s, "abcde"));
    CHECK(sscanf("  word rest", "%s", s) == 1);
    CHECK(same(s, "word"));
    CHECK(sscanf("abcdx", "%[a-c]", s) == 1 && same(s, "abc"));
    CHECK(sscanf("foo,5", "%[^,],%d", s, &a) == 2);
    CHECK(same(s, "foo") && a == 5);
    CHECK(sscanf("3.5 -2.25e2", "%f %lf", &f, &d) == 2);
    CHECK(((unsigned long *)&f)[0] == 0x40600000UL);
    CHECK(((unsigned long *)&d)[1] == 0xC06C2000UL);
    CHECK(sscanf("1 2", "%*d %d", &a) == 1 && a == 2);
    CHECK(sscanf("50%", "%d%%", &a) == 1 && a == 50);
    CHECK(sscanf("ab 12", "%s %n%d", s, &n, &a) == 2 && n == 3);
    CHECK(sscanf("70000", "%hd", &sh) == 1 && sh == 4464);
    CHECK(sscanf("-9223372036854775808", "%I64d", q) == 1);
    CHECK(q[0] == 0 && q[1] == 0x80000000UL);
    CHECK(sscanf("abx5", "abc%d", &a) == 0);
    CHECK(sscanf("", "%d", &a) == -1);
    CHECK(sscanf("   ", "%d", &a) == -1);
    CHECK(sscanf("x", "%d", &a) == 0);
    CHECK(sscanf("1,2", "%d,%d", &a, &b) == 2 && b == 2);
    CHECK(sscanf("12345", "%3d%d", &a, &b) == 2);
    CHECK(a == 123 && b == 45);
    CHECK(sscanf("-", "%d", &a) == -1);
    CHECK(sscanf("0x", "%x", &u) == 1 && u == 0);
}

static void file_tests(void)
{
    FILE *f;
    int a, b;
    char s[16];

    f = fopen("TS.TMP", "w");
    fputs("10 20 word\n3.5\n", f);
    fclose(f);
    f = fopen("TS.TMP", "r");
    CHECK(fscanf(f, "%d %d %s", &a, &b, s) == 3);
    CHECK(a == 10 && b == 20);
    CHECK(same(s, "word"));
    {
        float x;

        CHECK(fscanf(f, "%f", &x) == 1);
        CHECK(((unsigned long *)&x)[0] == 0x40600000UL);
    }
    CHECK(fscanf(f, "%d", &a) == -1);
    fclose(f);
    remove("TS.TMP");
}

void mainCRTStartup(void)
{
    strtod_tests();
    strtol_tests();
    sscanf_tests();
    file_tests();
    say(bad ? "\r\nSCAN: failed\r\n" : "\r\nSCAN: ok\r\n");
    exit(bad);
}
'''


def main():
    w = sys.stdout.write
    w("/* Generated by GENSCAN.PY - do not edit. */\n")
    w("typedef struct FILE\n{\n    char *a;\n    int b;\n    char *c;\n")
    w("    int d, e, f, g;\n    char *h;\n} FILE;\n\n")
    w("extern double __cdecl strtod(const char *, char **);\n")
    w("extern double __cdecl atof(const char *);\n")
    w("extern long __cdecl strtol(const char *, char **, int);\n")
    w("extern unsigned long __cdecl strtoul(const char *, char **,\n")
    w("                                       int);\n")
    w("extern int __cdecl atoi(const char *);\n")
    w("extern long __cdecl atol(const char *);\n")
    w("extern int __cdecl sscanf(const char *, const char *, ...);\n")
    w("extern int __cdecl fscanf(FILE *, const char *, ...);\n")
    w("extern FILE *__cdecl fopen(const char *, const char *);\n")
    w("extern int __cdecl fclose(FILE *);\n")
    w("extern int __cdecl fputs(const char *, FILE *);\n")
    w("extern int __cdecl remove(const char *);\n")
    w("extern int *__cdecl _errno(void);\n")
    w("extern void __cdecl exit(int);\n")
    w("extern int __stdcall WriteFile(void *, const void *,\n")
    w("                               unsigned long, unsigned long *,\n")
    w("                               void *);\n")
    w("extern void *__stdcall GetStdHandle(unsigned long);\n\n")
    w("typedef struct Case\n{\n    const char *text;\n")
    w("    unsigned long lo, hi;\n} Case;\n\n")
    w("static const Case cases[] =\n{\n")
    texts = FIXED + midpoints()
    n = 0
    for s in texts:
        try:
            x = float(s)
        except ValueError:
            continue
        lo, hi = bits(x)
        w("    {%s,\n     0x%08XUL, 0x%08XUL},\n" % (lit(s, 8), lo, hi))
        n += 1
    w("    {0, 0, 0}\n};\n\n")
    sys.stderr.write("%d strtod cases\n" % n)
    w(TAIL)


main()
