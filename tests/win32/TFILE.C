/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 196: the FILE layer of MSVCRT.DLL. Prints "FILE: ok" or
 * "FILE: fail NN"; the stdout lines "puts-line" and "P" come
 * from the stream functions themselves.
 */
typedef struct FILE {
    char *_ptr;
    int _cnt;
    char *_base;
    int _flag;
    int _file;
    int _charbuf;
    int _bufsiz;
    char *_tmpfname;
} FILE;

extern int *__cdecl _errno(void);
extern void __cdecl exit(int);
extern FILE *__cdecl fopen(const char *, const char *);
extern int __cdecl fclose(FILE *);
extern int __cdecl fflush(FILE *);
extern int __cdecl fputc(int, FILE *);
extern int __cdecl putc(int, FILE *);
extern int __cdecl fgetc(FILE *);
extern int __cdecl getc(FILE *);
extern int __cdecl ungetc(int, FILE *);
extern unsigned __cdecl fread(void *, unsigned, unsigned,
                              FILE *);
extern unsigned __cdecl fwrite(const void *, unsigned, unsigned,
                               FILE *);
extern int __cdecl fputs(const char *, FILE *);
extern int __cdecl puts(const char *);
extern int __cdecl putchar(int);
extern char *__cdecl fgets(char *, int, FILE *);
extern int __cdecl fseek(FILE *, long, int);
extern long __cdecl ftell(FILE *);
extern void __cdecl rewind(FILE *);
extern int __cdecl feof(FILE *);
extern int __cdecl ferror(FILE *);
extern void __cdecl clearerr(FILE *);
extern int __cdecl _fileno(FILE *);
extern int __cdecl setvbuf(FILE *, char *, int, unsigned);
extern void __cdecl setbuf(FILE *, char *);
extern FILE *__cdecl tmpfile(void);
extern int __cdecl remove(const char *);
extern FILE *__cdecl _fdopen(int, const char *);
extern int __cdecl _fcloseall(void);
extern int __cdecl _flushall(void);
extern long __cdecl _filelength(int);
extern int __cdecl _open(const char *, int, int);
extern int __cdecl _close(int);
extern __declspec(dllimport) char _iob[];

extern int __stdcall WriteFile(void *, const void *,
                               unsigned long,
                               unsigned long *, void *);
extern void *__stdcall GetStdHandle(unsigned long);

#define EOF (-1)
#define STDOUT ((FILE *)(_iob + 32))

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
    char t[] = "\r\nFILE: fail 00\r\n";

    t[14] = (char)('0' + step / 10);
    t[15] = (char)('0' + step % 10);
    say(t);
    exit(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static int same(const char *a, const char *b)
{
    while (*a && *a == *b) {
        a++;
        b++;
    }
    return *a == *b;
}

static void text_tests(void)
{
    FILE *f;
    char buf[64];

    CHECK(fopen("TF.NONE", "r") == 0 && *_errno() == 2);
    CHECK(fopen("TF.TMP", "q") == 0 && *_errno() == 22);
    f = fopen("TF.TMP", "w");
    CHECK(f != 0 && f->_file >= 3);
    CHECK(fputs("line1\n", f) == 0);
    CHECK(fputc('X', f) == 'X' && putc('Y', f) == 'Y');
    CHECK(fwrite("abcdef", 1, 6, f) == 6);
    CHECK(fputs("end\n", f) == 0);
    CHECK(fflush(f) == 0 && _filelength(f->_file) == 20);
    CHECK(fclose(f) == 0);
    f = fopen("TF.TMP", "rb");
    CHECK(fread(buf, 1, 40, f) == 20 && buf[5] == 13 &&
          buf[6] == 10);
    CHECK(feof(f) && !ferror(f));
    CHECK(fclose(f) == 0);
    f = fopen("TF.TMP", "r");
    CHECK(fgets(buf, 64, f) == buf && same(buf, "line1\n"));
    CHECK(fgetc(f) == 'X' && getc(f) == 'Y');
    CHECK(ungetc('Z', f) == 'Z' && fgetc(f) == 'Z');
    CHECK(fread(buf, 2, 3, f) == 3 && buf[4] == 'e');
    CHECK(fgets(buf, 64, f) == buf && same(buf, "end\n"));
    CHECK(fgetc(f) == EOF && feof(f));
    CHECK(fgets(buf, 64, f) == 0);
    clearerr(f);
    CHECK(!feof(f));
    CHECK(fclose(f) == 0);
    CHECK(fclose(f) == EOF);
    f = fopen("TF.TMP", "a");
    CHECK(fputs("more\n", f) == 0 && fclose(f) == 0);
    f = fopen("TF.TMP", "rb");
    CHECK(fseek(f, 0, 2) == 0 && ftell(f) == 26);
    fclose(f);
    CHECK(remove("TF.TMP") == 0 && remove("TF.TMP") == -1);
}

static void binary_tests(void)
{
    FILE *f;
    unsigned char buf[300], big[10000], back[10000];
    int i;

    for (i = 0; i < 300; i++)
        buf[i] = (unsigned char)i;
    f = fopen("TF.BIN", "wb");
    CHECK(fwrite(buf, 100, 3, f) == 3 && ftell(f) == 300);
    CHECK(fclose(f) == 0);
    f = fopen("TF.BIN", "rb+");
    CHECK(fseek(f, 10, 0) == 0 && fread(back, 1, 10, f) == 10);
    CHECK(back[0] == 10 && back[9] == 19 && ftell(f) == 20);
    CHECK(fseek(f, 5, 1) == 0 && ftell(f) == 25);
    CHECK(fgetc(f) == 25);
    CHECK(fseek(f, -1, 2) == 0 && fgetc(f) == (299 & 255));
    CHECK(fgetc(f) == EOF && feof(f));
    rewind(f);
    CHECK(!feof(f) && ftell(f) == 0 && fgetc(f) == 0);
    CHECK(fseek(f, 0, 0) == 0 && fputc(0x7F, f) == 0x7F);
    CHECK(fseek(f, 0, 0) == 0 && fgetc(f) == 0x7F);
    fclose(f);
    for (i = 0; i < 10000; i++)
        big[i] = (unsigned char)(i * 7 + i / 256);
    f = fopen("TF.BIN", "wb");
    CHECK(fwrite(big, 1, 10000, f) == 10000 && fclose(f) == 0);
    f = fopen("TF.BIN", "rb");
    CHECK(fread(back, 1, 10000, f) == 10000);
    for (i = 0; i < 10000; i++)
        if (back[i] != big[i])
            fail();
    CHECK(fread(back, 1, 1, f) == 0 && feof(f));
    fclose(f);
    remove("TF.BIN");
}

static void buffer_tests(void)
{
    FILE *f;
    char ub[32];

    f = fopen("TF.BUF", "wb");
    CHECK(setvbuf(f, 0, 4, 0) == 0);
    CHECK(fputs("0123456789", f) == 0);
    CHECK(_filelength(f->_file) == 10);
    fclose(f);
    f = fopen("TF.BUF", "wb");
    CHECK(setvbuf(f, ub, 0, 32) == 0 && f->_bufsiz == 32);
    CHECK(fputs("abc", f) == 0 && _filelength(f->_file) == 0);
    CHECK(fflush(f) == 0 && _filelength(f->_file) == 3);
    setbuf(f, 0);
    CHECK(fputc('d', f) == 'd' && _filelength(f->_file) == 4);
    CHECK(_flushall() >= 0);
    CHECK(_fcloseall() >= 1);
    remove("TF.BUF");
    f = tmpfile();
    CHECK(f != 0 && fputs("tmp data", f) == 0);
    rewind(f);
    CHECK(fgetc(f) == 't' && fgetc(f) == 'm');
    CHECK(_fileno(f) >= 3 && fclose(f) == 0);
    f = _fdopen(_open("TF.FD", 0x301, 0), "w");
    CHECK(f != 0 && fputs("fd", f) == 0 && fclose(f) == 0);
    CHECK(remove("TF.FD") == 0);
}

/* ftell and fseek count CR LF of text streams */
static void pos_tests(void)
{
    FILE *f = fopen("TF.POS", "w");
    char buf[16];

    CHECK(f != 0 && fputs("a\nb", f) == 0);
    CHECK(ftell(f) == 4);
    CHECK(fputs("\nc\n", f) == 0 && fclose(f) == 0);
    f = fopen("TF.POS", "r");
    CHECK(getc(f) == 'a' && ftell(f) == 1);
    CHECK(getc(f) == '\n' && ftell(f) == 3);
    CHECK(fseek(f, 0, 1) == 0 && getc(f) == 'b');
    CHECK(fgets(buf, 16, f) == buf && same(buf, "\n"));
    CHECK(ftell(f) == 6);
    CHECK(fseek(f, 0, 2) == 0 && ftell(f) == 9);
    CHECK(fclose(f) == 0 && remove("TF.POS") == 0);
}

void mainCRTStartup(void)
{
    pos_tests();
    text_tests();
    binary_tests();
    buffer_tests();
    CHECK(_fileno(STDOUT) == 1);
    CHECK(puts("puts-line") >= 0 && putchar('P') == 'P');
    CHECK(putchar('\n') == '\n' && fflush(STDOUT) == 0);
    say("\r\nFILE: ok\r\n");
    exit(0);
}
