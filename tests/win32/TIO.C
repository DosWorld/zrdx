/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 195: errno, the memory functions and the low-level I/O of
 * MSVCRT.DLL. Prints "IO: ok" or "IO: fail NN".
 */
extern int *__cdecl _errno(void);
extern void *__cdecl malloc(unsigned);
extern void *__cdecl calloc(unsigned, unsigned);
extern void *__cdecl realloc(void *, unsigned);
extern void __cdecl free(void *);
extern unsigned __cdecl _msize(void *);
extern int __cdecl _open(const char *, int, int);
extern int __cdecl _close(int);
extern int __cdecl _read(int, void *, unsigned);
extern int __cdecl _write(int, const void *, unsigned);
extern long __cdecl _lseek(int, long, int);
extern long __cdecl _tell(int);
extern long __cdecl _filelength(int);
extern int __cdecl _setmode(int, int);
extern int __cdecl _isatty(int);
extern long __cdecl _get_osfhandle(int);
extern int __cdecl _access(const char *, int);
extern int __cdecl _unlink(const char *);
extern int __cdecl rename(const char *, const char *);
extern int __cdecl _mkdir(const char *);
extern int __cdecl _rmdir(const char *);
extern int __cdecl _chdir(const char *);
extern char *__cdecl _getcwd(char *, int);
extern int __cdecl _commit(int);
extern void __cdecl exit(int);

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

static void fail(void)
{
    char t[] = "\r\nIO: fail 00\r\n";

    t[12] = (char)('0' + step / 10);
    t[13] = (char)('0' + step % 10);
    say(t);
    exit(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#define O_CREAT 0x100
#define O_TRUNC 0x200
#define O_EXCL 0x400
#define O_TEXT 0x4000
#define O_BINARY 0x8000

static void mem_tests(void)
{
    unsigned char *p, *q;
    unsigned i;

    p = malloc(100);
    CHECK(p != 0 && _msize(p) >= 100);
    for (i = 0; i < 100; i++)
        p[i] = (unsigned char)i;
    q = realloc(p, 5000);
    CHECK(q != 0 && q[99] == 99 && _msize(q) >= 5000);
    free(q);
    p = calloc(10, 30);
    CHECK(p != 0);
    for (i = 0; i < 300; i++)
        if (p[i])
            fail();
    free(p);
    CHECK(realloc(0, 10) != 0);
    CHECK(malloc(0) != 0);
    CHECK(calloc(0x10000, 0x10001UL) == 0 && *_errno() == 12);
    free(0);
}

static void file_tests(void)
{
    char buf[16];
    int fd;

    CHECK(_open("TIO.NONE", O_RDONLY, 0) == -1
        && *_errno() == 2);
    fd = _open("TIO.TMP", O_CREAT | O_TRUNC | O_RDWR | O_TEXT,
        0);
    CHECK(fd >= 3);
    CHECK(_write(fd, "a\nb", 3) == 3);
    CHECK(_lseek(fd, 0, 2) == 4 && _filelength(fd) == 4);
    CHECK(_lseek(fd, 0, 0) == 0 && _tell(fd) == 0);
    CHECK(_setmode(fd, O_BINARY) == O_TEXT);
    CHECK(_read(fd, buf, 16) == 4 && buf[1] == '\r' &&
          buf[2] == '\n');
    CHECK(_lseek(fd, 0, 0) == 0);
    CHECK(_setmode(fd, O_TEXT) == O_BINARY);
    CHECK(_read(fd, buf, 16) == 3);
    CHECK(buf[0] == 'a' && buf[1] == '\n' && buf[2] == 'b');
    CHECK(_read(fd, buf, 16) == 0);
    CHECK(_commit(fd) == 0);
    CHECK(_get_osfhandle(fd) != -1L && !_isatty(fd));
    CHECK(_close(fd) == 0);
    CHECK(_close(fd) == -1 && *_errno() == 9);
    fd = _open("TIO.TMP", O_CREAT | O_EXCL | O_RDWR, 0);
    CHECK(fd == -1 && *_errno() == 17);
    fd = _open("TIO.TMP", O_TRUNC | O_RDWR | O_BINARY, 0);
    CHECK(fd >= 3 && _write(fd, "x\x1Ay", 3) == 3);
    CHECK(_setmode(fd, O_TEXT) == O_BINARY && _lseek(fd, 0,
        0) == 0);
    CHECK(_read(fd, buf, 16) == 1 && buf[0] == 'x');
    CHECK(_read(fd, buf, 16) == 0);
    CHECK(_close(fd) == 0);
    CHECK(_access("TIO.TMP", 0) == 0 &&
          _access("TIO.NONE", 0) == -1);
    CHECK(rename("TIO.TMP", "TIO2.TMP") == 0);
    CHECK(_access("TIO.TMP", 0) == -1
        && _unlink("TIO2.TMP") == 0);
    CHECK(_unlink("TIO2.TMP") == -1 && *_errno() == 2);
}

static void dir_tests(void)
{
    char a[200], b[200];
    int con;

    CHECK(_getcwd(a, 200) == a && a[1] == ':');
    CHECK(_getcwd(b, 2) == 0 && *_errno() == 34);
    CHECK(_mkdir("TIODIR") == 0);
    CHECK(_mkdir("TIODIR") == -1);
    CHECK(_chdir("TIODIR") == 0);
    CHECK(_getcwd(b, 200) == b && b[0] == a[0]);
    CHECK(_chdir("..") == 0);
    CHECK(_getcwd(b, 200) == b && b[2] == a[2] && b[4] == a[4]);
    CHECK(_rmdir("TIODIR") == 0 && _rmdir("TIODIR") == -1);
    CHECK(!_isatty(1));
    con = _open("CON", O_WRONLY, 0);
    CHECK(con >= 3 && _isatty(con));
    CHECK(_write(con, "", 0) == 0);
    CHECK(_close(con) == 0);
    CHECK(_write(1, "w\n", 2) == 2);
}

void mainCRTStartup(void)
{
    mem_tests();
    file_tests();
    dir_tests();
    say("\r\nIO: ok\r\n");
    exit(0);
}
