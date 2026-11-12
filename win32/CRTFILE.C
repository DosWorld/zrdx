/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: the FILE layer over the descriptors of CRTIO.C. A
 * stream has the layout and the buffer discipline of the
 * Microsoft runtime: _cnt is the bytes left in a read buffer or
 * the room left in a write buffer, so the getc/putc macros of
 * compiled programs work. Streams on a console are unbuffered.
 */
#include "crt.h"

#define IOSLOTS 20
#define RDACT 0x400
#define WRACT 0x800
#define HEAPFILE 0x1000
#define O_WRONLY 1
#define O_RDWR 2
#define O_APPEND 8
#define O_CREAT 0x100
#define O_TRUNC 0x200
#define O_TEXT 0x4000
#define O_BINARY 0x8000

FILE _iob[IOSLOTS];
static int ready;
static FILE *extra;
static DWORD tmpcount;

typedef struct ExtraFile {
    FILE f;
    struct ExtraFile *next;
} ExtraFile;

static void stdio_init(void)
{
    if (ready)
        return;
    ready = 1;
    _iob[0]._file = 0;
    _iob[0]._flag = _IOREAD;
    _iob[1]._file = 1;
    _iob[1]._flag = _IOWRT;
    _iob[2]._file = 2;
    _iob[2]._flag = _IOWRT | _IONBF;
}

FILE *__cdecl __iob_func(void)
{
    stdio_init();
    return _iob;
}

/* a free FILE: a free slot of _iob or a heap block */
static FILE *newfile(void)
{
    ExtraFile *e;
    int i;

    stdio_init();
    for (i = 3; i < IOSLOTS; i++)
        if (_iob[i]._flag == 0)
            return &_iob[i];
    e = calloc(1, sizeof(ExtraFile));
    if (e == NULL)
        return NULL;
    e->f._flag = HEAPFILE;
    e->next = (ExtraFile *)extra;
    extra = &e->f;
    return &e->f;
}

static void freefile(FILE * f)
{
    ExtraFile **pp, *e;
    int own = f->_flag & HEAPFILE;

    f->_ptr = NULL;
    f->_base = NULL;
    f->_cnt = 0;
    f->_flag = 0;
    f->_file = -1;
    f->_tmpfname = NULL;
    if (!own)
        return;
    for (pp = (ExtraFile **) & extra; *pp; pp = &(*pp)->next)
        if (&(*pp)->f == f) {
            e = *pp;
            *pp = e->next;
            free(e);
            return;
        }
}

static int getbuf(FILE * f)
{
    f->_base = malloc(BUFSIZ);
    if (f->_base == NULL) {
        f->_base = (char *)&f->_charbuf;
        f->_bufsiz = 1;
        f->_flag |= _IONBF;
    } else {
        f->_bufsiz = BUFSIZ;
        f->_flag |= _IOMYBUF;
    }
    f->_ptr = f->_base;
    f->_cnt = 0;
    return 1;
}

/* writes the pending bytes of a write buffer */
static int drain(FILE * f)
{
    int n = (int)(f->_ptr - f->_base);

    f->_ptr = f->_base;
    f->_cnt = 0;
    if (n > 0 && (f->_flag & WRACT) && _write(f->_file, f->_base,
            (unsigned)n) != n) {
        f->_flag |= _IOERR;
        return EOF;
    }
    return 0;
}

int __cdecl _flsbuf(int ch, FILE * f)
{
    char c = (char)ch;

    stdio_init();
    if (!(f->_flag & (_IOWRT | _IORW))) {
        f->_flag |= _IOERR;
        crt_errno = CRT_EBADF;
        return EOF;
    }
    if (f->_flag & RDACT) {
        f->_flag &= ~RDACT;
        f->_cnt = 0;
        f->_ptr = f->_base;
    }
    if (f->_base == NULL && !(f->_flag & _IONBF)) {
        if (_isatty(f->_file))
            f->_flag |= _IONBF;
        else
            getbuf(f);
    }
    if (f->_flag & _IONBF) {
        f->_cnt = 0;
        if (_write(f->_file, &c, 1) != 1) {
            f->_flag |= _IOERR;
            return EOF;
        }
        return ch & 0xFF;
    }
    if (drain(f) == EOF)
        return EOF;
    f->_flag |= WRACT;
    f->_cnt = f->_bufsiz - 1;
    *f->_ptr++ = c;
    return ch & 0xFF;
}

int __cdecl _filbuf(FILE * f)
{
    int n;

    stdio_init();
    if (!(f->_flag & (_IOREAD | _IORW))) {
        f->_flag |= _IOERR;
        crt_errno = CRT_EBADF;
        return EOF;
    }
    if (f->_flag & WRACT) {
        drain(f);
        f->_flag &= ~WRACT;
    }
    if (f->_base == NULL)
        getbuf(f);
    if (f == &_iob[0])
        fflush(&_iob[1]);
    n = _read(f->_file, f->_base, (unsigned)f->_bufsiz);
    if (n <= 0) {
        f->_flag |= n == 0 ? _IOEOF : _IOERR;
        f->_cnt = 0;
        return EOF;
    }
    f->_flag |= RDACT;
    f->_cnt = n - 1;
    f->_ptr = f->_base + 1;
    return (unsigned char)f->_base[0];
}

int __cdecl fflush(FILE * f)
{
    int i, r = 0;
    ExtraFile *e;

    stdio_init();
    if (f == NULL) {
        for (i = 0; i < IOSLOTS; i++)
            if (_iob[i]._flag && fflush(&_iob[i]))
                r = EOF;
        for (e = (ExtraFile *) extra; e; e = e->next)
            if (fflush(&e->f))
                r = EOF;
        return r;
    }
    if (f->_flag & WRACT)
        return drain(f);
    return 0;
}

int __cdecl _flushall(void)
{
    return fflush(NULL);
}

void crt_flush_streams(void)
{
    if (ready)
        fflush(NULL);
}

int __cdecl fgetc(FILE * f)
{
    if ((f->_flag & WRACT) == 0 && --f->_cnt >= 0)
        return (unsigned char) * f->_ptr++;
    return _filbuf(f);
}

int __cdecl fputc(int c, FILE * f)
{
    if ((f->_flag & RDACT) == 0 && --f->_cnt >= 0)
        return (*f->_ptr++ = (char)c) & 0xFF;
    return _flsbuf(c, f);
}

int __cdecl getc(FILE * f)
{
    return fgetc(f);
}

int __cdecl putc(int c, FILE * f)
{
    return fputc(c, f);
}

int __cdecl getchar(void)
{
    stdio_init();
    return fgetc(&_iob[0]);
}

int __cdecl putchar(int c)
{
    stdio_init();
    return fputc(c, &_iob[1]);
}

int __cdecl ungetc(int c, FILE * f)
{
    if (c == EOF || !(f->_flag & (_IOREAD | _IORW)))
        return EOF;
    if (f->_flag & WRACT) {
        drain(f);
        f->_flag &= ~WRACT;
    }
    if (f->_base == NULL)
        getbuf(f);
    if (f->_cnt <= 0) {
        f->_ptr = f->_base + 1;
        f->_cnt = 0;
    }
    if (f->_ptr <= f->_base)
        return EOF;
    f->_flag |= RDACT;
    f->_flag &= ~_IOEOF;
    *--f->_ptr = (char)c;
    f->_cnt++;
    return c & 0xFF;
}

unsigned __cdecl fread(void *buf, unsigned size, unsigned count,
                       FILE * f)
{
    char *p = buf;
    unsigned want = size * count, got = 0, k;
    int c;

    if (size == 0 || count == 0)
        return 0;
    while (got < want) {
        if (f->_cnt > 0 && !(f->_flag & WRACT)) {
            k = want - got < (unsigned)f->_cnt ? want - got :
                (unsigned)f->_cnt;
            crt_copy(p + got, f->_ptr, k);
            f->_ptr += k;
            f->_cnt -= (int)k;
            got += k;
            continue;
        }
        c = _filbuf(f);
        if (c == EOF)
            break;
        p[got++] = (char)c;
    }
    return got / size;
}

unsigned __cdecl fwrite(const void *buf, unsigned size,
                        unsigned count,
                        FILE * f)
{
    const char *p = buf;
    unsigned want = size * count, put = 0, k;

    if (size == 0 || count == 0)
        return 0;
    while (put < want) {
        if ((f->_flag & _IONBF) && put > 0 &&
                !(f->_flag & RDACT)) {
            int n = _write(f->_file, p + put, want - put);

            if (n <= 0) {
                f->_flag |= _IOERR;
                break;
            }
            put += (unsigned)n;
            continue;
        }
        if (f->_cnt > 0 && !(f->_flag & RDACT)
                && (f->_flag & WRACT)) {
            k = want - put < (unsigned)f->_cnt ? want - put :
                (unsigned)f->_cnt;
            crt_copy(f->_ptr, p + put, k);
            f->_ptr += k;
            f->_cnt -= (int)k;
            put += k;
            continue;
        }
        if (_flsbuf((unsigned char)p[put], f) == EOF)
            break;
        put++;
    }
    return put / size;
}

int __cdecl fputs(const char *s, FILE * f)
{
    unsigned n = 0;

    while (s[n])
        n++;
    return fwrite(s, 1, n, f) == n ? 0 : EOF;
}

int __cdecl puts(const char *s)
{
    stdio_init();
    if (fputs(s, &_iob[1]) == EOF)
        return EOF;
    return fputc('\n', &_iob[1]) == EOF ? EOF : 0;
}

char *__cdecl fgets(char *buf, int n, FILE * f)
{
    int i = 0, c;

    if (n <= 0)
        return NULL;
    while (i < n - 1) {
        c = fgetc(f);
        if (c == EOF)
            break;
        buf[i++] = (char)c;
        if (c == '\n')
            break;
    }
    if (i == 0)
        return NULL;
    buf[i] = 0;
    return buf;
}

char *__cdecl gets(char *buf)
{
    int i = 0, c;

    stdio_init();
    while ((c = fgetc(&_iob[0])) != EOF && c != '\n')
        buf[i++] = (char)c;
    if (c == EOF && i == 0)
        return NULL;
    buf[i] = 0;
    return buf;
}

/*
 * File bytes behind the unread part of a read buffer. A text
 * stream was squeezed (CR LF -> LF): every LF counts as two.
 */
static long unread(const FILE * f)
{
    long n = f->_cnt;
    int i, st = crt_fdstate(f->_file);

    if (st & 1)
        for (i = 0; i < f->_cnt; i++)
            n += f->_ptr[i] == '\n';
    return n + ((st & 2) != 0);
}

/* file bytes the write buffer will become (LF -> CR LF) */
static long pending(const FILE * f)
{
    long n = f->_ptr - f->_base;
    long i, len = n;

    if (crt_fdstate(f->_file) & 1)
        for (i = 0; i < len; i++)
            n += f->_base[i] == '\n';
    return n;
}

int __cdecl fseek(FILE * f, long off, int whence)
{
    if (f->_flag & WRACT)
        drain(f);
    if ((f->_flag & RDACT) && whence == 1)
        off -= unread(f);
    f->_flag &= ~(WRACT | RDACT | _IOEOF);
    f->_ptr = f->_base;
    f->_cnt = 0;
    return _lseek(f->_file, off, whence) == -1L ? -1 : 0;
}

long __cdecl ftell(FILE * f)
{
    long p = _lseek(f->_file, 0, 1);

    if (p < 0)
        return -1L;
    if (f->_flag & WRACT)
        p += pending(f);
    else if (f->_flag & RDACT)
        p -= unread(f);
    return p;
}

void __cdecl rewind(FILE * f)
{
    fseek(f, 0, 0);
    f->_flag &= ~_IOERR;
}

int __cdecl feof(FILE * f)
{
    return (f->_flag & _IOEOF) != 0;
}

int __cdecl ferror(FILE * f)
{
    return (f->_flag & _IOERR) != 0;
}

void __cdecl clearerr(FILE * f)
{
    f->_flag &= ~(_IOEOF | _IOERR);
}

int __cdecl _fileno(FILE * f)
{
    return f->_file;
}

int __cdecl setvbuf(FILE * f, char *buf, int mode, unsigned size)
{
    if (mode != 0 && mode != 0x40 && mode != 4)
        return -1;
    fflush(f);
    if ((f->_flag & _IOMYBUF) && f->_base)
        free(f->_base);
    f->_flag &= ~(_IOMYBUF | _IONBF);
    f->_base = NULL;
    f->_ptr = NULL;
    f->_cnt = 0;
    if (mode == 4)
        f->_flag |= _IONBF;
    else if (buf && size > 1) {
        f->_base = buf;
        f->_ptr = buf;
        f->_bufsiz = (int)size;
    }
    return 0;
}

void __cdecl setbuf(FILE * f, char *buf)
{
    if (buf)
        setvbuf(f, buf, 0, BUFSIZ);
    else
        setvbuf(f, NULL, 4, 0);
}

/* mode string -> flags; 0 if bad */
static int parse_mode(const char *m, int *oflag, int *sflag)
{
    int plus = 0, bin = 0, i;

    for (i = 1; m[i]; i++) {
        if (m[i] == '+')
            plus = 1;
        else if (m[i] == 'b')
            bin = 1;
        else if (m[i] != 't')
            return 0;
    }
    switch (m[0]) {
    case 'r':
        *oflag = plus ? O_RDWR : 0;
        *sflag = plus ? _IORW : _IOREAD;
        break;
    case 'w':
        *oflag = (plus ? O_RDWR : O_WRONLY) | O_CREAT | O_TRUNC;
        *sflag = plus ? _IORW : _IOWRT;
        break;
    case 'a':
        *oflag = (plus ? O_RDWR : O_WRONLY) | O_CREAT | O_APPEND;
        *sflag = plus ? _IORW : _IOWRT;
        break;
    default:
        return 0;
    }
    *oflag |= bin ? O_BINARY : O_TEXT;
    return 1;
}

FILE *__cdecl fopen(const char *path, const char *mode)
{
    int oflag, sflag, fd;
    FILE *f;

    if (path == NULL || mode == NULL || !parse_mode(mode, &oflag,
            &sflag)) {
        crt_errno = CRT_EINVAL;
        return NULL;
    }
    f = newfile();
    if (f == NULL)
        return NULL;
    fd = _open(path, oflag, 0);
    if (fd < 0) {
        freefile(f);
        return NULL;
    }
    f->_file = fd;
    f->_flag = (f->_flag & HEAPFILE) | sflag;
    return f;
}

FILE *__cdecl _fdopen(int fd, const char *mode)
{
    int oflag, sflag;
    FILE *f;

    if (mode == NULL || !parse_mode(mode, &oflag, &sflag))
        return NULL;
    f = newfile();
    if (f == NULL)
        return NULL;
    f->_file = fd;
    f->_flag = (f->_flag & HEAPFILE) | sflag;
    return f;
}

int __cdecl fclose(FILE * f)
{
    int r = 0;
    char *tmp;

    if (f->_flag == 0)
        return EOF;
    if (fflush(f))
        r = EOF;
    if ((f->_flag & _IOMYBUF) && f->_base)
        free(f->_base);
    if (f->_file > 2 && _close(f->_file))
        r = EOF;
    tmp = f->_tmpfname;
    if (tmp) {
        _unlink(tmp);
        free(tmp);
    }
    if (f >= _iob && f < _iob + IOSLOTS && f < _iob + 3) {
        f->_ptr = NULL;
        f->_base = NULL;
        f->_cnt = 0;
        f->_flag = 0;
        return r;
    }
    freefile(f);
    return r;
}

int __cdecl _fcloseall(void)
{
    int i, n = 0;

    stdio_init();
    for (i = 3; i < IOSLOTS; i++)
        if (_iob[i]._flag && fclose(&_iob[i]) == 0)
            n++;
    while (extra && fclose(extra) == 0)
        n++;
    return n;
}

FILE *__cdecl tmpfile(void)
{
    char name[16];
    static const char hex[] = "0123456789ABCDEF";
    FILE *f;
    int i;
    DWORD v = GetTickCount() + tmpcount++;

    name[0] = 'T';
    name[1] = 'M';
    name[2] = 'P';
    for (i = 0; i < 5; i++)
        name[3 + i] = hex[(v >> (4 * i)) & 15];
    name[8] = 0;
    f = fopen(name, "w+b");
    if (f == NULL)
        return NULL;
    f->_tmpfname = malloc(9);
    if (f->_tmpfname)
        crt_copy(f->_tmpfname, name, 9);
    return f;
}

int __cdecl remove(const char *path)
{
    return _unlink(path);
}
