/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: low-level I/O (file descriptors) over the Win32
 * file functions. Descriptors live in chunks of small records;
 * 0, 1 and 2 are the standard handles. Text mode translates
 * CR LF to LF on input (Ctrl-Z ends the file) and LF to CR LF
 * on output.
 */
#include "crt.h"

#define FD_PER 85
#define FD_MAX 512
#define FD_OPEN 1
#define FD_TEXT 2
#define FD_APPEND 8
#define FD_EOF 0x10
#define FD_PEND 0x20
#define O_WRONLY 1
#define O_RDWR 2
#define O_APPEND 8
#define O_CREAT 0x100
#define O_TRUNC 0x200
#define O_EXCL 0x400
#define O_TEXT 0x4000
#define O_BINARY 0x8000
#define GENERIC_READ 0x80000000UL
#define GENERIC_WRITE 0x40000000UL

typedef struct FdRec {
    HANDLE h;
    DWORD flags;
    int pending;
} FdRec;

typedef struct FdChunk {
    FdRec rec[FD_PER];
    struct FdChunk *next;
} FdChunk;

extern int _fmode;
static FdChunk *fds;
static int io_ready;

/* record of descriptor fd, chunks are added as needed */
static FdRec *slot(int fd)
{
    FdChunk *c, **pp = &fds;
    int n;

    if (fd < 0 || fd >= FD_MAX)
        return NULL;
    for (n = fd / FD_PER; ; pp = &c->next, n--) {
        c = *pp;
        if (c == NULL) {
            c = calloc(1, sizeof(FdChunk));
            if (c == NULL)
                return NULL;
            *pp = c;
        }
        if (n == 0)
            return &c->rec[fd % FD_PER];
    }
}

static void io_init(void)
{
    static const DWORD std[3] = {0xFFFFFFF6UL, 0xFFFFFFF5UL,
                                 0xFFFFFFF4UL
                                };
    int i;
    FdRec *r;

    if (io_ready)
        return;
    io_ready = 1;
    for (i = 0; i < 3; i++) {
        r = slot(i);
        if (r == NULL)
            continue;
        r->h = GetStdHandle(std[i]);
        r->flags = FD_OPEN | FD_TEXT;
    }
}

static FdRec *fdget(int fd)
{
    FdRec *r;

    io_init();
    r = slot(fd);
    if (r == NULL || !(r->flags & FD_OPEN)) {
        crt_errno = CRT_EBADF;
        return NULL;
    }
    return r;
}

void crt_io_free(void)
{
    FdChunk *c, *n;

    for (c = fds; c; c = n) {
        n = c->next;
        free(c);
    }
    fds = NULL;
    io_ready = 0;
}

int __cdecl _open(const char *path, int oflag, int pmode)
{
    DWORD access, disp;
    HANDLE h;
    FdRec *r;
    int fd;

    io_init();
    access = (oflag & O_RDWR) == O_RDWR ? GENERIC_READ |
             GENERIC_WRITE :
             (oflag & O_WRONLY) ? GENERIC_WRITE : GENERIC_READ;
    if (oflag & O_CREAT)
        disp = (oflag & O_EXCL) ? 1 : (oflag & O_TRUNC) ? 2 : 4;
    else
        disp = (oflag & O_TRUNC) ? 5 : 3;
    if (disp == 5)
        access |= GENERIC_WRITE;
    for (fd = 3; fd < FD_MAX; fd++) {
        r = slot(fd);
        if (r == NULL || !(r->flags & FD_OPEN))
            break;
    }
    if (fd >= FD_MAX || r == NULL) {
        crt_errno = CRT_EMFILE;
        return -1;
    }
    h = CreateFileA(path, access, 3, NULL, disp, 0x80, NULL);
    if (h == INVALID_HANDLE_VALUE) {
        crt_seterr_win(GetLastError());
        return -1;
    }
    r->h = h;
    r->pending = 0;
    r->flags = FD_OPEN;
    if (oflag & O_APPEND)
        r->flags |= FD_APPEND;
    if ((oflag & O_BINARY) ? 0 : (oflag & O_TEXT) ? 1
            : _fmode == 0)
        r->flags |= FD_TEXT;
    return fd;
}

int __cdecl _close(int fd)
{
    FdRec *r = fdget(fd);

    if (r == NULL)
        return -1;
    r->flags = 0;
    if (fd > 2 && !CloseHandle(r->h)) {
        crt_seterr_win(GetLastError());
        return -1;
    }
    return 0;
}

int __cdecl _setmode(int fd, int mode)
{
    FdRec *r = fdget(fd);
    int old;

    if (r == NULL)
        return -1;
    old = (r->flags & FD_TEXT) ? O_TEXT : O_BINARY;
    if (mode == O_TEXT)
        r->flags |= FD_TEXT;
    else if (mode == O_BINARY)
        r->flags &= ~(DWORD)FD_TEXT;
    else {
        crt_errno = CRT_EINVAL;
        return -1;
    }
    return old;
}

int __cdecl _isatty(int fd)
{
    FdRec *r = fdget(fd);

    return r && GetFileType(r->h) == 2;
}

long __cdecl _get_osfhandle(int fd)
{
    FdRec *r = fdget(fd);

    return r ? (long)r->h : -1L;
}

long __cdecl _lseek(int fd, long off, int origin)
{
    FdRec *r = fdget(fd);
    DWORD p;
    long hi;

    if (r == NULL)
        return -1L;
    if (origin < 0 || origin > 2) {
        crt_errno = CRT_EINVAL;
        return -1L;
    }
    r->flags &= ~(DWORD)(FD_PEND | FD_EOF);
    hi = off < 0 && origin != 0 ? -1L : 0L;
    p = SetFilePointer(r->h, off, &hi, (DWORD)origin);
    if (p == 0xFFFFFFFFUL) {
        crt_seterr_win(GetLastError());
        return -1L;
    }
    return (long)p;
}

long __cdecl _tell(int fd)
{
    return _lseek(fd, 0, 1);
}

long __cdecl _filelength(int fd)
{
    long here = _lseek(fd, 0, 1), end;

    if (here < 0)
        return -1L;
    end = _lseek(fd, 0, 2);
    _lseek(fd, here, 0);
    return end;
}

int __cdecl _commit(int fd)
{
    FdRec *r = fdget(fd);

    return r && FlushFileBuffers(r->h) ? 0 : -1;
}

int crt_fdstate(int fd)
{
    FdRec *r = fdget(fd);

    if (r == NULL)
        return 0;
    return ((r->flags & FD_TEXT) ? 1 : 0) |
           ((r->flags & FD_PEND) ? 2 : 0);
}

/* text mode input: CR LF -> LF, Ctrl-Z is the end of the file */
static int squeeze(FdRec *r, unsigned char *b, int n)
{
    int i, o = 0;

    for (i = 0; i < n; i++) {
        if (b[i] == 0x1A) {
            r->flags |= FD_EOF;
            break;
        }
        if (b[i] == '\r') {
            if (i + 1 < n) {
                if (b[i + 1] == '\n')
                    continue;
            } else {
                DWORD got = 0;
                unsigned char nx;

                if (ReadFile(r->h, &nx, 1, &got, NULL) && got) {
                    if (nx == '\n') {
                        b[o++] = '\n';
                        break;
                    }
                    r->pending = nx;
                    r->flags |= FD_PEND;
                }
            }
        }
        b[o++] = b[i];
    }
    return o;
}

int __cdecl _read(int fd, void *buf, unsigned count)
{
    FdRec *r = fdget(fd);
    unsigned char *b = buf;
    DWORD got = 0;
    int n = 0, k;

    if (r == NULL)
        return -1;
    if (count == 0 || (r->flags & FD_EOF))
        return 0;
    if (r->flags & FD_PEND) {
        b[n++] = (unsigned char)r->pending;
        r->flags &= ~(DWORD)FD_PEND;
        if (n == (int)count)
            return n;
    }
    if (!ReadFile(r->h, b + n, count - (unsigned)n, &got,
                  NULL)) {
        crt_seterr_win(GetLastError());
        return -1;
    }
    if (!(r->flags & FD_TEXT))
        return n + (int)got;
    k = squeeze(r, b + n, (int)got);
    return n + k;
}

int __cdecl _write(int fd, const void *buf, unsigned count)
{
    FdRec *r = fdget(fd);
    const char *s = buf;
    char tmp[512];
    DWORD put;
    unsigned done = 0;
    int o;

    if (r == NULL)
        return -1;
    if (r->flags & FD_APPEND)
        SetFilePointer(r->h, 0, NULL, 2);
    if (!(r->flags & FD_TEXT)) {
        if (!WriteFile(r->h, buf, count, &put, NULL)
                || put != count) {
            crt_seterr_win(put == count ? GetLastError() : 112);
            return -1;
        }
        return (int)count;
    }
    while (done < count) {
        for (o = 0; done < count
                && o < (int)sizeof tmp - 1; done++) {
            if (s[done] == '\n')
                tmp[o++] = '\r';
            tmp[o++] = s[done];
        }
        if (!WriteFile(r->h, tmp, (DWORD)o, &put, NULL) ||
                put != (DWORD)o) {
            crt_seterr_win(112);
            return -1;
        }
    }
    return (int)count;
}

int __cdecl _access(const char *path, int mode)
{
    DWORD a = GetFileAttributesA(path);

    if (a == 0xFFFFFFFFUL) {
        crt_seterr_win(GetLastError());
        return -1;
    }
    if ((mode & 2) && (a & 1)) {
        crt_errno = CRT_EACCES;
        return -1;
    }
    return 0;
}

int __cdecl _unlink(const char *path)
{
    if (DeleteFileA(path))
        return 0;
    crt_seterr_win(GetLastError());
    return -1;
}

int __cdecl rename(const char *from, const char *to)
{
    if (MoveFileA(from, to))
        return 0;
    crt_seterr_win(GetLastError());
    return -1;
}

int __cdecl _mkdir(const char *path)
{
    if (CreateDirectoryA(path, NULL))
        return 0;
    crt_seterr_win(GetLastError());
    return -1;
}

int __cdecl _rmdir(const char *path)
{
    if (RemoveDirectoryA(path))
        return 0;
    crt_seterr_win(GetLastError());
    return -1;
}

int __cdecl _chdir(const char *path)
{
    if (SetCurrentDirectoryA(path))
        return 0;
    crt_seterr_win(GetLastError());
    return -1;
}

char *__cdecl _getcwd(char *buf, int size)
{
    char tmp[300];
    DWORD n = GetCurrentDirectoryA(sizeof tmp, tmp);

    if (n == 0 || n >= sizeof tmp)
        return NULL;
    if (buf == NULL)
        buf = malloc(n + 1 > (DWORD)size ? n + 1 : (DWORD)size);
    else if (n >= (DWORD)size) {
        crt_errno = CRT_ERANGE;
        return NULL;
    }
    if (buf)
        crt_copy(buf, tmp, n + 1);
    return buf;
}
