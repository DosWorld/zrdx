/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Definitions of MSVCRT.DLL (docs/MSVCRT.MD): the C runtime
 * functions are plain cdecl; the KERNEL32 functions it imports
 * are stdcall (the declarations below match KERNEL32.DLL).
 */
#ifndef CRT_H
#define CRT_H

#include "w32.h"
#include <stdarg.h>

typedef void (__cdecl * PVFV)(void);
typedef int (__cdecl * PIFV)(void);

extern char *WINAPI GetCommandLineA(void);
extern WORD *WINAPI GetCommandLineW(void);
extern char *WINAPI GetEnvironmentStringsA(void);
extern WORD *WINAPI GetEnvironmentStringsW(void);
extern int WINAPI lstrlenA(const char *);
extern HANDLE WINAPI GetProcessHeap(void);
extern void *WINAPI HeapAlloc(HANDLE, DWORD, DWORD);
extern void *WINAPI HeapReAlloc(HANDLE, DWORD, void *, DWORD);
extern BOOL WINAPI HeapFree(HANDLE, DWORD, void *);
extern DWORD WINAPI HeapSize(HANDLE, DWORD, const void *);
extern void WINAPI ExitProcess(UINT code);
extern HANDLE WINAPI GetStdHandle(DWORD which);
extern BOOL WINAPI WriteFile(HANDLE, const void *, DWORD,
                             DWORD *,
                             void *);

extern HANDLE WINAPI CreateFileA(const char *, DWORD, DWORD,
                                 void *,
                                 DWORD, DWORD, HANDLE);
extern BOOL WINAPI CloseHandle(HANDLE);
extern BOOL WINAPI ReadFile(HANDLE, void *, DWORD, DWORD *,
                            void *);
extern DWORD WINAPI SetFilePointer(HANDLE, long, long *, DWORD);
extern DWORD WINAPI GetFileType(HANDLE);
extern BOOL WINAPI FlushFileBuffers(HANDLE);
extern DWORD WINAPI GetFileAttributesA(const char *);
extern BOOL WINAPI DeleteFileA(const char *);
extern BOOL WINAPI MoveFileA(const char *, const char *);
extern BOOL WINAPI CreateDirectoryA(const char *, void *);
extern BOOL WINAPI RemoveDirectoryA(const char *);
extern BOOL WINAPI SetCurrentDirectoryA(const char *);
extern DWORD WINAPI GetCurrentDirectoryA(DWORD, char *);
extern DWORD WINAPI GetLastError(void);
extern int WINAPI WideCharToMultiByte(UINT, DWORD, const WORD *,
                                      int,
                                      char *, int, const char *,
                                      BOOL *);
extern int WINAPI MultiByteToWideChar(UINT, DWORD, const char *,
                                      int,
                                      WORD *, int);

/* errno values (Microsoft numbering) */
#define CRT_EPERM 1
#define CRT_ENOENT 2
#define CRT_EBADF 9
#define CRT_ENOMEM 12
#define CRT_EACCES 13
#define CRT_EEXIST 17
#define CRT_EINVAL 22
#define CRT_EMFILE 24
#define CRT_ENOSPC 28
#define CRT_ERANGE 34
#define CRT_EDOM 33

/* FILE of the Microsoft C runtime (32 bytes): the macros getc
   and
   putc of compiled programs use _ptr and _cnt directly */
typedef struct FILE
{
    char *_ptr;
    int _cnt;
    char *_base;
    int _flag;
    int _file;
    int _charbuf;
    int _bufsiz;
    char *_tmpfname;
} FILE;

#define EOF (-1)
#define BUFSIZ 4096
#define _IOREAD 0x01
#define _IOWRT 0x02
#define _IONBF 0x04
#define _IOMYBUF 0x08
#define _IOEOF 0x10
#define _IOERR 0x20
#define _IOSTRG 0x40
#define _IORW 0x80

extern int __cdecl _read(int fd, void *buf, unsigned count);
extern int __cdecl _write(int fd, const void *buf,
                          unsigned count);
extern long __cdecl _lseek(int fd, long off, int origin);
extern int __cdecl _isatty(int fd);
extern int __cdecl _close(int fd);
extern int __cdecl _open(const char *path, int oflag, int pmode);
extern int __cdecl _unlink(const char *path);
extern int __cdecl fflush(FILE * f);
extern int __cdecl fputc(int c, FILE * f);
extern int __cdecl ferror(FILE * f);
extern FILE _iob[];
extern DWORD WINAPI GetTickCount(void);
extern void crt_flush_streams(void);
extern int crt_vsscanf(const char *, const char *, va_list);
extern int crt_vfscanf(FILE *, const char *, va_list);

/* crtflt.c: exact double <-> decimal. A double is passed as its
   two
   32 bit halves. crt_cvt writes digits (no point): mode 0 gives
   ndig significant digits, mode 1 gives ndig digits after the
   point; *decpt is the position of the point counted from the
   first digit. Returns the number of digits written (0 = the
   value rounds to zero). */
extern int crt_cvt(DWORD lo, DWORD hi, int mode, int ndig,
                   char *out,
                   int *decpt);
extern void crt_strtod_bits(const char *s, char **end, DWORD *lo,
                            DWORD *hi);

/* crtfmt.c */
typedef void (*OutFn)(int c, void *ctx);
extern int crt_format(OutFn fn, void *ctx, const char *fmt,
                      va_list ap);

/* crtmem.c, crtio.c */
extern int crt_errno;
extern void crt_seterr_win(DWORD winerr);
extern void *__cdecl malloc(DWORD n);
extern void __cdecl free(void *p);
extern void *__cdecl calloc(DWORD count, DWORD size);
extern void *__cdecl realloc(void *p, DWORD n);
extern void crt_copy(void *dst, const void *src, DWORD n);
extern void crt_io_free(void);
extern void crt_ctype_init(void);
extern void crt_lib_free(void);
extern void crt_clock_start(void);

/* crtmain.c */
extern void crt_exit(int code);
extern void crt_write_err(const char *text);
extern int crt_atterm(PVFV fn);

/* x87 control word, 16 bits */
extern unsigned crt_getcw(void);
#pragma aux crt_getcw = \
    "push eax" \
    "fnstcw [esp]" \
    "pop eax" \
    "and eax, 0FFFFh" \
    value [eax];

extern void crt_setcw(unsigned cw);
#pragma aux crt_setcw = \
    "push eax" \
    "fldcw [esp]" \
    "pop eax" \
    parm [eax];

#endif
