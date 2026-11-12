/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: file size, position, time and the directory
 * search (FindFirstFile and friends over the LFN functions 714Eh
 * and 714Fh, the plain 4Eh and 4Fh when the LFN API is absent).
 */
#include "w32.h"

#define FIND_MAGIC 0x464E4430UL
#define FA_DIR 0x10UL

typedef struct FileTime {
    DWORD lo, hi;
} FileTime;

typedef struct SysTime {
    WORD year, month, wday, day, hour, minute, second, msec;
} SysTime;

/* the Win32 and the LFN layout are the same, 318 bytes */
typedef struct FindData {
    DWORD attr;
    FileTime created, accessed, written;
    DWORD sizehi, sizelo;
    DWORD reserved0, reserved1;
    char name[260];
    char alt[14];
} FindData;

typedef struct FindRec {
    DWORD magic;
    DWORD lfn;
    DWORD dos;
    BYTE dta[44];
} FindRec;

extern HANDLE WINAPI GetProcessHeap(void);
extern void *WINAPI HeapAlloc(HANDLE, DWORD, DWORD);
extern BOOL WINAPI HeapFree(HANDLE, DWORD, void *);
extern BOOL WINAPI SystemTimeToFileTime(const SysTime *,
                                        FileTime *);
extern BOOL WINAPI FileTimeToSystemTime(const FileTime *,
                                        SysTime *);
extern DWORD WINAPI SetFilePointer(HANDLE, long, long *, DWORD);
extern DWORD WINAPI GetLastError(void);

static void dos_to_ft(DWORD date, DWORD time, FileTime *ft)
{
    SysTime st;

    st.year = (WORD)(1980 + ((date >> 9) & 127));
    st.month = (WORD)((date >> 5) & 15);
    st.day = (WORD)(date & 31);
    st.hour = (WORD)((time >> 11) & 31);
    st.minute = (WORD)((time >> 5) & 63);
    st.second = (WORD)((time & 31) * 2);
    st.msec = 0;
    st.wday = 0;
    if (!SystemTimeToFileTime(&st, ft))
        ft->lo = ft->hi = 0;
}

static void ft_to_dos(const FileTime *ft, DWORD *date,
                      DWORD *time)
{
    SysTime st;

    if (!FileTimeToSystemTime(ft, &st) || st.year < 1980) {
        *date = 0x21;
        *time = 0;
        return;
    }
    *date = ((DWORD)(st.year - 1980) << 9) |
            ((DWORD)st.month << 5) | st.day;
    *time = ((DWORD)st.hour << 11) | ((DWORD)st.minute << 5) |
            (st.second / 2);
}

DWORD WINAPI GetFileSize(HANDLE h, DWORD *hi)
{
    DWORD pos, size;
    long zero = 0;
    W32T("GetFileSize");

    pos = SetFilePointer(h, 0, &zero, 1);
    if (pos == 0xFFFFFFFFUL)
        return pos;
    zero = 0;
    size = SetFilePointer(h, 0, &zero, 2);
    zero = 0;
    SetFilePointer(h, (long)pos, &zero, 0);
    if (hi)
        *hi = 0;
    return size;
}

BOOL WINAPI GetFileSizeEx(HANDLE h, DWORD *size)
{
    DWORD n;
    W32T("GetFileSizeEx");

    w32_seterr(0);
    n = GetFileSize(h, NULL);
    if (n == 0xFFFFFFFFUL && GetLastError() != 0)
        return FALSE;
    size[0] = n;
    size[1] = 0;
    return TRUE;
}

BOOL WINAPI SetFilePointerEx(HANDLE h, DWORD lo, long hi,
                             DWORD *newpos,
                             DWORD method)
{
    DWORD p;
    long high = hi;
    W32T("SetFilePointerEx");

    p = SetFilePointer(h, (long)lo, &high, method);
    if (p == 0xFFFFFFFFUL)
        return FALSE;
    if (newpos) {
        newpos[0] = p;
        newpos[1] = 0;
    }
    return TRUE;
}

BOOL WINAPI SetEndOfFile(HANDLE h)
{
    struct dregs r;
    int d = w32_dosh(h);
    W32T("SetEndOfFile");

    if (d < 0) {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    w32_zero(&r, sizeof r);
    r.eax = 0x4000;
    r.ebx = (DWORD)d;
    r.ecx = 0;
    dos21(&r);
    if (r.cf)
        w32_seterr(w32_doserr(&r));
    return !r.cf;
}

BOOL WINAPI GetFileTime(HANDLE h, FileTime *created,
                        FileTime *accessed,
                        FileTime *written)
{
    struct dregs r;
    int d = w32_dosh(h);
    W32T("GetFileTime");

    if (d < 0) {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    w32_zero(&r, sizeof r);
    r.eax = 0x5700;
    r.ebx = (DWORD)d;
    dos21(&r);
    if (r.cf) {
        w32_seterr(w32_doserr(&r));
        return FALSE;
    }
    if (written)
        dos_to_ft(r.edx & 0xFFFF, r.ecx & 0xFFFF, written);
    if (created)
        dos_to_ft(r.edx & 0xFFFF, r.ecx & 0xFFFF, created);
    if (accessed)
        dos_to_ft(r.edx & 0xFFFF, 0, accessed);
    return TRUE;
}

BOOL WINAPI SetFileTime(HANDLE h, const FileTime *created,
                        const FileTime *accessed,
                        const FileTime *written)
{
    struct dregs r;
    DWORD date, time;
    int d = w32_dosh(h);
    W32T("SetFileTime");

    if (d < 0) {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    if (written == NULL)
        return TRUE;
    ft_to_dos(written, &date, &time);
    w32_zero(&r, sizeof r);
    r.eax = 0x5701;
    r.ebx = (DWORD)d;
    r.ecx = time;
    r.edx = date;
    dos21(&r);
    if (r.cf)
        w32_seterr(w32_doserr(&r));
    return !r.cf;
}

/* DOS DTA of a plain search -> FIND_DATA */
static void dta_to_data(const BYTE *dta, FindData *fd)
{
    DWORD i;

    w32_zero(fd, sizeof(FindData));
    fd->attr = dta[21];
    dos_to_ft(dta[24] | (dta[25] << 8), dta[22] | (dta[23] << 8),
              &fd->written);
    fd->created = fd->written;
    fd->accessed = fd->written;
    fd->sizelo = dta[26] | (dta[27] << 8) |
                 ((DWORD)dta[28] << 16) | ((DWORD)dta[29] << 24);
    for (i = 0; i < 13 && dta[30 + i]; i++)
        fd->name[i] = (char)dta[30 + i];
    if (fd->attr == 0)
        fd->attr = 0x80;
}

HANDLE WINAPI FindFirstFileA(const char *pattern, FindData *fd)
{
    struct dregs r;
    FindRec *rec;
    HANDLE heap = GetProcessHeap();
    W32T("FindFirstFileA");

    if (pattern == NULL || fd == NULL) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return INVALID_HANDLE_VALUE;
    }
    rec = HeapAlloc(heap, 8, sizeof(FindRec));
    if (rec == NULL)
        return INVALID_HANDLE_VALUE;
    rec->magic = FIND_MAGIC;
    w32_zero(&r, sizeof r);
    r.eax = 0x714E;
    r.ecx = 0x0016;
    r.edx = (DWORD)pattern;
    r.esi = 0;
    r.edi = (DWORD)fd;
    dos21(&r);
    if (!r.cf) {
        rec->lfn = 1;
        rec->dos = r.eax & 0xFFFF;
        return rec;
    }
    if ((r.eax & 0xFFFF) != 0x7100) {
        w32_seterr(w32_doserr(&r));
        HeapFree(heap, 0, rec);
        return INVALID_HANDLE_VALUE;
    }
    w32_zero(&r, sizeof r);
    r.eax = 0x1A00;
    r.edx = (DWORD)rec->dta;
    dos21(&r);
    w32_zero(&r, sizeof r);
    r.eax = 0x4E00;
    r.ecx = 0x0016;
    r.edx = (DWORD)pattern;
    dos21(&r);
    if (r.cf) {
        w32_seterr(w32_doserr(&r));
        HeapFree(heap, 0, rec);
        return INVALID_HANDLE_VALUE;
    }
    dta_to_data(rec->dta, fd);
    return rec;
}

BOOL WINAPI FindNextFileA(HANDLE h, FindData *fd)
{
    struct dregs r;
    FindRec *rec = h;
    W32T("FindNextFileA");

    if (rec == NULL || rec == INVALID_HANDLE_VALUE ||
            rec->magic != FIND_MAGIC || fd == NULL) {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    w32_zero(&r, sizeof r);
    if (rec->lfn) {
        r.eax = 0x714F;
        r.ebx = rec->dos;
        r.esi = 0;
        r.edi = (DWORD)fd;
        dos21(&r);
    } else {
        r.eax = 0x1A00;
        r.edx = (DWORD)rec->dta;
        dos21(&r);
        w32_zero(&r, sizeof r);
        r.eax = 0x4F00;
        dos21(&r);
        if (!r.cf)
            dta_to_data(rec->dta, fd);
    }
    if (r.cf)
        w32_seterr(w32_doserr(&r));
    return !r.cf;
}

BOOL WINAPI FindClose(HANDLE h)
{
    struct dregs r;
    FindRec *rec = h;
    W32T("FindClose");

    if (rec == NULL || rec == INVALID_HANDLE_VALUE ||
            rec->magic != FIND_MAGIC) {
        w32_seterr(ERROR_INVALID_HANDLE);
        return FALSE;
    }
    if (rec->lfn) {
        w32_zero(&r, sizeof r);
        r.eax = 0x71A1;
        r.ebx = rec->dos;
        dos21(&r);
    }
    rec->magic = 0;
    HeapFree(GetProcessHeap(), 0, rec);
    return TRUE;
}
