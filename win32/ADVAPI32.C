/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * ADVAPI32.DLL: the minimum that console programs call: the
 * user name, a registry without keys, tokens that are denied
 * and RtlGenRandom (docs/WIN32.MD, task 213).
 */
#include "w32.h"

#define ERROR_SUCCESS 0
#define ERROR_INSUFFICIENT_BUFFER 122

W32_ANCHOR;

extern void WINAPI SetLastError(DWORD error);
extern DWORD WINAPI GetEnvironmentVariableA(const char *name,
        char *buf, DWORD size);
extern DWORD WINAPI GetEnvironmentVariableW(const WORD *name,
        WORD *buf, DWORD size);
extern DWORD WINAPI GetTickCount(void);

static DWORD seed;

static void copy(char *d, const char *s, DWORD n)
{
    while (n--)
        *d++ = *s++;
}

/*
 * The name of the user: USERNAME or "user". *size is the buffer
 * size in characters; the result includes the zero.
 */
BOOL WINAPI GetUserNameA(char *buf, DWORD *size)
{
    DWORD n = GetEnvironmentVariableA("USERNAME", buf, *size);

    if (n == 0) {
        n = 5;
        if (*size >= n)
            copy(buf, "user", 5);
    } else if (n < *size) {
        n++;
    }
    if (n > *size) {
        *size = n;
        SetLastError(ERROR_INSUFFICIENT_BUFFER);
        return FALSE;
    }
    *size = n;
    return TRUE;
}

BOOL WINAPI GetUserNameW(WORD *buf, DWORD *size)
{
    static const WORD user[] = {'u', 's', 'e', 'r', 0};
    static const WORD name[] = {
        'U', 'S', 'E', 'R', 'N', 'A', 'M', 'E', 0
    };
    DWORD n = GetEnvironmentVariableW(name, buf, *size), i;

    if (n == 0) {
        n = 5;
        if (*size >= n)
            for (i = 0; i < n; i++)
                buf[i] = user[i];
    } else if (n < *size) {
        n++;
    }
    if (n > *size) {
        *size = n;
        SetLastError(ERROR_INSUFFICIENT_BUFFER);
        return FALSE;
    }
    *size = n;
    return TRUE;
}

/* there is no registry: no key can be opened or read */
long WINAPI RegOpenKeyExA(HANDLE key, const char *sub, DWORD opt,
                          DWORD sam, HANDLE * result)
{
    if (result)
        *result = NULL;
    return ERROR_FILE_NOT_FOUND;
}

long WINAPI RegOpenKeyExW(HANDLE key, const WORD * sub,
                          DWORD opt, DWORD sam, HANDLE * result)
{
    if (result)
        *result = NULL;
    return ERROR_FILE_NOT_FOUND;
}

long WINAPI RegQueryValueExA(HANDLE key, const char *name,
                             DWORD * reserved, DWORD * type,
                             BYTE * data, DWORD * size)
{
    return ERROR_FILE_NOT_FOUND;
}

long WINAPI RegQueryValueExW(HANDLE key, const WORD * name,
                             DWORD * reserved, DWORD * type,
                             BYTE * data, DWORD * size)
{
    return ERROR_FILE_NOT_FOUND;
}

long WINAPI RegCloseKey(HANDLE key)
{
    return ERROR_SUCCESS;
}

BOOL WINAPI OpenProcessToken(HANDLE process, DWORD access,
                             HANDLE * token)
{
    if (token)
        *token = NULL;
    SetLastError(ERROR_ACCESS_DENIED);
    return FALSE;
}

BOOL WINAPI GetTokenInformation(HANDLE token, DWORD cls,
                                void *info, DWORD len,
                                DWORD * needed)
{
    SetLastError(ERROR_ACCESS_DENIED);
    return FALSE;
}

/* RtlGenRandom: xorshift from the tick count, not for secrets */
BYTE WINAPI SystemFunction036(void *buf, DWORD len)
{
    BYTE *p = buf;

    if (seed == 0)
        seed = GetTickCount() | 1;
    while (len--) {
        seed ^= seed << 13;
        seed ^= seed >> 17;
        seed ^= seed << 5;
        *p++ = (BYTE)(seed >> 11);
    }
    return 1;
}
