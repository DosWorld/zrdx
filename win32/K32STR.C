/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * KERNEL32.DLL: code pages and MultiByteToWideChar. Single byte
 * code pages map each byte to the same code point (Latin-1);
 * UTF-8 (65001) is decoded.
 */
#include "w32.h"

#define CP_ACP 0
#define CP_OEMCP 1
#define CP_UTF8 65001UL
#define MB_ERR_INVALID_CHARS 8UL

typedef struct CpInfo {
    UINT maxchar;
    BYTE defchar[2];
    BYTE lead[12];
} CpInfo;

extern UINT WINAPI GetOEMCP(void);

DWORD w32_realcp(DWORD cp)
{
    if (cp == CP_ACP)
        return 1252;
    if (cp == CP_OEMCP)
        return GetOEMCP();
    return cp;
}

static int known(DWORD cp)
{
    cp = w32_realcp(cp);
    return cp == CP_UTF8 || cp == 28591 || w32_cptab(cp) != NULL;
}

/* code point of byte b in a single byte code page */
static DWORD byte_to_wide(const WORD *tab, BYTE b)
{
    return b < 0x80 || tab == NULL ? b : tab[b - 0x80];
}

UINT WINAPI GetACP(void)
{
    W32T("GetACP");
    return 1252;
}

UINT WINAPI GetOEMCP(void)
{
    struct dregs r;
    W32T("GetOEMCP");

    w32_zero(&r, sizeof r);
    r.eax = 0x6601;
    dos21(&r);
    if (r.cf || (r.ebx & 0xFFFF) == 0)
        return 437;
    return r.ebx & 0xFFFF;
}

BOOL WINAPI GetCPInfo(UINT cp, CpInfo *info)
{
    DWORD i;
    W32T("GetCPInfo");

    if (!known(cp) || info == NULL) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return FALSE;
    }
    info->maxchar = cp == CP_UTF8 ? 4 : 1;
    info->defchar[0] = '?';
    info->defchar[1] = 0;
    for (i = 0; i < 12; i++)
        info->lead[i] = 0;
    return TRUE;
}

/* decodes one UTF-8 sequence at s (n bytes left); returns its
   length, 0 if invalid, *cp = code point */
static DWORD utf8(const BYTE *s, DWORD n, DWORD *cp)
{
    DWORD len, v, i;

    if (s[0] < 0x80) {
        *cp = s[0];
        return 1;
    }
    len = s[0] >= 0xF0 ? 4 : s[0] >= 0xE0 ? 3 :
          s[0] >= 0xC2 ? 2 : 0;
    if (len == 0 || s[0] > 0xF4 || len > n)
        return 0;
    v = s[0] & (0x3F >> (len - 1));
    for (i = 1; i < len; i++) {
        if ((s[i] & 0xC0) != 0x80)
            return 0;
        v = (v << 6) | (s[i] & 0x3F);
    }
    if ((len == 3 && v < 0x800) ||
            (len == 4 && (v < 0x10000UL || v > 0x10FFFFUL)) ||
            (v >= 0xD800 && v < 0xE000))
        return 0;
    *cp = v;
    return len;
}

int WINAPI MultiByteToWideChar(UINT cp, DWORD flags,
                               const char *src,
                               int cb, WORD *dst, int cch)
{
    const BYTE *s = (const BYTE *)src;
    DWORD n, i = 0, out = 0, v, len;
    const WORD *tab;
    W32T("MultiByteToWideChar");

    if (src == NULL || cb == 0 || cb < -1 || cch < 0 ||
            !known(cp) ||
            (cch > 0 && dst == NULL)) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    cp = w32_realcp(cp);
    tab = w32_cptab(cp);
    if (cb == -1) {
        for (n = 0; s[n]; n++)
            ;
        n++;
    } else
        n = (DWORD)cb;
    while (i < n) {
        v = byte_to_wide(tab, s[i]);
        len = 1;
        if (cp == CP_UTF8 && v >= 0x80) {
            len = utf8(s + i, n - i, &v);
            if (len == 0) {
                if (flags & MB_ERR_INVALID_CHARS) {
                    w32_seterr(ERROR_NO_UNICODE_TRANSLATION);
                    return 0;
                }
                v = 0xFFFD;
                len = 1;
            }
        }
        i += len;
        if (v >= 0x10000UL) {
            if (cch && out + 2 > (DWORD)cch)
                goto small;
            if (cch) {
                dst[out] = (WORD)(0xD7C0 + (v >> 10));
                dst[out + 1] = (WORD)(0xDC00 + (v & 0x3FF));
            }
            out += 2;
        } else {
            if (cch && out + 1 > (DWORD)cch)
                goto small;
            if (cch)
                dst[out] = (WORD)v;
            out++;
        }
    }
    return (int)out;
small:
    w32_seterr(ERROR_INSUFFICIENT_BUFFER);
    return 0;
}

/* appends code point v as UTF-8 to out (cap bytes); returns
   the byte count, 0 if it does not fit */
static DWORD put_utf8(DWORD v, BYTE *out, DWORD at, DWORD cap)
{
    BYTE t[4];
    DWORD n, i;

    if (v < 0x80) {
        t[0] = (BYTE)v;
        n = 1;
    } else if (v < 0x800) {
        t[0] = (BYTE)(0xC0 | (v >> 6));
        t[1] = (BYTE)(0x80 | (v & 0x3F));
        n = 2;
    } else if (v < 0x10000UL) {
        t[0] = (BYTE)(0xE0 | (v >> 12));
        t[1] = (BYTE)(0x80 | ((v >> 6) & 0x3F));
        t[2] = (BYTE)(0x80 | (v & 0x3F));
        n = 3;
    } else {
        t[0] = (BYTE)(0xF0 | (v >> 18));
        t[1] = (BYTE)(0x80 | ((v >> 12) & 0x3F));
        t[2] = (BYTE)(0x80 | ((v >> 6) & 0x3F));
        t[3] = (BYTE)(0x80 | (v & 0x3F));
        n = 4;
    }
    if (cap) {
        if (at + n > cap)
            return 0;
        for (i = 0; i < n; i++)
            out[at + i] = t[i];
    }
    return n;
}

/* byte of code point c in a single byte page, -1 if none */
static long wide_to_byte(const WORD *tab, DWORD c)
{
    DWORD i;

    if (c < 0x80)
        return (long)c;
    if (tab == NULL)
        return c < 0x100 ? (long)c : -1L;
    for (i = 0; i < 128; i++)
        if (tab[i] == c)
            return (long)(0x80 + i);
    return -1L;
}

int WINAPI WideCharToMultiByte(UINT cp, DWORD flags,
                               const WORD *src,
                               int cch, char *dst, int cb,
                               const char *defchar, BOOL *used)
{
    BYTE *out = (BYTE *)dst;
    const WORD *tab;
    DWORD n, i = 0, at = 0, v, k;
    long b;
    W32T("WideCharToMultiByte");

    if (src == NULL || cch == 0 || cch < -1 || cb < 0 ||
            !known(cp) ||
            (cb > 0 && dst == NULL)) {
        w32_seterr(ERROR_INVALID_PARAMETER);
        return 0;
    }
    if (used)
        *used = FALSE;
    cp = w32_realcp(cp);
    tab = w32_cptab(cp);
    if (cch == -1) {
        for (n = 0; src[n]; n++)
            ;
        n++;
    } else
        n = (DWORD)cch;
    while (i < n) {
        v = src[i++];
        if (v >= 0xD800 && v < 0xDC00 && i < n
                && src[i] >= 0xDC00 &&
                src[i] < 0xE000)
            v = 0x10000UL + ((v - 0xD800) << 10)
                + (src[i++] - 0xDC00);
        if (cp == CP_UTF8) {
            k = put_utf8(v, out, at, (DWORD)cb);
            if (cb && k == 0)
                goto small;
            at += k;
            continue;
        }
        b = wide_to_byte(tab, v);
        if (b < 0) {
            b = defchar ? (BYTE)defchar[0] : '?';
            if (used)
                *used = TRUE;
        }
        if (cb) {
            if (at + 1 > (DWORD)cb)
                goto small;
            out[at] = (BYTE)b;
        }
        at++;
    }
    return (int)at;
small:
    w32_seterr(ERROR_INSUFFICIENT_BUFFER);
    return 0;
}
