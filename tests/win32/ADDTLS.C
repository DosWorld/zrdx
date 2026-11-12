/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Test tool of task 207. ADDTLS file.EXE finds the 8 byte mark
 * "TLSDIR01" in a PE file and sets the TLS data directory to
 * the 24 bytes after it (Watcom cannot emit a TLS directory).
 */
#include <stdio.h>
#include <string.h>

#define MARK "TLSDIR01"
#define CHUNK 512

static unsigned long le(const unsigned char *p, int n)
{
    unsigned long v = 0;

    while (n--)
        v = (v << 8) | p[n];
    return v;
}

static void put(unsigned char *p, unsigned long v)
{
    int i;

    for (i = 0; i < 4; i++)
        p[i] = (unsigned char)(v >> (8 * i));
}

/* file offset of the mark, or -1 */
static long find(FILE *f)
{
    unsigned char b[CHUNK + 8];
    long pos = 0;
    size_t n, i, have = 0;

    while ((n = fread(b + have, 1, CHUNK, f)) > 0) {
        n += have;
        for (i = 0; i + 8 <= n; i++)
            if (memcmp(b + i, MARK, 8) == 0)
                return pos + (long)i;
        have = 7;
        memmove(b, b + n - 7, 7);
        pos += (long)n - 7;
    }
    return -1;
}

int main(int argc, char *argv[])
{
    FILE *f;
    unsigned char h[0x400], s[40], w[4];
    long mark, pe, sec;
    unsigned long rva = 0;
    int i, n;

    if (argc != 2 || (f = fopen(argv[1], "r+b")) == NULL)
        return 1;
    mark = find(f);
    if (mark < 0 || fseek(f, 0L, SEEK_SET) ||
            fread(h, 1, sizeof h, f) != sizeof h)
        return 2;
    pe = (long)le(h + 0x3C, 4);
    n = (int)le(h + pe + 6, 2);
    sec = pe + 24 + (long)le(h + pe + 20, 2);
    for (i = 0; i < n; i++) {
        fseek(f, sec + 40L * i, SEEK_SET);
        fread(s, 1, 40, f);
        if (mark >= (long)le(s + 20, 4) &&
                mark < (long)le(s + 20, 4) + (long)le(s + 16, 4))
            rva = le(s + 12, 4) + (unsigned long)mark -
                  le(s + 20, 4) + 8;
    }
    if (rva == 0)
        return 3;
    put(w, rva);
    fseek(f, pe + 192, SEEK_SET);
    fwrite(w, 1, 4, f);
    put(w, 24UL);
    fwrite(w, 1, 4, f);
    return fclose(f) != 0;
}
