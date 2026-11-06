/*
 * ALIGNER: pad an MZ stub to a given size and move its
 * relocations/header so that a 14-byte setup trailer and the
 * payload offset (dword at 3Ch) fit. Data after the MZ image
 * is kept after the new image. Exit codes: 0 ok, 1 usage/io,
 * 2 size too small, 3 bad size, 4 bad or unsupported MZ file,
 * 5 write error.
 */
#include <io.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct header {
    unsigned short signature;
    unsigned short r;
    unsigned short n;
    unsigned short nRelocs;
    unsigned short hSize;
    char undef[14];
    unsigned short RelocOff;
};

#define BUFSIZE 0xC000U
#define h ((struct header *)buffer)

static char *buffer;

/* Validate the MZ file; sets *len to the image length. */
static int check(unsigned fsize, unsigned long *len)
{
    unsigned long image_len, header_len, reloc_end;

    if (fsize < sizeof(struct header) || fsize >= BUFSIZE ||
            h->signature != 0x5A4D || h->n == 0 || h->r > 512)
        return 4;
    image_len = (unsigned long)h->n * 512;
    if (h->r != 0)
        image_len -= 512 - (unsigned long)h->r;
    header_len = (unsigned long)h->hSize * 16;
    reloc_end = (unsigned long)h->RelocOff +
                (unsigned long)h->nRelocs * 4;
    if (image_len > fsize ||
            header_len < sizeof(struct header) ||
            header_len > image_len ||
            (h->nRelocs != 0 &&
             (h->RelocOff < sizeof(struct header) ||
              reloc_end > header_len)))
        return 4;
    *len = image_len;
    return 0;
}

/* Move header tail and data so relocations start at 40h. */
static int repack(unsigned long image_len, unsigned nl,
                  unsigned *need)
{
    unsigned long v;
    unsigned data, off;

    v = (((unsigned long)h->nRelocs * 4 + 15) & ~15UL) + 0x40;
    data = (unsigned)image_len - h->hSize * 16;
    if (v + data > BUFSIZE)
        return 4;
    off = (unsigned)v;
    *need = off + data;
    if (nl < *need)
        return 2;
    memmove(buffer + off, buffer + h->hSize * 16, data);
    memmove(buffer + 0x40, buffer + h->RelocOff, h->nRelocs * 4);
    memset(buffer + 0x1C, 0, 0x40 - 0x1C);
    strcpy(buffer + 0x1C, "Zurenava DOS extender v0.52OSE");
    memset(buffer + off + data, 0, BUFSIZE - off - data);
    h->RelocOff = 0x40;
    buffer[0x3C] = (char)nl;
    buffer[0x3D] = (char)(nl >> 8);
    buffer[0x3E] = 0;
    buffer[0x3F] = 0;
    h->hSize = off / 16;
    return 0;
}

/* Write the image, then the kept tail; set the file size. */
static int store(int handle, unsigned nl, char *tail,
                 unsigned tail_len)
{
    if (lseek(handle, 0, SEEK_SET) < 0 ||
            (unsigned)write(handle, buffer, nl) != nl)
        return 5;
    if (tail_len != 0 &&
            (unsigned)write(handle, tail, tail_len) != tail_len)
        return 5;
    return chsize(handle, (long)nl + tail_len) != 0 ? 5 : 0;
}

static int process(int handle, unsigned nl)
{
    unsigned fsize, need, tail_len;
    unsigned long image_len;
    char *tail = NULL;
    int rc;

    memset(buffer, 0, BUFSIZE);
    fsize = read(handle, buffer, BUFSIZE);
    rc = check(fsize, &image_len);
    if (rc)
        return rc;
    tail_len = fsize - (unsigned)image_len;
    if (tail_len != 0) {
        tail = (char *)malloc(tail_len);
        if (tail == NULL)
            return 1;
        memcpy(tail, buffer + (unsigned)image_len, tail_len);
    }
    need = (unsigned)image_len;
    rc = 0;
    if (h->RelocOff < 0x40)
        rc = repack(image_len, nl, &need);
    else if (nl < need)
        rc = 2;
    if (rc == 0) {
        h->n = (nl + 511) / 512;
        h->r = nl % 512;
        rc = store(handle, nl, tail, tail_len);
    }
    free(tail);
    return rc;
}

int main(int argc, char *argv[])
{
    int handle, rc;
    unsigned long v;
    char *end;

    if (argc < 3)
        return 1;
    v = strtoul(argv[2], &end, 10);
    if (*end != 0 || v == 0 || v > BUFSIZE)
        return 3;
    buffer = (char *)malloc(BUFSIZE);
    if (buffer == NULL)
        return 1;
    handle = open(argv[1], O_BINARY | O_RDWR);
    if (handle < 0) {
        free(buffer);
        return 1;
    }
    rc = process(handle, (unsigned)v);
    close(handle);
    free(buffer);
    return rc;
}
