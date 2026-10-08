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

static char buffer[0xC000];
#define h ((struct header *)buffer)

int main(int argc, char *argv[])
{
    int handle;
    unsigned fsize;
    unsigned l, nl, need, NewDataOff, DataSize;
    unsigned long v;
    char *end;

    if (argc < 3) return 1;
    v = strtoul(argv[2], &end, 10);
    if (*end != 0 || v == 0 || v > sizeof(buffer)) return 3;
    nl = (unsigned)v;
    handle = open(argv[1], O_BINARY | O_RDWR);
    if (handle < 0) return 1;
    memset(buffer, 0, sizeof(buffer));
    fsize = read(handle, buffer, sizeof(buffer));
    if (fsize < sizeof(struct header) || fsize >= sizeof(buffer) || h->signature != 0x5A4D || h->n == 0) {
        close(handle);
        return 4;
    }

    l = (h->n - 1) * 512 + h->r;
    if (l > fsize || h->hSize * 16u > l) {
        close(handle);
        return 4;
    }
    need = l;
    if (h->RelocOff < 0x40) {
        NewDataOff = ((h->nRelocs * 4 + 15) & (~15)) + 0x40;
        DataSize = l - h->hSize * 16;
        need = NewDataOff + DataSize;
    }
    if (nl < need) {
        close(handle);
        return 2;
    }
    if (h->RelocOff < 0x40) {
        memmove(buffer + NewDataOff, buffer + h->hSize * 16, DataSize);
        memmove(buffer + 0x40, buffer + h->RelocOff, h->nRelocs * 4);
        memset(buffer + 0x1C, 0, 0x40 - 0x1C);
        strcpy(buffer + 0x1C, "Zurenava DOS extender v0.51OSE");
        memset(buffer + NewDataOff + DataSize, 0, sizeof(buffer) - NewDataOff - DataSize);
        h->RelocOff = 0x40;
        *((unsigned long *)(buffer + 0x40 - 4)) = nl;
        h->hSize = NewDataOff / 16;
    }
    h->n = (nl + 511) / 512;
    h->r = nl - (h->n - 1) * 512;
    lseek(handle, 0, SEEK_SET);
    if ((unsigned)write(handle, buffer, nl) != nl) {
        close(handle);
        return 5;
    }
    chsize(handle, nl);
    close(handle);
    return 0;
}
