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
    unsigned fsize, l, nl, NewDataOff, DataSize;

    if (argc < 3) return 1;
    handle = open(argv[1], O_BINARY | O_RDWR);
    if (handle < 0) return 1;
    memset(buffer, 0, sizeof(buffer));
    fsize = read(handle, buffer, sizeof(buffer));
    (void)fsize;

    l = (h->n - 1) * 512 + h->r;
    nl = atoi(argv[2]);
    if (h->RelocOff < 0x40) {
        NewDataOff = ((h->nRelocs * 4 + 15) & (~15)) + 0x40;
        DataSize = l - h->hSize * 16;
        memmove(buffer + NewDataOff, buffer + h->hSize * 16, DataSize);
        memmove(buffer + 0x40, buffer + h->RelocOff, h->nRelocs * 4);
        memset(buffer + 0x1C, 0, 0x40 - 0x1C);
        strcpy(buffer + 0x1C, "Zurenava DOS extender v0.50OSE");
        memset(buffer + NewDataOff + DataSize, 0, sizeof(buffer) - NewDataOff - DataSize);
        h->RelocOff = 0x40;
        *((unsigned long *)(buffer + 0x40 - 4)) = nl;
        h->hSize = NewDataOff / 16;
    }
    if (nl < l) return 2;
    h->n = (nl + 511) / 512;
    h->r = nl - (h->n - 1) * 512;
    lseek(handle, 0, SEEK_SET);
    write(handle, buffer, nl);
    chsize(handle, nl);
    close(handle);
    return 0;
}
