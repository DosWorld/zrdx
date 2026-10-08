#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define TRAILER_SIZE 14L
#define SIG "ZRXC"

struct config {
    unsigned flags;
    unsigned tbuf;
    unsigned long xms;
    unsigned lowmem;
};

static unsigned char trailer[14];

static void unpack(struct config *c)
{
    c->flags = trailer[4] | (trailer[5] << 8);
    c->tbuf = trailer[6] | (trailer[7] << 8);
    c->xms = trailer[8] | ((unsigned long)trailer[9] << 8) |
             ((unsigned long)trailer[10] << 16) | ((unsigned long)trailer[11] << 24);
    c->lowmem = trailer[12] | (trailer[13] << 8);
}

static void pack(const struct config *c)
{
    memcpy(trailer, SIG, 4);
    trailer[4] = (unsigned char)c->flags;
    trailer[5] = (unsigned char)(c->flags >> 8);
    trailer[6] = (unsigned char)c->tbuf;
    trailer[7] = (unsigned char)(c->tbuf >> 8);
    trailer[8] = (unsigned char)c->xms;
    trailer[9] = (unsigned char)(c->xms >> 8);
    trailer[10] = (unsigned char)(c->xms >> 16);
    trailer[11] = (unsigned char)(c->xms >> 24);
    trailer[12] = (unsigned char)c->lowmem;
    trailer[13] = (unsigned char)(c->lowmem >> 8);
}

static void usage(void)
{
    puts("Zurenava DOS extender setup utility v.0.50OSE (C) 2026, Viacheslav Komenda");
    puts("Usage:");
    puts("zrxsetup [options] <filename>");
    puts("Where options are:");
    puts(" /X<hex number> - max. XMS/RAW memory to allocate(in kilobytes)");
    puts(" /T<hex number> - max. size of transfer buffer(in paragraphs)");
    puts(" /L<hex number> - max. low memory to reserve(in paragraphs)");
    puts(" /M<0/1> - VCPI/DPMI detection mode(0-dpmi/vcpi, 1-vcpi/dpmi)");
    puts(" /B<0/1> - display copyright message(0-no, 1-yes)");
    puts(" /H or /? - display this help");
}

static void show(const struct config *c, const char *title)
{
    printf("%s\n", title);
    printf("  Max. of XMS/RAW memory to allocate(in kilobytes) is %lX\n", c->xms);
    printf("  Max. size of transfer buffer(in paragraphs) is %X\n", c->tbuf);
    printf("  Max. low memory to reserve(in paragraphs) is %X\n", c->lowmem);
    printf("  VCPI/DPMI detection mode is %s\n", (c->flags & 2) ? "vcpi/dpmi" : "dpmi/vcpi");
    printf("  Display copyright message is %s\n", (c->flags & 1) ? "no" : "yes");
}

static int number(const char *s, int opt, unsigned long max, unsigned long *v)
{
    char *end;

    if (*s == 0) {
        printf("bad parameter syntax\n");
        return 1;
    }
    *v = strtoul(s, &end, 16);
    if (*end != 0) {
        printf("bad parameter syntax:\"%s\"\n", s);
        return 1;
    }
    if (*v > max) {
        printf("\"%c\" parameter value is out of range(0-%lX).\n", opt, max);
        return 1;
    }
    return 0;
}

int main(int argc, char *argv[])
{
    struct config cfg, old;
    const char *name = NULL;
    FILE *f;
    int i, changed = 0;
    unsigned long v;
    char opt;

    if (argc < 2) {
        usage();
        return 1;
    }
    for (i = 1; i < argc; i++) {
        if (argv[i][0] == '/' || argv[i][0] == '-') {
            opt = (char)toupper((unsigned char)argv[i][1]);
            if (opt == 'H' || opt == '?') {
                usage();
                return 0;
            }
        } else if (name == NULL) {
            name = argv[i];
        } else {
            printf("unknown parameter\n");
            return 1;
        }
    }
    if (name == NULL) {
        usage();
        return 1;
    }

    f = fopen(name, "r+b");
    if (f == NULL) {
        printf("Can't open file \"%s\"\n", name);
        return 1;
    }
    if (fseek(f, -TRAILER_SIZE, SEEK_END) != 0 ||
        fread(trailer, 1, sizeof(trailer), f) != sizeof(trailer)) {
        printf("Can't read file \"%s\"\n", name);
        fclose(f);
        return 1;
    }
    if (memcmp(trailer, SIG, 4) != 0) {
        printf("zrdx header not detected\n");
        fclose(f);
        return 1;
    }
    unpack(&cfg);
    old = cfg;

    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '/' && argv[i][0] != '-') continue;
        opt = (char)toupper((unsigned char)argv[i][1]);
        switch (opt) {
        case 'X':
            if (number(argv[i] + 2, opt, 0xFFFFFFFFUL, &v)) goto fail;
            cfg.xms = v;
            break;
        case 'T':
            if (number(argv[i] + 2, opt, 0x1000UL, &v)) goto fail;
            cfg.tbuf = (unsigned)v;
            break;
        case 'L':
            if (number(argv[i] + 2, opt, 0xFFFFUL, &v)) goto fail;
            cfg.lowmem = (unsigned)v;
            break;
        case 'M':
            if (number(argv[i] + 2, opt, 1UL, &v)) goto fail;
            cfg.flags = (cfg.flags & ~2u) | (v ? 2u : 0u);
            break;
        case 'B':
            if (number(argv[i] + 2, opt, 1UL, &v)) goto fail;
            cfg.flags = (cfg.flags & ~1u) | (v ? 0u : 1u);
            break;
        default:
            printf("unknown parameter\n");
            goto fail;
        }
        changed = 1;
    }

    if (!changed) {
        char title[200];
        sprintf(title, "Current configuration of \"%s\":", name);
        show(&cfg, title);
        fclose(f);
        return 0;
    }
    show(&old, "Old configuration:");
    pack(&cfg);
    if (fseek(f, -TRAILER_SIZE, SEEK_END) != 0 ||
        fwrite(trailer, 1, sizeof(trailer), f) != sizeof(trailer)) {
        printf("Can't write file \"%s\"\n", name);
        fclose(f);
        return 1;
    }
    show(&cfg, "New configuration:");
    fclose(f);
    return 0;

fail:
    fclose(f);
    return 1;
}
