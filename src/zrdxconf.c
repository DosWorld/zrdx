/*
 * ZRDXCONF: view or change the configuration trailer
 * ("ZRDX", 14 bytes) at the end of a bound program.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define TRAILER_SIZE 14L
#define SIG "ZRDX"
#define TSZ sizeof(trailer)

struct config {
    unsigned flags;
    unsigned tbuf;
    unsigned long xms;
    unsigned lowmem;
    unsigned swap;
};

static unsigned char trailer[14];

static void defaults(struct config *c)
{
    c->flags = 0;
    c->swap = 0;
    c->tbuf = 0x400;
    c->xms = 0xFFFFFFFFUL;
    c->lowmem = 0xF000;
}

static void unpack(struct config *c)
{
    c->flags = trailer[4];
    c->swap = trailer[5];
    c->tbuf = trailer[6] | (trailer[7] << 8);
    c->xms = trailer[8] | ((unsigned long)trailer[9] << 8) |
             ((unsigned long)trailer[10] << 16) |
             ((unsigned long)trailer[11]
              << 24);
    c->lowmem = trailer[12] | (trailer[13] << 8);
}

static void pack(const struct config *c)
{
    memcpy(trailer, SIG, 4);
    trailer[4] = (unsigned char)c->flags;
    trailer[5] = (unsigned char)c->swap;
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
    puts("Zurenava DOS extender setup utility v.0.53OSE");
    puts("(C) 2026, Viacheslav Komenda");
    puts("Usage:");
    puts("zrdxconf [options] <filename>");
    puts("Where options are:");
    puts(" /X<hex number> - max. XMS/RAW memory to allocate"
         "(in kilobytes)");
    puts(" /T<hex number> - max. size of transfer buffer"
         "(in paragraphs)");
    puts(" /L<hex number> - max. low memory to reserve"
         "(in paragraphs)");
    puts(" /M<0/1> - VCPI/DPMI detection mode"
         "(0-dpmi/vcpi, 1-vcpi/dpmi)");
    puts(" /B<0/1> - display copyright message(0-no, 1-yes)");
    puts(" /S<hex number> - max. swap file size"
         "(in megabytes, 0-80, 0 is off)");
    puts(" /H or /? - display this help");
}

static void show(const struct config *c)
{
    printf("  Max. of XMS/RAW memory to allocate"
           "(in kilobytes) is %lX\n",
           c->xms);
    printf("  Max. size of transfer buffer"
           "(in paragraphs) is %X\n",
           c->tbuf);
    printf("  Max. low memory to reserve(in paragraphs) is %X\n",
           c->lowmem);
    printf("  VCPI/DPMI detection mode is %s\n",
           (c->flags & 2) ? "vcpi/dpmi" : "dpmi/vcpi");
    printf("  Display copyright message is %s\n",
           (c->flags & 1) ? "no" : "yes");
    printf("  Max. swap file size(in megabytes) is %X\n",
           c->swap);
}

static int number(const char *s, int opt, unsigned long min,
                  unsigned long max, unsigned long *v)
{
    char *end;

    if (*s == 0) {
        printf("bad parameter syntax\n");
        return 1;
    }
    for (end = (char *)s; isxdigit((unsigned char)*end); end++)
        ;
    if (*end == 0 && end - s > 8)
        end = (char *)s;
    *v = strtoul(s, NULL, 16);
    if (*end != 0 || end == s) {
        printf("bad parameter syntax:\"%s\"\n", s);
        return 1;
    }
    if (*v < min || *v > max) {
        printf("\"%c\" parameter value is out of "
               "range(%lX-%lX).\n",
               opt, min, max);
        return 1;
    }
    return 0;
}

int main(int argc, char *argv[])
{
    struct config cfg, old;
    const char *name = NULL, *arg;
    FILE *f;
    int i, changed = 0, append = 0;
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

    f = fopen(name, "rb");
    if (f == NULL) {
        printf("Can't open file \"%s\"\n", name);
        return 1;
    }
    if (fseek(f, -TRAILER_SIZE, SEEK_END) != 0 ||
            fread(trailer, 1, sizeof(trailer), f) != TSZ ||
            memcmp(trailer, SIG, 4) != 0) {
        puts("zrdx header not found, creating it");
        append = changed = 1;
        defaults(&cfg);
    } else {
        unpack(&cfg);
    }
    old = cfg;

    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '/' && argv[i][0] != '-') continue;
        opt = (char)toupper((unsigned char)argv[i][1]);
        arg = argv[i] + 2;
        switch (opt) {
        case 'X':
            if (number(arg, opt, 0UL, 0xFFFFFFFFUL, &v))
                goto fail;
            cfg.xms = v;
            break;
        case 'T':
            if (number(arg, opt, 0x100UL, 0x1000UL, &v))
                goto fail;
            cfg.tbuf = (unsigned)v;
            break;
        case 'L':
            if (number(arg, opt, 0UL, 0xFFFFUL, &v))
                goto fail;
            cfg.lowmem = (unsigned)v;
            break;
        case 'M':
            if (number(arg, opt, 0UL, 1UL, &v))
                goto fail;
            cfg.flags = (cfg.flags & ~2u) | (v ? 2u : 0u);
            break;
        case 'B':
            if (number(arg, opt, 0UL, 1UL, &v))
                goto fail;
            cfg.flags = (cfg.flags & ~1u) | (v ? 0u : 1u);
            break;
        case 'S':
            if (number(arg, opt, 0UL, 0x80UL, &v))
                goto fail;
            cfg.swap = (unsigned)v;
            break;
        default:
            printf("unknown parameter\n");
            goto fail;
        }
        changed = 1;
    }

    if (!changed) {
        printf("Current configuration of \"%s\":\n", name);
        show(&cfg);
        fclose(f);
        return 0;
    }
    puts("Old configuration:");
    show(&old);
    pack(&cfg);
    fclose(f);
    f = fopen(name, "r+b");
    if (f == NULL) {
        printf("Can't write file \"%s\"\n", name);
        return 1;
    }
    if (fseek(f, append ? 0L : -TRAILER_SIZE, SEEK_END) != 0 ||
            fwrite(trailer, 1, sizeof(trailer), f) != TSZ) {
        printf("Can't write file \"%s\"\n", name);
        fclose(f);
        return 1;
    }
    if (fclose(f) != 0) {
        printf("Can't write file \"%s\"\n", name);
        return 1;
    }
    puts("New configuration:");
    show(&cfg);
    return 0;

fail:
    fclose(f);
    return 1;
}
