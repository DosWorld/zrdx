#include <stdio.h>

int main(int argc, char *argv[])
{
    FILE *in, *out;
    long count = 0;
    int c;

    if (argc < 3) return 2;
    in = fopen(argv[1], "rb");
    out = fopen(argv[2], "wb");
    if (in == NULL || out == NULL) return 1;
    for (;;) {
        c = fgetc(in);
        if (c == EOF) break;
        fprintf(out, "%s%u%s", (count % 32) ? "," : "DB ", c, ((count + 1) % 32) ? "" : "\r\n");
        count++;
    }
    if (count % 32) fprintf(out, "\r\n");
    fclose(in);
    fclose(out);
    return 0;
}
