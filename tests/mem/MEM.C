#include <stdio.h>
#include <stdlib.h>

#define MB (1024UL * 1024UL)

static int fits(unsigned long size)
{
    void *p = malloc(size);
    if (p == NULL) return 0;
    free(p);
    return 1;
}

int main(void)
{
    unsigned long lo, hi, mid;

    lo = 0;
    hi = 0xFFFFF000UL;
    while (hi - lo > 4096UL) {
        mid = lo + (hi - lo) / 2;
        if (fits(mid)) lo = mid; else hi = mid;
    }
    printf("max single malloc: %lu MB\n", lo / MB);
    return 0;
}
