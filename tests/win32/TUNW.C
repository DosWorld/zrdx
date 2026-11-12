/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 206: RtlUnwind over hand made frames, alone and from a
 * handler of the first pass. Prints "UNW: ok" or "UNW: fail NN".
 */
#include "tw32.h"
#include "tseh.h"

extern void W RtlUnwind(void *, void *, void *, DWORD);

static int step;
static int calls;
static void *seen[8];
static DWORD flags[8], code[8], chain[8];
static Frame fr[3];

static void fail(void)
{
    static char text[] = "\r\nUNW: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

/* logs the call, lets the search go on */
EH(h_log)
{
    seen[calls] = f;
    flags[calls] = r->flags;
    code[calls] = r->code;
    chain[calls] = fs_get(0);
    calls++;
    return 1;
}

/* first pass: unwinds to its own frame and continues */
EH(h_catch)
{
    if (!(r->flags & 2)) {
        seen[calls] = f;
        flags[calls++] = r->flags;
        RtlUnwind(f, 0, r, 0);
        return 0;
    }
    return h_log(r, f, c, d);
}

static void link3(void *h0, void *h1, void *h2)
{
    push_frame(&fr[2], h2);
    push_frame(&fr[1], h1);
    push_frame(&fr[0], h0);
}

static void tests(void)
{
    DWORD save = fs_get(0);

    calls = 0;
    link3(h_log, h_log, h_log);
    RtlUnwind(&fr[2], 0, 0, 0);
    CHECK(calls == 2);
    CHECK(seen[0] == (void *)&fr[0]);
    CHECK(seen[1] == (void *)&fr[1]);
    CHECK(flags[0] == 2 && code[0] == 0xC0000027UL);
    CHECK(chain[0] == (DWORD)&fr[0]);
    CHECK(chain[1] == (DWORD)&fr[1]);
    CHECK(fs_get(0) == (DWORD)&fr[2]);
    fs_put(0, save);

    calls = 0;
    fs_put(0, (DWORD)&fr[0]);
    RtlUnwind(0, 0, 0, 0);
    CHECK(calls == 3 && flags[2] == 6);
    CHECK(fs_get(0) == 0xFFFFFFFFUL);
    fs_put(0, save);

    calls = 0;
    link3(h_log, h_log, h_catch);
    RaiseException(0xE0000010UL, 0, 0, 0);
    CHECK(calls == 5);
    CHECK(seen[0] == (void *)&fr[0]);
    CHECK(seen[2] == (void *)&fr[2]);
    CHECK(!(flags[0] & 2) && !(flags[2] & 2));
    CHECK(seen[3] == (void *)&fr[0]);
    CHECK(seen[4] == (void *)&fr[1]);
    CHECK((flags[3] & 2) && (flags[4] & 2));
    CHECK(code[3] == 0xE0000010UL);
    CHECK(fs_get(0) == (DWORD)&fr[2]);
    pop_frame(&fr[2]);
    CHECK(fs_get(0) == save);
}

void mainCRTStartup(void)
{
    tests();
    dos_print("\r\nUNW: ok\r\n$");
    ExitProcess(0);
}
