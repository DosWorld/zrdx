/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 205: exceptions. DPMI exceptions and RaiseException
 * reach hand made frames of the FS:[0] chain. Prints "SEH: ok"
 * or "SEH: fail NN".
 */
#include "tw32.h"
#include "tseh.h"

/* xor edx,edx; xor ecx,ecx; div ecx; mov eax,ecx */
extern DWORD div0(DWORD a);
#pragma aux div0 = \
    0x31 0xD2 0x31 0xC9 0xF7 0xF1 0x89 0xC8 \
    parm [eax] \
    modify [eax ecx edx] \
    value [eax];

/* ud2 */
extern void ud(void);
#pragma aux ud = 0x0F 0x0B;

/* mov [0FFFFFFF0h], eax */
extern void badwrite(void);
#pragma aux badwrite = 0xA3 0xF0 0xFF 0xFF 0xFF modify [];

/* fld1; xor edx,edx; xor ecx,ecx; div ecx; fstp qword [edi] */
extern void fpdiv(DWORD * result);
#pragma aux fpdiv = \
    0xD9 0xE8 0x31 0xD2 0x31 0xC9 0xF7 0xF1 0xDD 0x1F \
    parm [edi] \
    modify [eax ecx edx];

static int step;
static DWORD got_code, got_eax, got_np, got_i0, got_i1;
static void *got_frame, *got_addr;
static int calls, order[4];

static void fail(void)
{
    static char text[] = "\r\nSEH: fail 00\r\n$";

    text[14] = (char)('0' + step / 10);
    text[15] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

EH(h_cont)
{
    got_code = r->code;
    got_np = r->nparams;
    got_i0 = r->info[0];
    got_i1 = r->info[1];
    got_frame = f;
    got_addr = r->address;
    got_eax = c->eax;
    calls++;
    return 0;
}

/* skips the instruction (length in info[14] of the test) */
static DWORD skip;

EH(h_skip)
{
    h_cont(r, f, c, d);
    CHECK(r->address == (void *)c->eip);
    c->eip += skip;
    c->ecx = 99;
    return 0;
}

EH(h_search)
{
    order[calls++] = 1;
    return 1;
}

EH(h_second)
{
    order[calls++] = 2;
    return 0;
}

/* the saved x87 state: ST(0) = 1.0, becomes 2.0 */
EH(h_fp)
{
    h_cont(r, f, c, d);
    CHECK((c->fp[0] & 0xFFFF) == 0x27F);
    CHECK(c->fp[7] == 0 && c->fp[8] == 0x80000000UL);
    CHECK((c->fp[9] & 0xFFFF) == 0x3FFF);
    c->fp[9] = (c->fp[9] & 0xFFFF0000UL) | 0x4000;
    c->eip += 2;
    return 0;
}

static int __stdcall filt(ExcPointers *p)
{
    got_code = p->rec->code;
    got_eax = p->ctx != 0;
    calls++;
    return -1;
}

static void tests(void)
{
    Frame f1, fr[2];
    DWORD args[2], res[2], save = fs_get(0);
    void *old;

    args[0] = 7;
    args[1] = 8;
    push_frame(&f1, h_cont);
    RaiseException(0xE0000001UL, 0, 2, args);
    CHECK(calls == 1 && got_code == 0xE0000001UL);
    CHECK(got_np == 2 && got_i0 == 7 && got_i1 == 8);
    CHECK(got_frame == (void *)&f1);
    pop_frame(&f1);
    CHECK(fs_get(0) == save);

    calls = 0;
    push_frame(&fr[1], h_second);
    push_frame(&fr[0], h_search);
    RaiseException(0xE0000002UL, 0, 0, 0);
    CHECK(calls == 2 && order[0] == 1 && order[1] == 2);
    pop_frame(&fr[0]);
    pop_frame(&fr[1]);

    calls = 0;
    skip = 2;
    push_frame(&f1, h_skip);
    CHECK(div0(7) == 99);
    CHECK(calls == 1 && got_code == 0xC0000094UL);
    CHECK(got_eax == 7);
    CHECK(got_addr != 0);
    ud();
    CHECK(calls == 2 && got_code == 0xC000001DUL);
    skip = 5;
    badwrite();
    CHECK(calls == 3 && got_code == 0xC0000005UL);
    CHECK(got_np == 2);
    pop_frame(&f1);
    CHECK(fs_get(0) == save);

    push_frame(&f1, h_fp);
    fpdiv(res);
    CHECK(res[0] == 0 && res[1] == 0x40000000UL);
    pop_frame(&f1);
    calls = 0;
    old = SetUnhandledExceptionFilter(filt);
    RaiseException(0xE0000003UL, 0, 0, 0);
    CHECK(calls == 1 && got_code == 0xE0000003UL && got_eax);
    CHECK(SetUnhandledExceptionFilter(old) == (void *)filt);
}

void mainCRTStartup(void)
{
    tests();
    dos_print("\r\nSEH: ok\r\n$");
    ExitProcess(0);
}
