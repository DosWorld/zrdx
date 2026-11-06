/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Open Watcom C fixture (task 165): exports HostVal, loads
 * PLUGC.DLL, which imports it back, and calls PlugVal.
 * Built without a C runtime: wcc386 -s -ecc, see BUILD.BAT.
 */
#include "zrdxapi.h"

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

extern void dos_exit(unsigned code);
#pragma aux dos_exit = \
    "mov ah, 4Ch" \
    "int 21h" \
    parm [eax];

int HostVal(void)
{
    return 7;
}

static void fail(char step)
{
    static char text[] = "\r\nPE C: fail X\r\n$";

    text[14] = step;
    dos_print(text);
    dos_exit(1);
}

void mainCRTStartup(void)
{
    unsigned long lib;
    int (*plug)(void);

    if (ZrdxInit())
        fail('I');
    lib = LoadLibrary("PLUGC.DLL");
    if (!lib)
        fail('L');
    plug = (int (*)(void))GetProcAddress(lib, "_PlugVal");
    if (!plug)
        fail('P');
    if (plug() != 42)
        fail('V');
    if (FreeLibrary(lib))
        fail('F');
    dos_print("\r\nPE C: ok\r\n$");
    dos_exit(0);
}
