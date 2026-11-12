/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 205: an unhandled divide error ends the process with
 * the exception code (low byte 94h = 148).
 */
#include "tw32.h"

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

/* xor edx,edx; xor ecx,ecx; div ecx */
extern void div0(void);
#pragma aux div0 = 0x31 0xD2 0x31 0xC9 0xF7 0xF1 \
    modify [eax ecx edx];

void mainCRTStartup(void)
{
    div0();
    dos_print("\r\nSEHX: survived\r\n$");
    ExitProcess(0);
}
