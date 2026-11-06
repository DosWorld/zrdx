/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 178: a Watcom exe calls GetVersion of our KERNEL32.DLL
 * through the Open Watcom import library; exit code = result.
 */
extern unsigned long __stdcall GetVersion(void);

extern void dos_exit(unsigned code);
#pragma aux dos_exit = \
    "mov ah, 4Ch" \
    "int 21h" \
    parm [eax];

void mainCRTStartup(void)
{
    dos_exit((unsigned)GetVersion());
}
