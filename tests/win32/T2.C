/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 179: calls the start-up functions of KERNEL32.DLL one by
 * one and prints a marker before each (no C runtime).
 */
extern unsigned long __stdcall GetVersion(void);
extern char *__stdcall GetCommandLineA(void);
extern unsigned short *__stdcall GetCommandLineW(void);
extern unsigned long __stdcall GetModuleFileNameA(void *, char *,
        unsigned long);
extern void *__stdcall GetStdHandle(unsigned long);
extern unsigned __stdcall GetACP(void);
extern unsigned __stdcall GetOEMCP(void);
extern int __stdcall GetCPInfo(unsigned, void *);
extern void *__stdcall VirtualAlloc(void *, unsigned long,
                                    unsigned long,
                                    unsigned long);
extern int __stdcall VirtualFree(void *, unsigned long,
                                 unsigned long);
extern unsigned long __stdcall GetFileType(void *);
extern int __stdcall WriteFile(void *, const void *,
                               unsigned long,
                               unsigned long *, void *);
extern void __stdcall ExitProcess(unsigned);

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

void mainCRTStartup(void)
{
    char buf[200];
    unsigned long n;
    void *p;
    char info[24];

    dos_print("S1$");
    GetVersion();
    dos_print("S2$");
    GetCommandLineA();
    dos_print("S3$");
    {
        char *c = GetCommandLineA();
        unsigned long k = 0;

        while (c[k])
            k++;
        WriteFile(GetStdHandle(0xFFFFFFF5UL), c, k, &n, 0);
        dos_print("|$");
    }
    GetCommandLineW();
    dos_print("S4$");
    n = GetModuleFileNameA(0, buf, 200);
    WriteFile(GetStdHandle(0xFFFFFFF5UL), buf, n, &n, 0);
    dos_print("|$");
    dos_print("S5$");
    p = GetStdHandle(0xFFFFFFF5UL);
    dos_print("S6$");
    GetFileType(p);
    dos_print("S7$");
    GetACP();
    GetOEMCP();
    GetCPInfo(437, info);
    dos_print("S8$");
    p = VirtualAlloc(0, 65536UL, 0x3000UL, 4);
    dos_print("S9$");
    VirtualFree(p, 0, 0x8000UL);
    dos_print("SA$");
    WriteFile(GetStdHandle(0xFFFFFFF5UL), "hi\r\n", 4, &n, 0);
    dos_print("SB$");
    ExitProcess(3);
}
