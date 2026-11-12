/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Prototypes of the Win32 functions that the tests call (the
 * documented signatures); no C runtime, no windows.h.
 */
#ifndef TW32_H
#define TW32_H

#define W __stdcall
typedef unsigned long DWORD;
typedef void *HANDLE;
typedef int BOOL;

extern HANDLE W CreateFileA(const char *, DWORD, DWORD, void *,
                            DWORD,
                            DWORD, HANDLE);
extern BOOL W CloseHandle(HANDLE);
extern BOOL W WriteFile(HANDLE, const void *, DWORD, DWORD *,
                        void *);
extern BOOL W ReadFile(HANDLE, void *, DWORD, DWORD *, void *);
extern DWORD W SetFilePointer(HANDLE, long, long *, DWORD);
extern DWORD W GetFileType(HANDLE);
extern BOOL W FlushFileBuffers(HANDLE);
extern HANDLE W GetStdHandle(DWORD);
extern BOOL W SetStdHandle(DWORD, HANDLE);
extern DWORD W GetLastError(void);
extern void W SetLastError(DWORD);
extern DWORD W GetVersion(void);
extern DWORD W GetCurrentThreadId(void);
extern void *W SetUnhandledExceptionFilter(void *);
extern int W UnhandledExceptionFilter(void *);
extern BOOL W SetConsoleCtrlHandler(void *, BOOL);
extern HANDLE W GetModuleHandleA(const char *);
extern HANDLE W LoadLibraryA(const char *);
extern BOOL W FreeLibrary(HANDLE);
extern void *W GetProcAddress(HANDLE, const char *);
extern DWORD W GetModuleFileNameA(HANDLE, char *, DWORD);
extern DWORD W GetModuleFileNameW(HANDLE, unsigned short *,
                                  DWORD);
extern char *W GetCommandLineA(void);
extern unsigned short *W GetCommandLineW(void);
extern void *W VirtualAlloc(void *, DWORD, DWORD, DWORD);
extern BOOL W VirtualFree(void *, DWORD, DWORD);
extern DWORD W VirtualQuery(const void *, void *, DWORD);
extern HANDLE W CreateEventA(void *, BOOL, BOOL, const char *);
extern BOOL W SetEnvironmentVariableA(const char *,
                                      const char *);
extern unsigned W GetACP(void);
extern unsigned W GetOEMCP(void);
extern BOOL W GetCPInfo(unsigned, void *);
extern int W MultiByteToWideChar(unsigned, DWORD, const char *,
                                 int,
                                 unsigned short *, int);
extern char *W CharUpperA(char *);
extern char *W CharLowerA(char *);
extern unsigned short *W CharUpperW(unsigned short *);
extern unsigned short *W CharLowerW(unsigned short *);
extern DWORD W CharUpperBuffA(char *, DWORD);
extern DWORD W CharLowerBuffA(char *, DWORD);
extern DWORD W CharUpperBuffW(unsigned short *, DWORD);
extern DWORD W CharLowerBuffW(unsigned short *, DWORD);
extern BOOL W IsCharAlphaA(char);
extern BOOL W IsCharAlphaW(unsigned short);
extern BOOL W IsCharAlphaNumericA(char);
extern BOOL W IsCharAlphaNumericW(unsigned short);
extern BOOL W IsCharUpperA(char);
extern BOOL W IsCharLowerA(char);
extern BOOL W IsCharUpperW(unsigned short);
extern BOOL W IsCharLowerW(unsigned short);
extern char *W CharNextA(const char *);
extern char *W CharPrevA(const char *, const char *);
extern void W ExitProcess(unsigned);
extern DWORD W GetEnvironmentVariableA(const char *, char *,
                                       DWORD);
extern DWORD W GetEnvironmentVariableW(const unsigned short *,
                                       unsigned short *, DWORD);
extern BOOL W SetEnvironmentVariableW(const unsigned short *,
                                      const unsigned short *);
extern char *W GetEnvironmentStringsA(void);
extern unsigned short *W GetEnvironmentStringsW(void);
extern BOOL W FreeEnvironmentStringsA(char *);
extern BOOL W FreeEnvironmentStringsW(unsigned short *);
extern HANDLE W GetCurrentProcess(void);
extern DWORD W GetCurrentProcessId(void);
extern BOOL W TerminateProcess(HANDLE, unsigned);
extern BOOL W IsDebuggerPresent(void);
extern void W OutputDebugStringA(const char *);
extern void W GetStartupInfoA(DWORD *);
extern void W GetSystemInfo(void *);
extern void W GlobalMemoryStatus(void *);
extern DWORD W GetTickCount(void);
extern BOOL W QueryPerformanceCounter(DWORD *);
extern BOOL W QueryPerformanceFrequency(DWORD *);
extern void W GetLocalTime(void *);
extern void W GetSystemTime(void *);
extern void W GetSystemTimeAsFileTime(DWORD *);
extern BOOL W SystemTimeToFileTime(const void *, DWORD *);
extern BOOL W FileTimeToSystemTime(const DWORD *, void *);
extern void W Sleep(DWORD);
extern HANDLE W HeapCreate(DWORD, DWORD, DWORD);
extern BOOL W HeapDestroy(HANDLE);
extern void *W HeapAlloc(HANDLE, DWORD, DWORD);
extern BOOL W HeapFree(HANDLE, DWORD, void *);
extern void *W HeapReAlloc(HANDLE, DWORD, void *, DWORD);
extern DWORD W HeapSize(HANDLE, DWORD, const void *);
extern HANDLE W GetProcessHeap(void);
extern BOOL W HeapValidate(HANDLE, DWORD, const void *);
extern DWORD W HeapCompact(HANDLE, DWORD);
extern BOOL W HeapLock(HANDLE);
extern BOOL W HeapUnlock(HANDLE);
extern HANDLE W GlobalAlloc(DWORD, DWORD);
extern HANDLE W GlobalFree(HANDLE);
extern DWORD W GlobalSize(HANDLE);
extern HANDLE W GlobalReAlloc(HANDLE, DWORD, DWORD);
extern void *W GlobalLock(HANDLE);
extern BOOL W GlobalUnlock(HANDLE);
extern HANDLE W LocalAlloc(DWORD, DWORD);
extern HANDLE W LocalFree(HANDLE);
extern DWORD W LocalSize(HANDLE);
extern HANDLE W LocalReAlloc(HANDLE, DWORD, DWORD);
extern DWORD W TlsAlloc(void);
extern BOOL W TlsFree(DWORD);
extern void *W TlsGetValue(DWORD);
extern BOOL W TlsSetValue(DWORD, void *);
extern void W InitializeCriticalSection(void *);
extern void W EnterCriticalSection(void *);
extern BOOL W TryEnterCriticalSection(void *);
extern void W LeaveCriticalSection(void *);
extern void W DeleteCriticalSection(void *);
extern long W InterlockedIncrement(long *);
extern long W InterlockedDecrement(long *);
extern long W InterlockedExchange(long *, long);
extern long W InterlockedExchangeAdd(long *, long);
extern long W InterlockedCompareExchange(long *, long, long);
extern BOOL W SetEvent(HANDLE);
extern BOOL W ResetEvent(HANDLE);
extern DWORD W WaitForSingleObject(HANDLE, DWORD);
extern DWORD W GetFileAttributesA(const char *);
extern BOOL W SetFileAttributesA(const char *, DWORD);
extern BOOL W DeleteFileA(const char *);
extern BOOL W CreateDirectoryA(const char *, void *);
extern BOOL W RemoveDirectoryA(const char *);
extern BOOL W MoveFileA(const char *, const char *);
extern BOOL W CopyFileA(const char *, const char *, BOOL);
extern DWORD W GetCurrentDirectoryA(DWORD, char *);
extern BOOL W SetCurrentDirectoryA(const char *);
extern DWORD W GetFullPathNameA(const char *, DWORD, char *,
                                char **);
extern DWORD W GetTempPathA(DWORD, char *);
extern DWORD W GetFileSize(HANDLE, DWORD *);
extern BOOL W GetFileSizeEx(HANDLE, DWORD *);
extern BOOL W SetFilePointerEx(HANDLE, DWORD, long, DWORD *,
                               DWORD);
extern BOOL W SetEndOfFile(HANDLE);
extern BOOL W GetFileTime(HANDLE, DWORD *, DWORD *, DWORD *);
extern BOOL W SetFileTime(HANDLE, const DWORD *, const DWORD *,
                          const DWORD *);
extern HANDLE W FindFirstFileA(const char *, void *);
extern BOOL W FindNextFileA(HANDLE, void *);
extern BOOL W FindClose(HANDLE);
extern BOOL W GetConsoleMode(HANDLE, DWORD *);
extern BOOL W SetConsoleMode(HANDLE, DWORD);
extern BOOL W GetConsoleScreenBufferInfo(HANDLE, void *);
extern BOOL W SetConsoleTextAttribute(HANDLE, unsigned short);
extern BOOL W SetConsoleCursorPosition(HANDLE, DWORD);
extern unsigned W GetConsoleCP(void);
extern unsigned W GetConsoleOutputCP(void);
extern BOOL W SetConsoleCP(unsigned);
extern BOOL W SetConsoleOutputCP(unsigned);
extern BOOL W SetConsoleTitleA(const char *);
extern BOOL W FlushConsoleInputBuffer(HANDLE);
extern BOOL W WriteConsoleA(HANDLE, const void *, DWORD, DWORD *,
                            void *);
extern BOOL W WriteConsoleW(HANDLE, const unsigned short *,
                            DWORD,
                            DWORD *, void *);
extern int W WideCharToMultiByte(unsigned, DWORD,
                                 const unsigned short *, int,
                                 char *,
                                 int, const char *, BOOL *);
extern BOOL W GetStringTypeW(DWORD, const unsigned short *, int,
                             unsigned short *);
extern BOOL W GetStringTypeA(DWORD, DWORD, const char *, int,
                             unsigned short *);
extern int W LCMapStringW(DWORD, DWORD, const unsigned short *,
                          int,
                          unsigned short *, int);
extern int W LCMapStringA(DWORD, DWORD, const char *, int,
                          char *,
                          int);
extern int W CompareStringW(DWORD, DWORD, const unsigned short *,
                            int,
                            const unsigned short *, int);
extern int W CompareStringA(DWORD, DWORD, const char *, int,
                            const char *, int);
extern int W lstrlenA(const char *);
extern int W lstrlenW(const unsigned short *);
extern char *W lstrcpyA(char *, const char *);
extern unsigned short *W lstrcpyW(unsigned short *,
                                  const unsigned short *);
extern char *W lstrcpynA(char *, const char *, int);
extern char *W lstrcatA(char *, const char *);
extern int W lstrcmpA(const char *, const char *);
extern int W lstrcmpiA(const char *, const char *);
extern int W lstrcmpW(const unsigned short *,
                      const unsigned short *);
extern int W lstrcmpiW(const unsigned short *,
                       const unsigned short *);
extern BOOL W IsDBCSLeadByte(unsigned char);

#endif
