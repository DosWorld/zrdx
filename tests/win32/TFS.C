/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Task 187: one check per exported function of the file,
 * directory and search group (long file names included). Prints
 * "FS: ok" or "FS: fail NN". Needs a writable current directory.
 */
#include "tw32.h"

extern void dos_print(const char *text);
#pragma aux dos_print = \
    "mov ah, 9" \
    "int 21h" \
    parm [edx] \
    modify [eax];

static int step;

static void fail(void)
{
    static char text[] = "\r\nFS: fail 00\r\n$";

    text[12] = (char)('0' + step / 10);
    text[13] = (char)('0' + step % 10);
    dos_print(text);
    ExitProcess(1);
}

#define CHECK(c) do { step++; if (!(c)) fail(); } while (0)

static int endswith(const char *s, const char *tail)
{
    int n = 0, m = 0;

    while (s[n])
        n++;
    while (tail[m])
        m++;
    if (m > n)
        return 0;
    while (m)
        if (s[--n] != tail[--m])
            return 0;
    return 1;
}

static void put(const char *name, const char *text, DWORD n)
{
    HANDLE f;
    DWORD done;

    f = CreateFileA(name, 0x40000000UL, 0, 0, 2, 0x80, 0);
    if (f == (HANDLE) - 1L || !WriteFile(f, text, n, &done, 0))
        fail();
    CloseHandle(f);
}

static void dir_tests(void)
{
    char cur[300], full[300], *part;
    DWORD n;

    CHECK(CreateDirectoryA("ZRDXD", 0));
    CHECK(!CreateDirectoryA("ZRDXD", 0));
    CHECK(GetLastError() == 183);
    CHECK(GetFileAttributesA("ZRDXD") & 0x10);
    CHECK(GetFileAttributesA("ZRDXNONE") == 0xFFFFFFFFUL);
    CHECK(GetLastError() == 2);
    n = GetCurrentDirectoryA(300, cur);
    CHECK(n > 2 && cur[1] == ':' && cur[2] == '\\');
    CHECK(GetCurrentDirectoryA(2, full) == n + 1);
    CHECK(SetCurrentDirectoryA("ZRDXD"));
    CHECK(GetCurrentDirectoryA(300, full) > n);
    CHECK(endswith(full, "ZRDXD"));
    CHECK(SetCurrentDirectoryA(".."));
    CHECK(GetCurrentDirectoryA(300, full) == n);
    CHECK(!SetCurrentDirectoryA("ZRDXNONE"));
    n = GetFullPathNameA("ZRDXD\\B.TXT", 300, full, &part);
    CHECK(n > 8 && full[1] == ':' && part != 0);
    CHECK(part[0] == 'B');
    CHECK(GetTempPathA(300, full) >= 2 && endswith(full, "\\"));
}

static void file_tests(void)
{
    HANDLE f;
    DWORD size[2], ft[2], hi, pos[2];
    unsigned short st[8], back[8];

    put("ZRDXD\\A.TXT", "hello", 5);
    CHECK(GetFileAttributesA("ZRDXD\\A.TXT") != 0xFFFFFFFFUL);
    CHECK(SetFileAttributesA("ZRDXD\\A.TXT", 1));
    CHECK(GetFileAttributesA("ZRDXD\\A.TXT") & 1);
    CHECK(SetFileAttributesA("ZRDXD\\A.TXT", 0x80));
    CHECK(!(GetFileAttributesA("ZRDXD\\A.TXT") & 1));
    CHECK(MoveFileA("ZRDXD\\A.TXT", "ZRDXD\\B.TXT"));
    CHECK(GetFileAttributesA("ZRDXD\\A.TXT") == 0xFFFFFFFFUL);
    put("ZRDXD\\A.TXT", "x", 1);
    CHECK(!MoveFileA("ZRDXD\\A.TXT", "ZRDXD\\B.TXT"));
    CHECK(GetLastError() == 183);
    CHECK(DeleteFileA("ZRDXD\\A.TXT"));
    CHECK(!DeleteFileA("ZRDXD\\A.TXT") && GetLastError() == 2);
    CHECK(CopyFileA("ZRDXD\\B.TXT", "ZRDXD\\C.TXT", 1));
    CHECK(!CopyFileA("ZRDXD\\B.TXT", "ZRDXD\\C.TXT", 1));
    CHECK(CopyFileA("ZRDXD\\B.TXT", "ZRDXD\\C.TXT", 0));
    f = CreateFileA("ZRDXD\\C.TXT", 0xC0000000UL, 0, 0, 3, 0, 0);
    CHECK(f != (HANDLE) - 1L);
    CHECK(GetFileSize(f, &hi) == 5 && hi == 0);
    CHECK(GetFileSizeEx(f, size) && size[0] == 5);
    CHECK(size[1] == 0);
    CHECK(SetFilePointerEx(f, 2, 0, pos, 0) && pos[0] == 2);
    CHECK(SetEndOfFile(f));
    CHECK(GetFileSize(f, 0) == 2);
    CHECK(GetFileTime(f, 0, 0, ft));
    CHECK(FileTimeToSystemTime(ft, back) && back[0] >= 2020);
    st[0] = 2001;
    st[1] = 2;
    st[2] = 0;
    st[3] = 3;
    st[4] = 4;
    st[5] = 5;
    st[6] = 6;
    st[7] = 0;
    CHECK(SystemTimeToFileTime(st, ft));
    CHECK(SetFileTime(f, 0, 0, ft));
    CHECK(CloseHandle(f));
    f = CreateFileA("ZRDXD\\C.TXT", 0x80000000UL, 1, 0, 3, 0, 0);
    CHECK(f != (HANDLE) - 1L);
    CHECK(GetFileTime(f, 0, 0, pos));
    CHECK(FileTimeToSystemTime(pos, back));
    CHECK(back[0] == 2001 && back[1] == 2 && back[3] == 3);
    CHECK(back[4] == 4 && back[5] == 5 && back[6] == 6);
    CHECK(CloseHandle(f));
}

static void find_tests(void)
{
    char fd[318];
    HANDLE h;
    DWORD n;
    int count = 0, i;

    h = FindFirstFileA("ZRDXD\\*.TXT", fd);
    CHECK(h != (HANDLE) - 1L);
    do {
        count++;
        CHECK(fd[44] == 'B' || fd[44] == 'C');
        n = fd[44] == 'B' ? 5UL : 2UL;
        CHECK(*(DWORD *)(fd + 32) == n);
        CHECK(!(*(DWORD *)fd & 0x10));
    } while (FindNextFileA(h, fd));
    CHECK(GetLastError() == 18 && count == 2);
    CHECK(FindClose(h));
    CHECK(!FindClose(h));
    h = FindFirstFileA("ZRDXD\\*.ZZZ", fd);
    CHECK(h == (HANDLE) - 1L);
    CHECK(GetLastError() == 2 || GetLastError() == 18);
    h = FindFirstFileA("ZRDXD", fd);
    CHECK(h != (HANDLE) - 1L && (*(DWORD *)fd & 0x10));
    CHECK(FindClose(h));
    for (i = 0; i < 4; i++)
        fd[i] = 0;
}

static void lfn_tests(void)
{
    char fd[318], full[300];
    HANDLE h;

    put("ZRDXD\\a long file name.text", "lfn", 3);
    CHECK(GetFileAttributesA("ZRDXD\\a long file name.text") !=
          0xFFFFFFFFUL);
    h = FindFirstFileA("ZRDXD\\a long*", fd);
    CHECK(h != (HANDLE) - 1L);
    CHECK(fd[44] == 'a' && fd[45] == ' ');
    CHECK(FindClose(h));
    CHECK(GetFullPathNameA("ZRDXD\\a long file name.text", 300,
                           full, 0) > 20);
    CHECK(CopyFileA("ZRDXD\\a long file name.text",
                    "ZRDXD\\copy of a long name.text", 0));
    CHECK(MoveFileA("ZRDXD\\copy of a long name.text",
                    "ZRDXD\\moved long name.text"));
    CHECK(DeleteFileA("ZRDXD\\moved long name.text"));
    CHECK(DeleteFileA("ZRDXD\\a long file name.text"));
}

static void cleanup_tests(void)
{
    CHECK(!RemoveDirectoryA("ZRDXD"));
    CHECK(DeleteFileA("ZRDXD\\B.TXT"));
    CHECK(DeleteFileA("ZRDXD\\C.TXT"));
    CHECK(RemoveDirectoryA("ZRDXD"));
    CHECK(!RemoveDirectoryA("ZRDXD") && GetLastError() == 3);
}

void mainCRTStartup(void)
{
    dir_tests();
    file_tests();
    find_tests();
    lfn_tests();
    cleanup_tests();
    dos_print("\r\nFS: ok\r\n$");
    ExitProcess(0);
}
