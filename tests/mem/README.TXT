Largest memory block available to one malloc call
=================================================

MEM.C is a Watcom C program. It finds, with a binary search, the largest size
that a single malloc call can return and prints it in megabytes.

Built executables are in this directory:
  memd.exe  - DOS/4GW stub
  memc.exe  - CauseWay stub (the extender is inside cwstub.exe)
  memz.exe  - the same program with the ZRDX stub (zrxbind)

BUILD.BAT rebuilds them (Open Watcom in PATH, zrxbind in ..\..\bin),
RUN.BAT runs them one by one, CLEAN.BAT deletes the build results.

Results in DOSBox-X (MB, one malloc call)

 RAM of the PC    ZRDX    DOS/4GW    CauseWay
      32            30       30        1020
    1024          1021       62        1020
    3000          2995       62        1020

CauseWay uses virtual memory (a swap file), so its value does not depend on
the installed RAM. ZRDX and DOS/4GW count only real memory. DOS/4GW is limited
by about 64 MB.
