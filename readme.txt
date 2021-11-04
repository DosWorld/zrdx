                      Zurenava DOS extender(ZRDX).
             DOS4GW-compatible DOS extender, version 0.50 beta.
       Copyright(C) 1998-1999 by Sergey Belyakov(S.Belyakov@chat.ru).
                    Homepage: http://www.zrdx.da.ru

       Translated to English by berk//xq
               (berk@hotmail.ru  49516372  http://xq.hotmail.ru).

=> LEGAL <=
~~~~~~~~~~~
        ZRDX 0.50 is distributed as freeware and can be freely copied and used in
any purposes, either commercial or non-commercial. There are no restrictions
applied to development and use of software which can be created via modifying
or extending original ZRDX source files or binary code.
        ZRDX is distributed "AS IS". The author does not guarantee fitness for any
particular purpose, and can not be held responsible for any advantages or
misadvantages coming from using or not using this product.
        All rights on original source files are reserved by the author, Sergey
Belyakov.

=> BRIEF OVERVIEW <=
~~~~~~~~~~~~~~~~~~~~
        ZRDX can be used to run programs, designed for DOS4G/W, PMODE/W, DOS32A,
WDOSX via Watcom C++, TMT Pascal, LadSoft C, etc. compilers.
        Application running under ZRDX can access all physical memory (up to 4G),
all DPMI functions (int 31h) are compatible with DPMI 0.9 specification,
except fns 0A00h-0B03h. DOS extender provides translation of all functions,
translated by PMODE/W, except identification fns, and almost all functions,
translated by DOS4G/W. If you are using only standard libraries, these fns
are quietly enough for your programs to run. If you call DOS directly, you
may examine the list of all translated functions in file 'src/extender.asm'.
        You are encouraged to use UPX for packing your executables. It is much more
effective than PMWLITE or any other executables packer, and supports wide
variety of target platforms.

=> HOW TO USE ZRXBIND UTILITY <=
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ZRDX is a stub, which is BIND to start of executable file. It switches your
PC into protected mode and then loads main module. This is exactly the same, as
with DOS4G/W or any other extender programs.
        To bind ZRDX to already linked program, use ZRXBIND with commands -r or -c.
The -r (replace) command replaces old stub with ZRDX and makes a backup copy
(if you didn't specify -n option). The -c (copy) command copies original program
to a new name, binding ZRDX to that new file, and leaving original file
unchanged.
Examples:
        "zrxbind -r -n doom.exe" binds ZRDX to "doom.exe" without backup copy.
        "zrxbind -c doom.exe doomz.exe" creates ZRDX-stubbed file "doomz.exe",
leaving "doom.exe" without changes.

If you want to bind ZRDX to your program right when you link it, you should
extract ZRDX itself from ZRXBIND. To make such, you should run ZRXBIND with
command -w. For example, running "zrxbind -w zrdx.exe" will write stub to
"zrdx.exe" and you will be able to link it by specifying wlink option
'stub=zrdx.exe'.

=> ZRXSETUP AND CONFIGURING YOUR ZRDX <=
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ZRXSETUP is proposed for additionally configuring DOS extender. You can
find out complete switches list by running it with /? command (zrxsetup /?).
        <filename> parameter specifies name of program with ZRDX stub bind into it,
or the name of ZRDX stub file.
        If there were no more switches given, setup program will display current
stub configuration. If you do specify switches, setup will additionally
display, what configuration parameters were changed and writes new values to
stub executable.

Switches are:
/X<hex number> - maximal amount of allocated memory in raw/xms mode.
        Because raw/xms doesn't allow change of memory allocation on the run,
        extender takes all free memory upon start. If your program then runs
        another one, it will fail to get extended memory. So if you are going
        to use 'exec', you should decide how much memory your program will want
        and set up this parameter accordingly.
        In VCPI and DPMI modes this problem doesn't exist, since they use much
        more smart allocation strategies.

/L<hex number> - defines amount of conventional (DOS) memory, that should be
        left free. When initializing DPMI server, all DOS memory minus specified
        value will be added to a DPMI free memory pool. Thus, decreasing this
        parameter leads to increase of total client-available memory.
        If you are going to do 'exec's, its good idea to leave this field as it is
        (0F000h). Otherwise, this value can be set according to amount of DOS
        memory actually used by application (for example, for some buffers).
        Minimal value is 0. Adjusting this parameter close to minimal will allow
        you to execute small programs even if extended memory isn't available at all.

/T<hex number> - defines length of DOS extender exchange buffer.
        Since DOS can work only with low (conventional) memory, and extended
        functions' parameters can be anywhere in memory, there is a special buffer
        that allows transfer of data from extended to DOS and back. Say, while
        reading a file, extender might split one large read into several small
        chunks and after reading each part, copy it into client buffer.
        Therefore, larger buffers makes less reads and speeds up large read
        operations, however reducing amount of free low memory.

/M<1/0> - sequence of detecting protected mode interfaces.
        0 - dpmi/vcpi/xms/raw, 1 - vcpi/dpmi/xms/raw.
        Standard recommends first variant (that is, DPMI has higher priority),
        but practically some DPMI servers are very buggy, or, for example,
        provide VMM capabilities, while its not needed and slows things down.
        Thats why most of DOS extenders (including DOS4G/W) prefer VCPI service
        (when its present, of course).

/B<1/0> - switches startup copyright banner on/off.
        No comments. Banners - must die! :)

        You might have paid attention that there are much less configurable
parameters than in PMODE/W or DOS32A. The reason is simple: ZRDX kernel is
much more able to dynamically configure itself. Here is the list:
 - ZRDX dynamically allocates room for page tables (and low memory too) in
        VCPI mode upon client memory requests. Page tables are allocated in top
        of memory only, therefore leaving addressable memory space up to 4Gb.
 - ZRDX dynamically allocates physical to linear space mapping pages.
        Mapping can also be up to 4Gb (if you have 4Mb free memory).
 - ZRDX dynamically allocates LDT space. There's only one page per LDT (511
        selectors) upon start. If there's need for more selectors, LDTs are grown
        up to 8191 selectors.
 - ZRDX uses exclusive stacks management system and does not require its
        precise setup. One thing you might want to tune up is total size of
        common DOS stack (you need only one of that), but default 512 bytes
        is quietly enough.
 - Another thing you might want to tune is number of real mode callbacks,
   but if you use more than 20, your program won't work under DPMI, because
   most of the DPMI servers limit this value to 20 callbacks.

=> VMM.ASM - WHAT IS THAT AND WHY ITS HERE? <=
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        If you want, you may build experimental version with VMM support (seems
to go away in next versions ?). The necessary steps to carry out:
Comment line 'Release = 1' and uncomment 'VMM = 1' in segdefs.asm.
This system is very incomplete. Page freeing is not supported under VCPI,
so you better run it under RAW/XMS only. Besides, unimplemented page unlocking
function. Work with swap file is also not very correct (you have to reopen it
after each resize and delete upon program exit). VMM speed is not worse than
in DOS4G/W.

