;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

AllocPages equ alloc_pages
;XAllocPages equ alloc_pages
FreePages  equ free_pages
;XFreePages  equ free_pages
P_W  = 2
P_P  = 1
P_D  = 40h
P_U  = 4
;P_S  =
P_S0 = 0200h
P_S0_LOG2    = 9
P_S_BEGIN    = 4 shl P_S0_LOG2
P_S_MINCOUNT = 2 shl P_S0_LOG2
P_SA = 0E00h
P_A         = 20h
P_A_LOG2    = 5
P_DISCARDED = P_D+P_W
P_LOCKED    = P_P+P_U+P_A+P_D+(1 shl P_S0_LOG2)
P_INVALID   = P_A+P_D
PE_VCPI     = 800h
P_LOCK_COUNT_MASK = 3F8h
P_LOCK_COUNT_INC  = 8

segm Text
assume cs:Text
; we are handle several events:
; 1) page not prezent, but:
;  a) may be loaded from disk
;  b) may be initalized first
; 2) page present but setted to r/o by server, because it not modified
;    sinse last page_ins()
;this is a ring0 code!
DPROC Exception0EHandler
        cmp  esp, OffKernelStack-6*4
        je   InterruptHL
        pushad
        ;push eax ebx ecx edx esi edi
        mov  eax, Data1Selector
        push ds es
        F = 10*4
        @@err_code equ byte ptr ss:[esp+F+4]  ;skip pushad, ds, es, num
        mov  ds, eax
        mov  es, eax
        @@pti equ esi
        @@pi  equ edi
        @@p   equ edx
        @@t   equ ecx
        @@x   equ ebx

        InvalidateTLB
        mov  @@pti, cr2
        mov  @@pi, @@pti
        shr  @@pti, 22
        shr  @@pi, 12
        cmp  @@pi, 1077h
        $ifnot jne
          mov  eax, cr2
          mov  ebx, dword ptr @@err_code
          ;Log
          ;call RegDump
        $endif
        cmp  @@pti, 4
        jb   @@Unhandled
        test PageDir[@@pti*4], P_P ;nEntriesInTable[@@pti*4], 0
        jz   @@Unhandled
        mov  @@p, PageTables[@@pi*4]
        test @@p, P_P
        $ifnot jz
          ;jmp  @@Unhandled
          test @@p, P_D
          jnz  @@Unhandled
          mov  @@t, @@p
          shr  @@p, 12
          or   @@t, P_W+P_D
          mov  @@p, PageExtinfo[@@p*4]
          ;test @@p, P_W
          ;jz   @@Unhandled
          mov  PageTables[@@pi*4], @@t
          push @@p
          call d_fast_free
          jmp  @@restart_page_fault
        $else        ;page is not present
          test @@p, P_D
          $ifnot jnz
            ;jmp @@Unhandled
            ;test @@p, P_W
            ;$ifnot jnz       ;is write permitted ?
            ;  test @@err_code, P_W
            ;  jnz  @@Unhandled
            ;$endif
            call SwitchTo1
            jc   @@Unhandled
            call alloc_page
            jc   @@Unhandled
            mov  al, P_A+P_P+P_W+P_D
            mov  FreePageEntry, eax
            mov  al, 0
            InvalidateTLB
            ;invlpg free_page
            push OffFreePageWin
            mov  @@t, not 0FFFh
            and  @@t, @@p
            push @@t
            call d_read_page
            test @@err_code, P_W
            $ifnot ;jz
              push @@p
              call d_fast_free
              or  eax, P_A+P_D+P_P+P_W+P_U+P_S_BEGIN
            $else jmp
              mov  @@t, eax
              mov  @@x, PE_VCPI
              shr  @@t, 10
              and  @@x, PageExtinfo[@@t]
              or   @@x, @@p
              mov  PageExtinfo[@@t], @@x
              or   eax, P_A+P_P+P_U+P_S_BEGIN
            $endif
            ;mov  PageTables[@@pi*4], eax
          $else jmp    ;page descarded(p or w != 0)
            test  @@p, P_A          ;if page is invalid, A = 1
            jnz  @@Unhandled
            call alloc_page
            jc   @@Unhandled
            or   eax, P_A+P_P+P_W+P_D+P_U+P_S_BEGIN
          $endif
          mov  PageTables[@@pi*4], eax
          call SwitchTo0
        $endif
@@restart_page_fault:
        .486P
        InvalidateTLB
        ;shl  @@pi, 12
        ;invlpg byte ptr ds:[@@pi]
        ;mov  eax, SwitchTableCR3
        ;RRT
        ;mov  cr3, eax
        .386P
        pop es ds
        popad
        ;pop  edi esi edx ecx ebx eax
        add  esp, 8    ;drop exception number and
        iretd

@@Unhandled:
        call SwitchTo0
        push 15
        call RegDump
        mov  eax, cr2
        push eax
        shr  eax, 22
        push PageDir[eax*4]
        mov  eax, cr2
        shr  eax, 12
        test byte ptr [esp], P_P
        $ifnot jz
          push dword ptr PageTables[eax*4]
        $else jmp
          push 0
        $endif
        push esp 6 16
        call MemDump
        add  esp, 3*4
        pop es ds
        ;pop  edi esi edx ecx ebx eax
        popad
        jmp ExceptionWithCodeHL
        ENDP

comment #
u1 page_exception(u3 err_code, u32 cr2)(
  u10 pti = cr2 >> 22;
  u32 t, pe;
  if nEntriesInTable[pti]; (
    u20 pi = cr2>>12;
    p = page_tables[pi];
    if p&P_P; (
      if! p & (P_W|P_D); (
        pe = PageExtinfo[p>>12];
        if pe & PE_W; (
          page_tables[pi] = p|P_W|P_D;
          d_fast_free(pe);
          return 1;
        )
      )
    )else(
      if! p & (P_W|P_D); (
        if p & P_W0 || ! errcode & P_W; (
          t = alloc_page();
          if errcode & P_W; (
            d_fast_free(p);
            t |= P_D|P_P|P_W|P_U|P_S;
          )else(
            PageExtinfo[t>>12] = PageExtinfo[t>>12]&PE_VCPI|p;
            t |= P_P|P_U|P_S;
          )
          page_tables[pi] = t;
          return 1;
        )
      )else if p == P_DISCARDED; (
        t = alloc_page();
        page_tables[pi] = t|P_P|P_W|P_D|P_U|P_S;
        return 1;
      )
    )
  )
)

present:
loop counter:
0 - mapped
1 - locked
2-7 - available for access counter
                                     P_W  P_D PE_W  P_A P_I P_P
- r/o by client request with mirror   0    0    0    x   N   1
- r/o by client request w/o mirror    0    1    N    x   N   1
- clean writable                      0    0    1    x   N   1
- dirty                               1    1    0    x   N   1

- swapped out                         v    0    N    0   0   0
- discarded                           v    1    N    0   0   0
- invalid(uncommitted)                v    1    N    1   0   0
- invalid(not exist in a memory block)0    1    N    1   1   0
#
;------------------------------- Free Page -----------------------------------
;free page, always succeful
free_page PROC   ;page pointer
        push eax ebx
        F = 3*4
        mov  eax, [esp+F][0]
        mov  ebx, nEntriesInFplist
        and  ah, 0F0h
        inc  ebx
        mov  al, P_A+P_D+P_W+P_P
        $ifnot jle
          xchg eax, FpListPte
          InvalidateTLB
          mov  ebx, -1023
          inc  n_fplists
        $endif
        mov  fplist[ebx*4+1023*4], eax
        mov  nEntriesInFplist, ebx
        pop  ebx eax
        retn 4
        ENDP

;------------------------------ Alloc Page ----------------------------------
;allocate one page using free pool, vcpi, swap
;return eax->page, CF -> status
;when called from ring 0, may put ring 0 esp to ebp
alloc_page PROC
        push edx
        cmp  n_fplists, 0
        je @@AllocVCPI
        mov  edx, nEntriesInFplist
        mov  eax, fplist[edx*4+1023*4]
        dec  edx
        cmp  edx, -1024
        $ifnot jg
          dec  n_fplists
          xchg FpListPte, eax
          InvalidateTLB
          xor  edx, edx
        $endif
        mov  nEntriesInFplist, edx
        jmp  @@Success
        $do
          mov  eax, edx
          shr  edx, 22
          cmp  PageExtinfoTable[edx*4], 0
          $ifnot je
            mov  edx, eax
            shr  edx, 12
            mov  PageExtinfo[edx*4], eax
            jmp  @@Success
          $endif
          mov  PageExtinfoTable[edx*4], eax
          InvalidateTLB
      @@AllocVCPI:
          cmp  Seed, 512+15
          ja   @@rrrr
          ;inc  Seed
          call Alloc1Page ;alloc page with vcpi/xms/raw
        $enddo jc        ;if alloc1page succeed
        ;stc
        ;jmp  @@FailRet
        @@rrrr:
        call SwitchTo1
        jc   @@FailRet
        ;sti
        call page_out
        cli
        jc   @@FailRet
@@Success:
        and  eax, not 0FFFh
@@FailRet:
        pop  edx
        retn
        ENDP
;------------------------------ SwitchTo1 -----------------------------------
;switch to ring 1, when at ring 0
;put ring 0 stack frame size to ebp
;interrupts must be disabled
DPROC SwitchTo1
        cmp  LockedMode, 0
        jne  @@FailRet
        push ecx
        mov  ecx, cs
        test cl, 3
        $ifnot jnz
          push esi edi
          mov  edi, KernelStack1
          cmp  word ptr [ecx][-4], Data1Selector  ;exception from ring 1?
          $ifnot jne
            mov edi, [ecx][-8]              ;using ring 1 stack top
          $endif
          mov  esi, esp
          mov  ecx, OffKernelStack
          sub  ecx, esp
          sub  edi, ecx
          mov  ebp, ecx
          shr  ecx, 2
          push Data1Selector
          push edi
          cld
          rep  movsd
          push Code1Selector
          push OffSwitchTo1X
          retf
LLabel SwitchTo1X
          pop  edi esi
        $endif
        clc
        pop ecx
        retn
@@FailRet:
        stc
        retn
        ENDP
;switch to ring 0, when at ring 1
;using ring 0 frame size in ebp
DPROC SwitchTo0
        push ecx
        mov  ecx, cs
        test cl, 3
        $ifnot jz
          push esi edi
          mov  esi, esp
          DB  CallFarCode
          DD  0
          DW  SwitchTo0GateSelector
LLabel SwitchTo00
          add  esp, 16
          mov  ecx, ebp
          sub  esp, ebp
          shr  ecx, 2
          mov  edi, esp
          cld
          rep  movsd
          pop  edi esi
        $endif
        pop  ecx
        retn
        ENDP
;------------------------------- Page Out -----------------------------------
page_out PROC
        push ebx ecx edx
        ;cmp n_unlocked_pages, 2    ;must keep at least 2 unlocked pages
        ;jb  @@Fail
        @@pti  equ ecx
        @@pdi  equ edx
        mov  @@pti, sw_pti
        $do
      @@dir_loop:
          mov  @@pdi, @@pti
          shr  @@pdi, 10
          and  @@pdi, 3FFh
          $ifnot jz
            ;cmp  nEntriesInTable[@@pdi*4], 0
            ;mov  eax, nEntriesInTable[@@pdi*4]
            test  PageDir[@@pdi*4], P_P
            $ifnot jz
              $do
                mov  eax, PageTables[@@pti*4]    ;1
                inc  @@pti                        ;+
                mov  ebx, eax                     ;1
                and  eax, P_A+P_SA                ;+
                cmp  eax, P_S_MINCOUNT            ;1
                $ifnot jbe                        ;+
                  ;Log
                  cmp  eax, P_A+P_SA              ;1
                  $ifnot je                       ;+
                    ;Log
                    and  eax, P_A                 ;1
                    shl  eax, P_S0_LOG2-P_A_LOG2+1;1
                    add  ebx, -P_S0               ;+
                    add  ebx, eax                 ;1
                  $endif                          ;
                  and  ebx, not P_A               ;1
                  ;Log
                  test @@pti, 03FFh               ;+
                  mov  PageTables[@@pti*4-4], ebx;1
                  $loop jnz                       ;+
                  jmp  @@dir_loop
                $else
                  je   @@swap_out                 ;1
                  ;test eax, P_SA
                  ;$ifnot jz
                  ;  Log
                  ;$endif
                  test @@pti, 03FFh               ;1
                  $loop jnz                       ;+
                  jmp  @@dir_loop
                $endif
              $enddo
            $else
              add  @@pti, 400h
              jmp  @@dir_loop
            $endif
          $endif
          mov  @@pti, OffClientPages shr 12
        $enddo jmp
@@swap_out:
        mov  sw_pti, @@pti
        dec  @@pti
        mov  ebx, PageTables[@@pti*4]     ;reload page entry
        test ebx, P_D
        $ifnot jz              ;is page dirty ?
          mov  eax, @@pti
          shl  eax, 12         ;linear address of swapped page
          push eax
          call d_alloc_page    ;allocate new page in swap file
          push eax
          call d_write_page    ;store dirty page to swap file
          mov  dl, P_W
          and  dl, bl          ;set writeble attribute for this page
          or   al, dl
        $else jmp
          ;Log
          mov  eax, ebx        ;load address in swap, saved in extinfo
          shr  eax, 12
          mov  eax, PageExtinfo[eax*4]
          and  eax, not 0FFFh or P_W
        $endif
        mov  PageTables[@@pti*4], eax  ;save address in swap to main page table
        xchg eax, ebx
        clc
        pop  edx ecx ebx
        retn
        ENDP

;la and n must be page aligned
alloc_pages PROC       ;(u32 la, s, m)
        pushad
        @@pi0 equ edx
        @@pi1 equ ebx
        @@nt  equ ecx
        @@t   equ eax
        @@pti equ esi
        F = 9*4
        mov  @@pi0, [esp+F][4]
        add  @@pi0, 0FFFh
        mov  @@pi1, [esp+F][8]    ;s
        add  @@pi1 , @@pi0
        shr  @@pi0, 12
        push @@pi0
        F = F+4
        shr  @@pi1, 12            ;convert la to pi
        $do
          mov  @@t, 3FFh
          mov  @@nt, @@t
          and  @@t, @@pi0
          sub  @@nt, @@t
          mov  @@t, @@pi1
          inc  @@nt
          sub  @@t, @@pi0
          $break jz
          cmp  @@nt, @@t
          $ifnot jb
            mov @@nt, @@t
          $endif
          mov  @@pti, @@pi0
          shr  @@pti, 10
          add  nEntriesInTable[@@pti*4], @@nt
          cmp  nEntriesInTable[@@pti*4], @@nt
          $ifnot jne
            call alloc_page
            jc   @@fail1
            mov  al, 67h
            mov  PageDir[@@pti*4], eax
            mov  PageDirAlias[@@pti*4], eax
            InvalidateTLB
            cmp  @@nt, 1024
            $ifnot je
              mov  eax, P_INVALID
              imul edi, @@pti, 4096
              add  edi, OffPageTables
              push ecx
              mov  ecx, 1024
              rep  stosd
              pop  ecx
              InvalidateTLB
            $endif
          $endif
          ;cmp  @@pi0, 1077h
          ;je   @@Lok
          cmp  byte ptr [esp+F][0], 0
          $ifnot je
            mov  eax, P_DISCARDED
            lea  edi, PageTables[@@pi0*4]
            add  @@pi0, ecx
            cld
            rep  stosd
            InvalidateTLB
            $loop jmp
          $else
            @@Lok:
            $do
              call alloc_page
              jc @@fail
              or   eax, P_LOCKED or P_W
              mov  PageTables[@@pi0*4], eax
              InvalidateTLB
              inc  @@pi0
            $enddo loop
          $endif
        $enddo jmp
        pop  eax
        clc
@@ret:
        InvalidateTLB
        popad
        retn 12
@@fail:
        sub  nEntriesInTable[@@pti*4], @@nt
        $ifnot jne
          push PageDirAlias[@@pti*4]
          mov  PageDir[@@pti*4], 0
          mov  PageDirAlias[@@pti*4], 0
          call free_page
        $endif
@@fail1:push @@pi0
        call free_pages_n
        stc
        jmp  @@ret
        ENDP
comment #
u1 alloc_vpages(u20 la, n)(
  pi0 = la+0xFFF;
  pi1 = pi0+n;
  pi0 >>= 12;
  pi1 >>= 12;
  while; (
    if! nt = min(0x400 - (pi0 & 0x3FF), pi1-pi0); break;
    pti = pi0>>10;
    //if nt != 1024;  disable();
    if! nEntriesInTable[pti]; (
      t = alloc_page();
      PageDirAlias[pti] = PageDir[pti] = t|P_ALL; inv_tlb();
      if mode; (
    )else(
      enable();
    )
    nEntriesInTable[pti] += nt;
    if mode; (
    )else(
      do (
      t = alloc_page();
      if ! t; ( free_pages_n(lpi0, pi0); )
        page_tables[pi0] = t | P_ALL;
      )while --nt;
    )
  )
)
  if !alloc_page_tables(pi0, pi1); return 0;
  while pi0 != pi1; (
    page_tables[pi0++] = PA_L|PA_UA|PA_D|PA_P;  //set all new pages as discarded
  )
  return 1;
)
#
free_pages PROC       ;(u32 la, n; u1 m)
        @@pi0 equ eax
        @@pi1 equ edx
        push @@pi0 @@pi1
        F = 3*4
        mov  @@pi0, [esp+F][4]
        mov  @@pi1, [esp+F][8]
        add  @@pi0, 0FFFh
        add  @@pi1, @@pi0
        shr  @@pi0, 12
        shr  @@pi1, 12
        push @@pi0 @@pi1
        push dword ptr [esp+F+8][0]
        call free_pages_n
        pop  @@pi1 @@pi0
        retn 12
        ENDP

free_pages_n PROC     ;(u20 pi0, pi1; u1 m)
        pushad
        F = 9*4
        @@pi0 equ ebx
        @@pi1 equ edx
        @@nt  equ ecx
        @@t   equ edi
        @@pti equ esi
        @@p   equ eax
        mov  @@pi0, [esp+F][8]
        mov  @@pi1, [esp+F][4]
        $do
          mov  @@t, 3FFh
          mov  @@nt, @@t
          and  @@t, @@pi0
          sub  @@nt, @@t
          mov  @@t, @@pi1
          inc  @@nt
          sub  @@t, @@pi0
          $break jz
          cmp  @@nt, @@t
          $ifnot jb
            mov @@nt, @@t
          $endif
          mov  @@t, @@nt
          mov  @@pti, @@pi0
          shr  @@pti, 10
          $do
            mov  @@p, PageTables[@@pi0*4]
            mov  PageTables[@@pi0*4], P_INVALID
            test @@p, P_SA
            $ifnot jz
              push @@p
              call free_page
            $endif
            test @@p, P_D
            $ifnot jnz
              test @@p, P_P
              $ifnot jz
                shr  @@p, 12
                mov  @@p, PageExtinfo[@@p*4]
              $endif
              push @@p
              call d_fast_free
            $endif
            inc  @@pi0
            dec  @@t
          $enddo jnz
          sub  nEntriesInTable[@@pti*4], @@nt
          $ifnot jne
            push PageDir[@@pti*4]
            call free_page
            mov  PageDir[@@pti*4], 0
            mov  PageDirAlias[@@pti*4], 0
          $endif
          InvalidateTLB
        $enddo jmp
        popad
        retn 12
        ENDP
DPROC ReturnFreePool
        retn
        ENDP
comment #
free_vpages(u32 la, s)(
  lpi0 = pi0 = la>>12;
  pi1 = (la+s)>>12;
  for; pi0 != pi1; pi++; (
  page: locked or unlocked:
    p = PageTables[pi0];
    if p & P_W; (           //is page mirrored ?
      d_fast_free(p>>12);
    )
    if p & P_SA != 0; (      //is page allocated ?
      free_page(p);
    )
    PageTables[pi0] = P_INVALID; //invalidate page
  )
  free_page_entries(lpi0, pi1);
)
#

DPROC d_alloc_page
        push edi ecx
        mov  edi, free_swap_cluster
        shr  edi, 5
        xor  eax, eax
        mov  ecx, swap_file_size
        add  ecx, 31
        shr  ecx, 5
        sub  ecx, edi
        lea  edi, swap_file_bitmap[edi*4]
        cld
        repe scasd
        $ifnot jz
          bsf  eax, dword ptr ds:[edi-4]
          mov  ecx, [edi-4]
          lea  eax, [edi*8+eax-Offswap_file_bitmap*8-4*8]
        $else jmp
        ;extend swap file
          mov  eax, swap_file_size
          add  swap_file_size, 32
        $endif
        mov  free_swap_cluster, eax
        btr  swap_file_bitmap, eax
        shl  eax, 12
        pop  ecx edi
        retn
ENDP
DPROC d_write_page  ;(swap_offset, page_offset)
        pushad
        F = 9*4
        mov  esi, [esp+F][4]
        mov  edi, OffSwapBuffer
        RRT
        mov  ecx, 400h
        cld
        rep  movsd
        mov  ah, 40h     ;dos write
        call d_swap_io
        popad
        retn 8
ENDP
DPROC d_read_page   ;(swap_offset, page_offset)
        pushad
        F = 9*4
        mov  ah, 3Fh
        call d_swap_io
        mov  esi, OffSwapBuffer
        RRT
        mov  edi, [esp+F][4]
        mov  ecx, 400h
        cld
        rep  movsd
        popad
        retn 8
ENDP
DPROC d_swap_io
        push eax
        mov  ax, 4200h
        mov  edx, dword ptr [esp+9*4+2*4][0]   ;load swap index
        shld ecx, edx, 16
        mov  ebx, swap_file_handle
        call dos_io
        pop  eax
        mov  dx, ROffSwapBuffer
        mov  si, seg DGroup16
        mov  cx, 1000h
        call dos_io
        retn
        ENDP
;eax, ebx, ecx, edx - registers transferred to dos
;esi, edi - dos ds, es
DPROC dos_io
        ;push fs gs
        call Dos1Call
        ;pop  gs fs
        retn
ENDP
DPROC d_fast_free
        push eax
        F = 2*4
        mov  eax, [esp+F][0]
        shr  eax, 12
        bts  swap_file_bitmap, eax
        cmp  eax, free_swap_cluster
        $ifnot jae
          mov  free_swap_cluster, eax
        $endif
        pop  eax
        retn 4
ENDP

DPROC Pause1
        pushad
        Log
        mov  ah, 0
        push dword ptr ds:[16h*4]
        call DosPCall
        cmp  al, 1Bh
        $ifnot jnz
          hlt
        $endif
        popad
        retn
        ENDP

;comment ^
DPROC LockPages
DPMIFn 6 0          ;Page lock
        ;Log
        pushad
        call set_page_region
        @@Limit equ esi
        @@pei equ ecx
        @@pee equ eax
        @@pdi equ @@pei
        ;$ifnot jc
          ;push OffPageDir 8 12
          ;call MemDump
          ;hlt

          push ebx
          $do                       ;lock all present pages from area
            mov  @@pdi, ebx
            shr  @@pdi, 10
            cli
            test PageDir[@@pdi*4], P_P
            $ifnot jz
              ;call Pause1
              mov  edx, PageTables[ebx*4]
              ;Log
              test dl, P_P
              $ifnot jz
                test dh, P_SA shr 8         ;physical map page?
                $toendif jz
                mov  @@pei, edx
                shr  @@pei, 12
                mov  @@pee, PageExtinfo[@@pei*4]
                test dh, 6 shr (P_S0_LOG2-8)    ;already locked page ?
                $ifnot jz
                  and  dh, (not P_SA) shr 8
                  or   dh, P_S0 shr 8
                  and  @@pee, not P_LOCK_COUNT_MASK ;set lock counter to zero(one lock)
                $else jmp
                  add  @@pee, P_LOCK_COUNT_INC   ;inc lock counter
                  test @@pee, P_LOCK_COUNT_MASK
                  jz   @@LockError               ;lock counter overflow?
                $endif
                mov  PageExtinfo[@@pei*4], @@pee
                mov  PageTables[ebx*4], edx
                InvalidateTLB
              $endif
            $endif
            @@LockError:
            sti
            inc  ebx
            cmp  ebx, @@Limit
          $enddo jb
          pop  ebx
          ;call Pause1
          push ebx
          $do         ;allocate all not present pages in the area
            mov  @@pdi, ebx
            cli
            shr  @@pdi, 10
            test PageDir[@@pdi*4], P_P
            $ifnot jz
              mov  edx, PageTables[ebx*4]
              test dl, P_P
              $ifnot jnz
                call load_page
                ;jc  @@LockError
                or   ah, P_S0 shr 8
                mov  PageTables[ebx*4], eax
                InvalidateTLB
              $endif
            $endif
            inc  ebx
            sti
            cmp  ebx, @@Limit
          $enddo jb
          pop ebx
        ;$endif
        popad
        FnRet
ENDP
;bx:cx - first LA, si:di - region size
;exit - ebx: index of first page
;esi - index of last page+1
DPROC set_page_region
        mov  esi, [esp+9*4].DPMIFrame_ESI
        ;Log
        shl  ebx, 16
        mov  bx, cx    ;ebx - first la
        shl  esi, 16
        mov  si, di
        lea  esi, [esi+ebx+0FFFh]
        shr  ebx, 12
        cmp  ebx, OffClientPages shr 12
        $ifnot jae
          mov  ebx, OffClientPages shr 12
        $endif
        shr  esi, 12
        cmp  esi, ebx
        $ifnot ja
          mov esi, ebx
          inc esi
        $endif
        ;Log
        retn
        ENDP

;edx - page descriptor
DPROC load_page  ;(pi, pv, wp), return pv and error flag
        push ebx ecx
        @@p  equ edx
        @@t  equ ecx
        @@x  equ ebx
        test @@p, P_D
        $ifnot jnz
          call alloc_page
          jc   @@err
          mov  al, P_A+P_P+P_W+P_D
          mov  FreePageEntry, eax
          mov  al, 0
          InvalidateTLB
          push OffFreePageWin
          mov  @@t, not 0FFFh
          and  @@t, @@p
          push @@t
          call d_read_page
          mov  @@t, eax
          mov  @@x, PE_VCPI
          shr  @@t, 10
          and  @@x, PageExtinfo[@@t]
          or   @@x, @@p
          and  @@x, not P_LOCK_COUNT_MASK
          mov  PageExtinfo[@@t], @@x
          or   eax, P_A+P_P+P_U
        $else jmp    ;page descarded(p or w != 0)
          test @@p, P_A          ;if page is invalid, A = 1
          jnz  @@err
          call alloc_page
          jc   @@err
          or   eax, P_A+P_P+P_W+P_D+P_U
        $endif
        ;clc
@@ret:
        pop  ecx ebx
        retn
@@err:  stc
        jmp @@ret
        ENDP
eseg Text
