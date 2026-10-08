;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

AllocPages equ alloc_pages
;XAllocPages equ alloc_pages
FreePages equ free_pages
;XFreePages  equ free_pages
%assign P_W 2
%assign P_P 1
%assign P_D 40h
%assign P_U 4
;P_S  =
P_S0 equ 0200h
%assign P_S0_LOG2 9
P_S_BEGIN equ 4 << P_S0_LOG2
P_S_MINCOUNT equ 2 << P_S0_LOG2
P_SA equ 0E00h
%assign P_A 20h
P_A_LOG2 equ 5
P_DISCARDED equ P_D + P_W
P_LOCKED equ P_P + P_U + P_A + P_D + (1 << P_S0_LOG2)
P_INVALID equ P_A + P_D
PE_VCPI equ 800h
P_LOCK_COUNT_MASK equ 3F8h
P_LOCK_COUNT_INC equ 8

        SEGM Text
;assume cs:Text
; we are handle several events:
; 1) page not prezent, but:
;  a) may be loaded from disk
;  b) may be initalized first
; 2) page present but setted to r/o by server, because it not modified
;    sinse last page_ins()
;this is a ring0 code!
        DPROC Exception0EHandler
        cmp esp, OffKernelStack - 6 * 4
        je InterruptHL
        pushad
        ;push eax ebx ecx edx esi edi
        mov eax, Data1Selector
        push ds
        push es
%assign F 10 * 4
;        @@err_code equ byte ptr ss:[esp+F+4]  ;skip pushad, ds, es, num
        mov ds, eax
        mov es, eax
%define _pti esi
%define _pi edi
%define _p edx
%define _t ecx
%define _x ebx

        InvalidateTLB
        mov _pti, cr2
        mov _pi, _pti
        shr _pti, 22
        shr _pi, 12
        cmp _pi, 1077h
        _ifnot jne
          mov eax, cr2
          mov ebx, [esp + F + 4]
          ;Log
          ;call RegDump
        _endif
        cmp _pti, 4
        jb Unhandled@40
        test dword [OffPageDir + (_pti * 4)], P_P ;nEntriesInTable[@@pti*4], 0
        jz Unhandled@40
        mov _p, dword [OffPageTables + (_pi * 4)]
        test _p, P_P
        _ifnot jz
          ;jmp  @@Unhandled
          test _p, P_D
          jnz Unhandled@40
          mov _t, _p
          shr _p, 12
          or _t, P_W + P_D
          mov _p, dword [OffPageExtinfo + (_p * 4)]
          ;test @@p, P_W
          ;jz   @@Unhandled
          mov dword [OffPageTables + (_pi * 4)], _t
          push _p
          call d_fast_free
          jmp restart_page_fault@40
        _else        ;page is not present
          test _p, P_D
          _ifnot jnz
            ;jmp @@Unhandled
            ;test @@p, P_W
            ;$ifnot jnz       ;is write permitted ?
            ;  test @@err_code, P_W
            ;  jnz  @@Unhandled
            ;$endif
            call SwitchTo1
            jc Unhandled@40
            call alloc_page
            jc Unhandled@40
            mov al, P_A + P_P + P_W + P_D
            mov [OffPage2 + ((OffFreePageWin - KernelBase) >> 10)], eax
            mov al, 0
            InvalidateTLB
            ;invlpg free_page
            push OffFreePageWin
            mov _t, ~(0FFFh)
            and _t, _p
            push _t
            call d_read_page
            test byte [esp + F + 4], P_W
            _ifnot ;jz
              push _p
              call d_fast_free
              or eax, P_A + P_D + P_P + P_W + P_U + P_S_BEGIN
            _else jmp
              mov _t, eax
              mov _x, PE_VCPI
              shr _t, 10
              and _x, dword [ecx + OffPageExtinfo]
              or _x, _p
              mov dword [ecx + OffPageExtinfo], _x
              or eax, P_A + P_P + P_U + P_S_BEGIN
            _endif
            ;mov  PageTables[@@pi*4], eax
          _else jmp    ;page descarded(p or w != 0)
            test _p, P_A          ;if page is invalid, A = 1
            jnz Unhandled@40
            call alloc_page
            jc Unhandled@40
            or eax, P_A + P_P + P_W + P_D + P_U + P_S_BEGIN
          _endif
          mov [OffPageTables + (_pi * 4)], eax
          call SwitchTo0
        _endif
restart_page_fault@40:
        .486p
        InvalidateTLB
        ;shl  @@pi, 12
        ;invlpg byte ptr ds:[@@pi]
        ;mov  eax, SwitchTableCR3
        ;RRT
        ;mov  cr3, eax
cpu 386
        pop es
        pop ds
        popad
        ;pop  edi esi edx ecx ebx eax
        add esp, 8    ;drop exception number and
        iretd

Unhandled@40:
        call SwitchTo0
        push 15
        call RegDump
        mov eax, cr2
        push eax
        shr eax, 22
        push dword [eax*4 + OffPageDir]
        mov eax, cr2
        shr eax, 12
        test byte [esp], P_P
        _ifnot jz
          push dword [eax*4 + OffPageTables]
        _else jmp
          push 0
        _endif
        push esp
        push 6
        call MemDump
        add esp, 3 * 4
        pop es
        pop ds
        ;pop  edi esi edx ecx ebx eax
        popad
        jmp ExceptionWithCodeHL


;------------------------------- Free Page -----------------------------------
;free page, always succeful
free_page:
        push eax
        push ebx
%assign F 3 * 4
        mov eax, [esp + F + 0]
        mov ebx, [OffnEntriesInFplist]
        and ah, 0F0h
        inc ebx
        mov al, P_A + P_D + P_W + P_P
        _ifnot jle
          xchg eax, [OffPage2 + ((Offfplist - KernelBase) >> 10)]
          InvalidateTLB
          mov ebx, -(1023)
          inc dword [Offn_fplists]
        _endif
        mov [ebx*4 + Offfplist + (1023 * 4)], eax
        mov [OffnEntriesInFplist], ebx
        pop ebx
        pop eax
        ret 4


;------------------------------ Alloc Page ----------------------------------
;allocate one page using free pool, vcpi, swap
;return eax->page, CF -> status
;when called from ring 0, may put ring 0 esp to ebp
alloc_page:
        push edx
        cmp dword [Offn_fplists], 0
        je AllocVCPI@44
        mov edx, [OffnEntriesInFplist]
        mov eax, [edx*4 + Offfplist + (1023 * 4)]
        dec edx
        cmp edx, -(1024)
        _ifnot jg
          dec dword [Offn_fplists]
          xchg [OffPage2 + ((Offfplist - KernelBase) >> 10)], eax
          InvalidateTLB
          xor edx, edx
        _endif
        mov [OffnEntriesInFplist], edx
        jmp Success@44
        _do
          mov eax, edx
          shr edx, 22
          cmp dword [edx*4 + OffPageExtinfoTable], 0
          _ifnot je
            mov edx, eax
            shr edx, 12
            mov [edx*4 + OffPageExtinfo], eax
            jmp Success@44
          _endif
          mov [edx*4 + OffPageExtinfoTable], eax
          InvalidateTLB
AllocVCPI@44:
          cmp dword [OffSeed], 512 + 15
          ja rrrr@44
          ;inc  Seed
          call Alloc1Page ;alloc page with vcpi/xms/raw
        _enddo jc        ;if alloc1page succeed
        ;stc
        ;jmp  @@FailRet
rrrr@44:
        call SwitchTo1
        jc FailRet@44
        ;sti
        call page_out
        cli
        jc FailRet@44
Success@44:
        and eax, ~(0FFFh)
FailRet@44:
        pop edx
        ret

;------------------------------ SwitchTo1 -----------------------------------
;switch to ring 1, when at ring 0
;put ring 0 stack frame size to ebp
;interrupts must be disabled
        DPROC SwitchTo1
        cmp byte [OffLockedMode], 0
        jne FailRet@46
        push ecx
        mov ecx, cs
        test cl, 3
        _ifnot jnz
          push esi
          push edi
          mov edi, [OffTSS + TSS_ESP1]
          cmp word [ecx - 4], Data1Selector  ;exception from ring 1?
          _ifnot jne
            mov edi, [ecx - 8]              ;using ring 1 stack top
          _endif
          mov esi, esp
          mov ecx, OffKernelStack
          sub ecx, esp
          sub edi, ecx
          mov ebp, ecx
          shr ecx, 2
          push Data1Selector
          push edi
          cld
          rep movsd
          push Code1Selector
          push OffSwitchTo1X
          retf
        LLabel SwitchTo1X
          pop edi
          pop esi
        _endif
        clc
        pop ecx
        ret
FailRet@46:
        stc
        ret

;switch to ring 0, when at ring 1
;using ring 0 frame size in ebp
        DPROC SwitchTo0
        push ecx
        mov ecx, cs
        test cl, 3
        _ifnot jz
          push esi
          push edi
          mov esi, esp
          db CallFarCode
          dd 0
          dw SwitchTo0GateSelector
        LLabel SwitchTo00
          add esp, 16
          mov ecx, ebp
          sub esp, ebp
          shr ecx, 2
          mov edi, esp
          cld
          rep movsd
          pop edi
          pop esi
        _endif
        pop ecx
        ret

;------------------------------- Page Out -----------------------------------
page_out:
        push ebx
        push ecx
        push edx
        ;cmp n_unlocked_pages, 2    ;must keep at least 2 unlocked pages
        ;jb  @@Fail
%define _pti ecx
%define _pdi edx
        mov _pti, dword [Offsw_pti]
        _do
dir_loop@50:
          mov _pdi, _pti
          shr _pdi, 10
          and _pdi, 3FFh
          _ifnot jz
            ;cmp  nEntriesInTable[@@pdi*4], 0
            ;mov  eax, nEntriesInTable[@@pdi*4]
            test dword [OffPageDir + (_pdi * 4)], P_P
            _ifnot jz
              _do
                mov eax, [OffPageTables + (_pti * 4)]    ;1
                inc _pti                        ;+
                mov ebx, eax                     ;1
                and eax, P_A + P_SA                ;+
                cmp eax, P_S_MINCOUNT            ;1
                _ifnot jbe                        ;+
                  ;Log
                  cmp eax, P_A + P_SA              ;1
                  _ifnot je                       ;+
                    ;Log
                    and eax, P_A                 ;1
                    shl eax, P_S0_LOG2 - P_A_LOG2 + 1 ;1
                    add ebx, -(P_S0)               ;+
                    add ebx, eax                 ;1
                  _endif                          ;
                  and ebx, ~(P_A)               ;1
                  ;Log
                  test _pti, 03FFh               ;+
                  mov [OffPageTables + (_pti * 4) - 4], ebx ;1
                  _loop jnz                       ;+
                  jmp dir_loop@50
                _else
                  je swap_out@50                 ;1
                  ;test eax, P_SA
                  ;$ifnot jz
                  ;  Log
                  ;$endif
                  test _pti, 03FFh               ;1
                  _loop jnz                       ;+
                  jmp dir_loop@50
                _endif
              _enddo
            _else
              add _pti, 400h
              jmp dir_loop@50
            _endif
          _endif
          mov _pti, OffClientPages >> 12
        _enddo jmp
swap_out@50:
        mov dword [Offsw_pti], _pti
        dec _pti
        mov ebx, [OffPageTables + (_pti * 4)]     ;reload page entry
        test ebx, P_D
        _ifnot jz              ;is page dirty ?
          mov eax, _pti
          shl eax, 12         ;linear address of swapped page
          push eax
          call d_alloc_page    ;allocate new page in swap file
          push eax
          call d_write_page    ;store dirty page to swap file
          mov dl, P_W
          and dl, bl          ;set writeble attribute for this page
          or al, dl
        _else jmp
          ;Log
          mov eax, ebx        ;load address in swap, saved in extinfo
          shr eax, 12
          mov eax, [eax*4 + OffPageExtinfo]
          and eax, ~(0FFFh) | P_W
        _endif
        mov [OffPageTables + (_pti * 4)], eax  ;save address in swap to main page table
        xchg eax, ebx
        clc
        pop edx
        pop ecx
        pop ebx
        ret


;la and n must be page aligned
alloc_pages:
        pushad
%define _pi0 edx
%define _pi1 ebx
%define _nt ecx
%define _t eax
%define _pti esi
%assign F 9 * 4
        mov _pi0, [esp + F + 4]
        add _pi0, 0FFFh
        mov _pi1, [esp + F + 8]    ;s
        add _pi1, _pi0
        shr _pi0, 12
        push _pi0
%assign F F + 4
        shr _pi1, 12            ;convert la to pi
        _do
          mov _t, 3FFh
          mov _nt, _t
          and _t, _pi0
          sub _nt, _t
          mov _t, _pi1
          inc _nt
          sub _t, _pi0
          _break jz
          cmp _nt, _t
          _ifnot jb
            mov _nt, _t
          _endif
          mov _pti, _pi0
          shr _pti, 10
          add dword [OffnEntriesInTable + (_pti * 4)], _nt
          cmp dword [OffnEntriesInTable + (_pti * 4)], _nt
          _ifnot jne
            call alloc_page
            jc fail1@52
            mov al, 67h
            mov [OffPageDir + (_pti * 4)], eax
            mov [OffPageDirAlias + (_pti * 4)], eax
            InvalidateTLB
            cmp _nt, 1024
            _ifnot je
              mov eax, P_INVALID
              imul edi, _pti, 4096
              add edi, OffPageTables
              push ecx
              mov ecx, 1024
              rep stosd
              pop ecx
              InvalidateTLB
            _endif
          _endif
          ;cmp  @@pi0, 1077h
          ;je   @@Lok
          cmp byte [esp + F + 0], 0
          _ifnot je
            mov eax, P_DISCARDED
            lea edi, [OffPageTables + (_pi0 * 4)]
            add _pi0, ecx
            cld
            rep stosd
            InvalidateTLB
            _loop jmp
          _else
Lok@52:
            _do
              call alloc_page
              jc Fail@52
              or eax, P_LOCKED | P_W
              mov [OffPageTables + (_pi0 * 4)], eax
              InvalidateTLB
              inc _pi0
            _enddo loop
          _endif
        _enddo jmp
        pop eax
        clc
ret@52:
        InvalidateTLB
        popad
        ret 12
Fail@52:
        sub dword [OffnEntriesInTable + (_pti * 4)], _nt
        _ifnot jne
          push dword [OffPageDirAlias + (_pti * 4)]
          mov dword [OffPageDir + (_pti * 4)], 0
          mov dword [OffPageDirAlias + (_pti * 4)], 0
          call free_page
        _endif
fail1@52:
        push _pi0
        call free_pages_n
        stc
        jmp ret@52

free_pages:
%define _pi0 eax
%define _pi1 edx
        push _pi0
        push _pi1
%assign F 3 * 4
        mov _pi0, [esp + F + 4]
        mov _pi1, [esp + F + 8]
        add _pi0, 0FFFh
        add _pi1, _pi0
        shr _pi0, 12
        shr _pi1, 12
        push _pi0
        push _pi1
        push dword [esp + F + 8 + 0]
        call free_pages_n
        pop _pi1
        pop _pi0
        ret 12


free_pages_n:
        pushad
%assign F 9 * 4
%define _pi0 ebx
%define _pi1 edx
%define _nt ecx
%define _t edi
%define _pti esi
%define _p eax
        mov _pi0, [esp + F + 8]
        mov _pi1, [esp + F + 4]
        _do
          mov _t, 3FFh
          mov _nt, _t
          and _t, _pi0
          sub _nt, _t
          mov _t, _pi1
          inc _nt
          sub _t, _pi0
          _break jz
          cmp _nt, _t
          _ifnot jb
            mov _nt, _t
          _endif
          mov _t, _nt
          mov _pti, _pi0
          shr _pti, 10
          _do
            mov _p, dword [OffPageTables + (_pi0 * 4)]
            mov dword [OffPageTables + (_pi0 * 4)], P_INVALID
            test _p, P_SA
            _ifnot jz
              push _p
              call free_page
            _endif
            test _p, P_D
            _ifnot jnz
              test _p, P_P
              _ifnot jz
                shr _p, 12
                mov _p, dword [OffPageExtinfo + (_p * 4)]
              _endif
              push _p
              call d_fast_free
            _endif
            inc _pi0
            dec _t
          _enddo jnz
          sub dword [OffnEntriesInTable + (_pti * 4)], _nt
          _ifnot jne
            push dword [OffPageDir + (_pti * 4)]
            call free_page
            mov dword [OffPageDir + (_pti * 4)], 0
            mov dword [OffPageDirAlias + (_pti * 4)], 0
          _endif
          InvalidateTLB
        _enddo jmp
        popad
        ret 12

        DPROC ReturnFreePool
        ret


        DPROC d_alloc_page
        push edi
        push ecx
        mov edi, [Offfree_swap_cluster]
        shr edi, 5
        xor eax, eax
        mov ecx, [Offswap_file_size]
        add ecx, 31
        shr ecx, 5
        sub ecx, edi
        lea edi, [edi*4 + Offswap_file_bitmap]
        cld
        repe scasd
        _ifnot jz
          bsf eax, [edi - 4]
          mov ecx, [edi - 4]
          lea eax, [edi*8 + eax - (Offswap_file_bitmap * 8) - (4 * 8)]
        _else jmp
        ;extend swap file
          mov eax, [Offswap_file_size]
          add dword [Offswap_file_size], 32
        _endif
        mov [Offfree_swap_cluster], eax
        btr dword [Offswap_file_bitmap], eax
        shl eax, 12
        pop ecx
        pop edi
        ret

        DPROC d_write_page  ;(swap_offset, page_offset)
        pushad
%assign F 9 * 4
        mov esi, [esp + F + 4]
        mov edi, OffSwapBuffer
        RRT
        mov ecx, 400h
        cld
        rep movsd
        mov ah, 40h     ;dos write
        call d_swap_io
        popad
        ret 8

        DPROC d_read_page   ;(swap_offset, page_offset)
        pushad
%assign F 9 * 4
        mov ah, 3Fh
        call d_swap_io
        mov esi, OffSwapBuffer
        RRT
        mov edi, [esp + F + 4]
        mov ecx, 400h
        cld
        rep movsd
        popad
        ret 8

        DPROC d_swap_io
        push eax
        mov ax, 4200h
        mov edx, [esp + (9 * 4) + (2 * 4) + 0]   ;load swap index
        shld ecx, edx, 16
        mov ebx, [Offswap_file_handle]
        call dos_io
        pop eax
        mov dx, ROffSwapBuffer
        mov si, DGROUP16
        mov cx, 1000h
        call dos_io
        ret

;eax, ebx, ecx, edx - registers transferred to dos
;esi, edi - dos ds, es
        DPROC dos_io
        ;push fs gs
        call Dos1Call
        ;pop  gs fs
        ret

        DPROC d_fast_free
        push eax
%assign F 2 * 4
        mov eax, [esp + F + 0]
        shr eax, 12
        bts dword [Offswap_file_bitmap], eax
        cmp eax, [Offfree_swap_cluster]
        _ifnot jae
          mov [Offfree_swap_cluster], eax
        _endif
        pop eax
        ret 4


        DPROC Pause1
        pushad
        Log
        mov ah, 0
        push dword [(16h * 4)]
        call DosPCall
        cmp al, 1Bh
        _ifnot jnz
          hlt
        _endif
        popad
        ret


;comment ^
        DPROC LockPages
        DPMIFn 6, 0          ;Page lock
        ;Log
        pushad
        call set_page_region
%define _limit esi
%define _pei ecx
%define _pee eax
%define _pdi ecx
        ;$ifnot jc
          ;push OffPageDir 8 12
          ;call MemDump
          ;hlt

          push ebx
          _do                       ;lock all present pages from area
            mov _pdi, ebx
            shr _pdi, 10
            cli
            test dword [OffPageDir + (_pdi * 4)], P_P
            _ifnot jz
              ;call Pause1
              mov edx, [ebx*4 + OffPageTables]
              ;Log
              test dl, P_P
              _ifnot jz
                test dh, P_SA >> 8         ;physical map page?
                _toendif jz
                mov _pei, edx
                shr _pei, 12
                mov _pee, dword [OffPageExtinfo + (_pei * 4)]
                test dh, 6 >> (P_S0_LOG2 - 8)    ;already locked page ?
                _ifnot jz
                  and dh, (~(P_SA)) >> 8
                  or dh, P_S0 >> 8
                  and _pee, ~(P_LOCK_COUNT_MASK) ;set lock counter to zero(one lock)
                _else jmp
                  add _pee, P_LOCK_COUNT_INC   ;inc lock counter
                  test _pee, P_LOCK_COUNT_MASK
                  jz LockError@74               ;lock counter overflow?
                _endif
                mov dword [OffPageExtinfo + (_pei * 4)], _pee
                mov [ebx*4 + OffPageTables], edx
                InvalidateTLB
              _endif
            _endif
LockError@74:
            sti
            inc ebx
            cmp ebx, _limit
          _enddo jb
          pop ebx
          ;call Pause1
          push ebx
          _do         ;allocate all not present pages in the area
            mov _pdi, ebx
            cli
            shr _pdi, 10
            test dword [OffPageDir + (_pdi * 4)], P_P
            _ifnot jz
              mov edx, [ebx*4 + OffPageTables]
              test dl, P_P
              _ifnot jnz
                call load_page
                ;jc  @@LockError
                or ah, P_S0 >> 8
                mov [ebx*4 + OffPageTables], eax
                InvalidateTLB
              _endif
            _endif
            inc ebx
            sti
            cmp ebx, _limit
          _enddo jb
          pop ebx
        ;$endif
        popad
        FnRet

;bx:cx - first LA, si:di - region size
;exit - ebx: index of first page
;esi - index of last page+1
        DPROC set_page_region
        mov esi, [esp + (9 * 4) + DPMIFrame_ESI]
        ;Log
        shl ebx, 16
        mov bx, cx    ;ebx - first la
        shl esi, 16
        mov si, di
        lea esi, [esi + ebx + 0FFFh]
        shr ebx, 12
        cmp ebx, OffClientPages >> 12
        _ifnot jae
          mov ebx, OffClientPages >> 12
        _endif
        shr esi, 12
        cmp esi, ebx
        _ifnot ja
          mov esi, ebx
          inc esi
        _endif
        ;Log
        ret


;edx - page descriptor
        DPROC load_page  ;(pi, pv, wp), return pv and error flag
        push ebx
        push ecx
%define _p edx
%define _t ecx
%define _x ebx
        test _p, P_D
        _ifnot jnz
          call alloc_page
          jc _err
          mov al, P_A + P_P + P_W + P_D
          mov [OffPage2 + ((OffFreePageWin - KernelBase) >> 10)], eax
          mov al, 0
          InvalidateTLB
          push OffFreePageWin
          mov _t, ~(0FFFh)
          and _t, _p
          push _t
          call d_read_page
          mov _t, eax
          mov _x, PE_VCPI
          shr _t, 10
          and _x, dword [ecx + OffPageExtinfo]
          or _x, _p
          and _x, ~(P_LOCK_COUNT_MASK)
          mov dword [ecx + OffPageExtinfo], _x
          or eax, P_A + P_P + P_U
        _else jmp    ;page descarded(p or w != 0)
          test _p, P_A          ;if page is invalid, A = 1
          jnz _err
          call alloc_page
          jc _err
          or eax, P_A + P_P + P_W + P_D + P_U
        _endif
        ;clc
ret@78:
        pop ecx
        pop ebx
        ret
Err@78:
        stc
        jmp ret@78

        ESEG Text
