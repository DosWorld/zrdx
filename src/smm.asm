;             This file is part of the ZRDX 0.50OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

        SEGM Text
;assume cs:Text, ds:Data, ss:Data
        DPROC AllocPages
PageSize equ 1000h
%define _pos edx
%define _size eax
%define _pagesubdirindex ebx
%define _xdir edi
%define _xsize ecx
%define _t esi
%define _xdir1 esi
%define _nfreepagesondir ebp
        pushad
;uses eax, ebx, ecx, edx, esi, edi, ebp
        mov _pos, [esp + 8 + (8 * 4)]
        mov _size, [esp + 12 + (8 * 4)]
        add _pos, PageSize - 1
        add _size, _pos
        shr _pos, 12  ;Calculating number of first page
        shr _size, 12
        cld
        push _pos
        sub _size, _pos ;calculating number of pages
        _ifnot jbe, near
        _do
          cmp byte [esp + 4 + (8 * 4) + 4], 0
          _ifnot je
%ifdef Release
            sti
%endif
            jmp $ + 2
            cli
          _endif
          mov _pagesubdirindex, _pos
          shr _pagesubdirindex, 10
          cmp dword [OffnEntriesInTable + (_pagesubdirindex * 4)], 0
          _ifnot jne
            ;allocate page for table
            lea _xdir, [OffPageDir + (_pagesubdirindex * 4)]
            mov _xsize, 1
            call XAllocPages
            jz Fail@32
          _endif
          mov _t, dword [OffPageDir + (_pagesubdirindex * 4)]
          mov dword [OffPage2 + ((OffPageTableWin - KernelBase) >> 10)], _t
          mov _xdir, _pos
          and _xdir, 3FFh
          mov _xsize, 400h
          sub _xsize, _xdir
          cmp _xsize, _size
          _ifnot jb
            mov _xsize, _size
          _endif
          lea _xdir, [OffPageTableWin + (_xdir * 4)]
          call XAllocPages
          _ifnot jnz
            cmp dword [OffnEntriesInTable + (_pagesubdirindex * 4)], 0
            _ifnot jne
              ;free previsionaly allocated subdirectory, if main allocation failed
              lea _xdir1, [OffPageDir + (_pagesubdirindex * 4)]
              mov _xsize, 1
              call XFreePages
            _endif
Fail@32:
            pop eax
            push dword [esp + 4 + (8 * 4)]
            call FreePagesN
            stc
            jmp ret@32
          _endif
          add dword [OffnEntriesInTable + (_pagesubdirindex * 4)], _xsize
          add _pos, _xsize
          sub _size, _xsize
        _enddo ja
        _endif
        pop eax                   ;add esp, 4
        clc
ret@32:
        popad
        ret 12

;destroy @@XDir(edi), return @@XSize(ecx)
;destroy ebp, esi
XAllocPages:
        InvalidateTLB
        cmp dword [OffnFreePages], 0
        _ifnot je
          mov _nfreepagesondir, dword [OffnEntriesInFplist] ; nFreePagesOnDir
          or _nfreepagesondir, _nfreepagesondir
          _ifnot jz
            cmp _xsize, _nfreepagesondir
            _ifnot jbe
              mov _xsize, _nfreepagesondir
            _endif
            sub _nfreepagesondir, _xsize
            lea esi, [Offfplist + (_nfreepagesondir * 4) + 4]
            push ecx
            rep movsd
            pop ecx
            ;jmp  @@L0
          _else jmp
            mov _t, dword [Offfplist + 0]
            lea _xsize, [ebp + 1]
            xchg dword [OffPage2 + ((Offfplist - KernelBase) >> 10)], _t
            mov [edi], _t
            mov _nfreepagesondir, 1023
            ;@@L0:
          _endif
          mov dword [OffnEntriesInFplist], _nfreepagesondir
          sub dword [OffnFreePages], _xsize
          ;jmp @@Ret2
        _else jmp
;VCPIAlloc:
          push eax
          push edx
          xor _xsize, _xsize
          call Alloc1Page
          _ifnot jnc
            mov [edi], edx
            inc _xsize
          _endif
          pop edx
          pop eax
        _endif
;@@Ret2:
        InvalidateTLB
        or _xsize, _xsize
        ret


;@@Pos, @@Size
        DPROC FreePages
%define _pos eax
%define _size edx
;uses eax, edx
        push eax
        push edx
        mov _pos, [esp + 8 + (2 * 4)]
        mov _size, [esp + 12 + (2 * 4)]
        add _pos, PageSize - 1
        add _size, _pos
        shr _pos, 12
        shr _size, 12
        push dword [esp + 4 + (2 * 4)]
        call FreePagesN
        pop edx
        pop eax
        ret 12

        DPROC FreePagesN
%define _firstpage eax
%define _lastpage edx
%define _xdir esi
%define _xsize ecx
%define _nd edx
%define _subdirindex ebx
%define _t edi
        ;Log
        pushad
        cld
        sub _lastpage, _firstpage  ;@@nd
        _ifnot jbe
        _do
          cmp byte [esp + 4 + (8 * 4)], 0
          _ifnot je
%ifdef Release
            sti
%endif
            jmp $ + 2
            cli
          _endif
          mov _subdirindex, _firstpage
          shr _subdirindex, 10
          mov _xsize, dword [OffPageDir + (_subdirindex * 4)]
          mov dword [OffPage2 + ((OffPageTableWin - KernelBase) >> 10)], _xsize
          ;InvalidateTLB
          mov _xsize, 400h
          mov _xdir, _firstpage
          and _xdir, 3FFh
          sub _xsize, _xdir
          cmp _xsize, _nd
          _ifnot jb
            mov _xsize, _nd
          _endif
          ;Log
          lea _xdir, [OffPageTableWin + (_xdir * 4)]
          call XFreePages
          add _firstpage, _xsize
          sub _nd, _xsize
          sub dword [OffnEntriesInTable + (_subdirindex * 4)], _xsize
          _ifnot jnz
            lea _xdir, [OffPageDir + (_subdirindex * 4)]
            mov _xsize, 1
            call XFreePages
          _endif
          or _nd, _nd
        _enddo jnz
        _endif
        popad
        ret 4
;@@XDir, @@XSize
;destroy @@XDir, edi, eax
XFreePages:
%define _t edi
        InvalidateTLB
        mov _t, 1023
        sub _t, dword [OffnEntriesInFplist]
        _ifnot jz
          cmp _xsize, _t
          _ifnot jb
            mov _xsize, _t
          _endif
          mov _t, dword [OffnEntriesInFplist] ; nFreePagesOnDir
          add dword [OffnEntriesInFplist], _xsize
          lea edi, [Offfplist + (_t * 4) + 4]
          push ecx
          push ecx
          push esi
          rep movsd
          pop edi
          pop ecx
          push eax
          xor eax, eax
          rep stosd
          pop eax
          pop ecx
          InvalidateTLB
        _else jmp
          mov dword [OffnEntriesInFplist], _t    ; @@T already zero
          lea _xsize, [edi + 1]
          xchg _t, [esi]
          xchg dword [OffPage2 + ((Offfplist - KernelBase) >> 10)], _t
          InvalidateTLB
          mov dword [Offfplist + 0], _t
        _endif
        add dword [OffnFreePages], _xsize
        ret

        DPROC ReturnFreePool
        pushad
        mov ecx, [OffPage2 + ((Offfplist - KernelBase) >> 10)]         ;FreePagesRef
        cmp dword [OffnFreePages], 0
        _ifnot jz
        mov ecx, [OffnEntriesInFplist]  ;nFreePagesOnDir
        jcxz L@38
        _do
        _do

        sti
        jmp $ + 2
        cli
        mov edx, [ecx*4 + Offfplist]
        call VCPIFree@38
        dec dword [OffnFreePages]
        _enddo loop
L@38:
        mov edx, [Offfplist + 0]
        xchg [OffPage2 + ((Offfplist - KernelBase) >> 10)], edx
        call VCPIFree@38
        InvalidateTLB
        mov ch, 1024 >> 8
        dec dword [OffnFreePages]
        _enddo loopnz
        _endif
L0@38:
E@38:
E1@38:
        ;mov nFreePagesOnDir, 1023
        popad
        ret

VCPIFree@38:
        test dh, VCPIPageBit
        _ifnot jz
        and dx, 0F000h
        mov ax, 0DE05h
        VCPITrap
        _endif
        ret

        ESEG Text
