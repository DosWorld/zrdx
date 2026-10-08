;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

;protected init code
        SEGM Text
;assume cs:dgroup, ds:dgroup, es:nothing, ss:dgroup
;assume cs:Text, ds:nothing, es:nothing, ss:nothing
        LLabel PMInit
        push Data0Selector
        pop eax
        mov ss, eax
        mov esp, OffKernelStack
        pushfd
        or byte [esp + 1], 30h      ;set protected mode IOPL to 3
        popfd
        mov al, Data1Selector
        mov ds, eax
        mov fs, eax
        mov gs, eax
        mov es, eax
        mov dword [OffSwitchTableEIP], OffRMS_PMHandler
        RRT 4
        push eax ;Data1Selector
        push OffKernelStack1End  ;OffUserStackEnd;-size DC_Struct; OffLockedStack-800h
        push Code1Selector
        push OffPL3Entry
        mov eax, cr0
        mov [OffRMCR0], eax          ;save original CR0 for Real(or VM) Mode
        RRT
        and eax, ~(05002Ch)   ;clear AM & WP & TS & EM & NE
        mov [OffPMCR0], eax
        RRT
        mov cr0, eax
        retf

        DPROC PL3Entry
        push Data3Selector
        push OffUserStackEnd
        ;push eax
        mov al, NExitPages + NLoaderPages
        sub al, [OffNExtraRPages]
        _ifnot ja
          mov al, 0
        _endif
        cmp al, NExitPages
        _ifnot jb
          mov al, NExitPages
        _endif
        mov esi, ((OffLastInit + 0FFFh) & ~(0FFFh)) - 4
        mov ebx, OffPage2 + NExitPages * 4
        mov dword [OffnEntriesInTable + 4], 2   ;prevent page table 2 from free
        _do
        dec al
        mov ecx, 1024
        _break js
        sub ebx, 4
        push eax
        call Alloc1Page
        jnc near MemErr1@7

MovePage@7:
        mov edi, OffFreePageWin + 1000h - 4
        std
        mov [OffPage2 + ((OffFreePageWin - KernelBase) >> 10)], edx
        db CallFarCode ;PageMoveTrap
        dd 0
        dw PageMoveGateSelector
Lo@7:
NExitPages equ (OffLastInit - KernelBase + 4095) / 4096
NLoaderPages equ WinSize >> 12
        pop eax
        _enddo jmp
%ifndef VMM
        and byte [OffPageDir + 4], ~(2) ;disable client write access to server area
%endif
        cld

%define _nextrarpages ecx
%define _counter ebx
%define _counterb bl
        movzx _nextrarpages, byte [OffNExtraRPages]
        _ifnot jecxz_n, near
          mov esi, OffPage0 + 4
        LLabel PatchPoint5
          mov edi, Offfplist
          xor _counter, _counter
          _do
            cmp _counter, _nextrarpages
            _break jae
            lodsd          ;load phisical page address
            cmp _counter, NLoaderPages - 1
            _ifnot jb
              cmp _counter, NLoaderPages + NExitPages - 1
              jbe L5@7
            _endif
            and ah, 0F0h
            mov al, 67h
            inc dword [OffnFreePages]
            or _counter, _counter
            _ifnot jnz
              mov [OffPage2 + ((Offfplist - KernelBase) >> 10)], eax
              InvalidateTLB
              xor eax, eax
              mov [OffnEntriesInFplist], eax
              jmp L4@7
            _endif
            inc dword [OffnEntriesInFplist] ;FreePagesOnDir
L4@7:
            stosd
L5@7:
        inc _counter
            _loop jmp
          _enddo
          sub _counter, NLoaderPages + NExitPages + 15   ;!!!!!!!!!!!!!!
          _ifnot jae
            cmp _counter, -(15)
            _ifnot ja
              mov _counterb, -(15)
            _endif
            _do
              call Alloc1Page
              cld
              jnc MemErr@7
              xchg eax, edx
              stosd
              inc dword [OffnFreePages]
              inc dword [OffnEntriesInFplist] ;FreePagesOnDir
              ;inc  nFreePagesOnDir
              inc _counter
            _enddo jne
          _endif
        _endif
        sti
        ;push OffGDT+40h 20 10
        ;+(InvalidateTLBGateSelector and not 7) 8 10
        ;call MemDump
%ifdef VMM
        mov eax, [OffPage2 + ((OffPageDirAlias - KernelBase) >> 10)]
        mov [OffPageDir + 8], eax
        mov eax, [OffPage2 + ((OffPageExtinfoTable - KernelBase) >> 10)]
        mov [OffPageDir + 12], eax
        mov eax, [OffPageDir + 4]
        mov [OffPageDirAlias + 4], eax
        InvalidateTLB
        ;push 10000
        ;push OffGDT
        ;call d_write_page
        mov edi, Offswap_file_bitmap
        push 800h
        push edi
        push 0
        call alloc_pages
        or eax, -(1)
        cld
        mov ecx, 800h >> 5
        rep stosd
%endif
        ;hlt
        mov ebx, ROffILoaderEntry + LS
L09@7:
        xor eax, eax
        mov al, (17 + 1) * 8 + 7  ;data selector
        mov ds, eax
        push eax
        push WinSize - 80h
        mov al, ((17 + 3) * 8 + 7) & 0FFh  ;PSP selector
        mov es, eax
        mov al, (17 * 8 + 7) & 0FFh  ;CS selector
        push eax
        push ebx
        xchg eax, edi
        retf
MemErr1@7:
MemErr@7:
        mov ebx, ROffPInitErrorE + LS
        mov si, ROffErrNoDPMIMemoryEM + LS
        jmp L09@7

        DPROC PageMoveHandler
        mov ebp, cr3
        mov cr3, ebp
        rep movsd
        cmp ebx, (OffPage2 + ((OffPage2 - KernelBase) >> 10))
        _ifnot jne
          mov [OffPageDir + 4], edx
          mov [OffFreePageWin + (Page2Index * 4)], edx
        _else jmp
          mov [ebx], edx
        _endif
        cmp ebx, (OffPage2 + ((OffPageDir - KernelBase) >> 10))
        _ifnot jne
          and dx, ~(0FFFh)
          mov [OffSwitchTableCR3], edx
          RRT
          mov ebp, edx
        _endif
        mov cr3, ebp
        retf


%ifndef Release
;parameters: linear address, word count, display line
MemDump:
        pushad
        imul edi, [esp + (9 * 4)], 160
        add edi, 0B8000h
        mov esi, [esp + (11 * 4)]
        mov ecx, [esp + (10 * 4)]
        push ds
        push es
        mov ax, Data3Selector
        mov ds, ax
        mov es, ax
        xor dl, dl
        xchg dl, [OffPrintToMem]
        _do
        push dword [ss:esi]
        add esi, 4
        push 8
        call PrintNX
        _enddo loop
        mov [OffPrintToMem], dl
        pop es
        pop ds
        popad
        ret 12

DispLog:
        pushfd
        push ebp
        push edi
        push esi
        push edx
        push ecx
        push ebx
        push eax
        push ds
        push es
        cld
        mov ax, Data3Selector
        mov ds, ax
        mov es, ax
        imul edi, [OffLogLine], 160
        mov word [edi + 0B8000h], 0E00h + ' ' ;mark prev line off
        inc dword [OffLogLine]
        cmp dword [OffLogLine], 25
        _ifnot jb
          and dword [OffLogLine], 0
        _endif
        imul edi, [OffLogLine], 160
        add edi, 0B8000h
        mov ax, 0E00h + '*'
        stosw
        push dword [esp + (10 * 4)]
        push 4
        call PrintNX                     ;print lo part of EIP
        mov ecx, 8
        lea esi, [esp + 8]
        _do
          push dword [ss:esi]
          add esi, 4
          push 8
          call PrintNX
        _enddo loop
        pop es
        pop ds
        pop eax
        pop ebx
        pop ecx
        pop edx
        pop esi
        pop edi
        pop ebp
        popfd
        ret


RegDump:
        pushfd
        push ebp
        push edi
        push esi
        push edx
        push ecx
        push ebx
        push eax
        push esp
        push 8
        push dword [esp + (11 * 4)]
        call MemDump
        pop eax
        pop ebx
        pop ecx
        pop edx
        pop esi
        pop edi
        pop ebp
        popfd
        ret 4

%endif

        ESEG Text
