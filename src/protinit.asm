;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

;protected init code
Segm Text
;assume cs:dgroup, ds:dgroup, es:nothing, ss:dgroup
assume cs:Text, ds:nothing, es:nothing, ss:nothing
LLabel PMInit
        push Data0Selector
        pop  eax
        mov  ss, eax
        mov  esp, OffKernelStack
        pushfd
        or   byte ptr [esp][1], 30h      ;set protected mode IOPL to 3
        popfd
        mov  al, Data1Selector
        mov  ds, eax
        mov  fs, eax
        mov  gs, eax
        mov  es, eax
        mov  SwitchTableEIP, OffRMS_PMHandler
        RRT  4
        push eax ;Data1Selector
        push OffKernelStack1End  ;OffUserStackEnd;-size DC_Struct; OffLockedStack-800h
        push Code1Selector
        push OffPL3Entry
        mov  eax, cr0
        mov  RMCR0, eax          ;save original CR0 for Real(or VM) Mode
        RRT
        and  eax, not  05002Ch   ;clear AM & WP & TS & EM & NE
        mov  PMCR0, eax
        RRT
        mov  cr0, eax
        retf

DPROC PL3Entry
        push Data3Selector
        push OffUserStackEnd
        ;push eax
        mov  al, NExitPages+NLoaderPages
        sub  al, NExtraRPages
        $ifnot ja
          mov al, 0
        $endif
        cmp  al, NExitPages
        $ifnot jb
          mov al, NExitPages
        $endif
        mov  esi, ((OffLastInit+0FFFh) and not 0FFFh)-4
        mov  ebx, OffPage2+NExitPages*4
        mov  nEntriesInTable[4], 2   ;prevent page table 2 from free
        $do
        dec  al
        mov  ecx, 1024
        $break js
        sub  ebx, 4
        push eax
        call Alloc1Page
        jnc  @@MemErr1

@@MovePage:
        mov  edi, OffFreePageWin+1000h-4
        std
        mov  FreePageEntry, edx
        DB   CallFarCode              ;PageMoveTrap
        DD   0
        DW   PageMoveGateSelector
@@Lo:
        NExitPages = (OffLastInit-KernelBase+4095)/4096
        NLoaderPages = WinSize shr 12
        pop  eax
        $enddo jmp
IFNDEF VMM
        and  byte ptr PageDir[4], not 2;disable client write access to server area
ENDIF
        cld

        @@NExtraRPages equ ecx
        @@Counter      equ ebx
        @@CounterB     equ bl
        movzx @@NExtraRPages, NExtraRPages
        $ifnot jecxz
          mov  esi, OffPage0+4
LLabel PatchPoint5
          mov  edi, Offfplist
          xor  @@Counter, @@Counter
          $do
            cmp  @@Counter, @@NExtraRPages
            $break jae
            lodsd          ;load phisical page address
            cmp  @@Counter, NLoaderPages-1
            $ifnot jb
              cmp  @@Counter, NLoaderPages+NExitPages-1
              jbe  @@L5
            $endif
            and  ah, 0F0h
            mov  al, 67h
            inc  nFreePages
            or   @@Counter, @@Counter
            $ifnot jnz
              mov  FpListEntry, eax
              InvalidateTLB
              xor  eax, eax
              mov  nEntriesInFplist, eax
              jmp  @@L4
            $endif
            inc  nEntriesInFplist ;FreePagesOnDir
@@L4:
            stosd
@@L5:       inc  @@Counter
            $loop jmp
          $enddo
          sub  @@Counter, NLoaderPages+NExitPages + 15   ;!!!!!!!!!!!!!!
          $ifnot jae
            cmp @@Counter, -15
            $ifnot ja
              mov  @@CounterB, -15
            $endif
            $do
              call Alloc1Page
              cld
              jnc  @@MemErr
              xchg eax, edx
              stosd
              inc  nFreePages
              inc  nEntriesInFplist ;FreePagesOnDir
              ;inc  nFreePagesOnDir
              inc  @@Counter
            $enddo jne
          $endif
        $endif
        sti
        ;push OffGDT+40h 20 10
        ;+(InvalidateTLBGateSelector and not 7) 8 10
        ;call MemDump
        IFDEF VMM
        mov  eax, PageDirAliasPte
        mov  PageDir[8], eax
        mov  eax, PageExtinfoTablePte
        mov  PageDir[12], eax
        mov  eax, PageDir[4]
        mov  PageDirAlias[4], eax
        InvalidateTLB
        ;push 10000
        ;push OffGDT
        ;call d_write_page
        mov  edi, Offswap_file_bitmap
        push 800h
        push edi
        push 0
        call alloc_pages
        or   eax, -1
        cld
        mov  ecx, 800h shr 5
        rep  stosd
        ENDIF
        ;hlt
        mov  ebx, ROffILoaderEntry+LS
@@L09:
        xor  eax, eax
        mov  al, (17+1)*8+7  ;data selector
        mov  ds, eax
        push eax
        push WinSize-80h
        mov  al, ((17+3)*8+7) and 0FFh  ;PSP selector
        mov  es, eax
        mov  al, (17*8+7) and 0FFh  ;CS selector
        push eax
        push ebx
        xchg eax, edi
        retf
@@MemErr1:
@@MemErr:
        mov  ebx, ROffPInitErrorE+LS
        mov  si, ROffErrNoDPMIMemoryEM+LS
        jmp  @@L09

DPROC   PageMoveHandler
        mov  ebp, cr3
        mov  cr3, ebp
        rep  movsd
        cmp  ebx, offset Page2Entry
        $ifnot jne
          mov  PageDir[4], edx
          mov  FreePageWin[Page2Index*4], edx
        $else  jmp
          mov  [ebx], edx
        $endif
        cmp  ebx, offset PageDirEntry
        $ifnot jne
          and dx, not 0FFFh
          mov SwitchTableCR3, edx
          RRT
          mov ebp, edx
        $endif
        mov cr3, ebp
        retf
        ENDP

IFNDEF Release
;parameters: linear address, word count, display line
MemDump  PROC NEAR
        pushad
        imul  edi, dword ptr ss:[esp+9*4], 160
        add  edi, 0B8000h
        mov  esi, [esp+11*4]
        mov  ecx, [esp+10*4]
        push ds es
        mov  ax, Data3Selector
        mov  ds, ax
        mov  es, ax
        xor  dl, dl
        xchg dl, PrintToMem
        $do
        push dword ptr ss:[esi]
        add  esi, 4
        push 8
        call PrintNX
        $enddo loop
        mov  PrintToMem, dl
        pop es ds
        popad
        retn 12
        ENDP
DispLog PROC NEAR
        pushfd
        push ebp edi esi edx ecx ebx eax
        push ds es
        cld
        mov ax, Data3Selector
        mov ds, ax
        mov es, ax
        imul edi, LogLine, 160
        mov word ptr [0B8000h+edi], 0E00h+' ';mark prev line off
        inc  LogLine
        cmp  LogLine, 25
        $ifnot jb
          and LogLine, 0
        $endif
        imul edi, LogLine, 160
        add edi, 0B8000h
        mov ax, 0E00h + '*'
        stosw
        push dword ptr ss:[esp+10*4]
        push 4
        call PrintNX                     ;print lo part of EIP
        mov ecx, 8
        lea esi, [esp+8]
        $do
          push dword ptr ss:[esi]
          add  esi, 4
          push 8
          call PrintNX
        $enddo loop
        pop es ds
        pop eax ebx ecx edx esi edi ebp
        popfd
        ret
        ENDP

RegDump PROC NEAR
        pushfd
        push ebp edi esi edx ecx ebx eax
        push esp
        push 8
        push dword ptr ss:[esp+11*4]
        call MemDump
        pop eax ebx ecx edx esi edi ebp
        popfd
        retn 4
        ENDP
ENDIF

ESeg Text

