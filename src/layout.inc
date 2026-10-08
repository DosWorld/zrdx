;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda
;NASM version

;layout of the virtual segments, must precede the code

%assign MaxSystemSwitchCode 5 * 4
%assign nMaxCallbacks 20
        VSegm BSS16
        DFB FirstSwitchCode, MaxSystemSwitchCode + nMaxCallbacks * 4
RMIretSwitchCode equ 0 + ROffFirstSwitchCode
Dos1CallRetSwitchCode equ 4 + ROffFirstSwitchCode
RMIERetSwitchCode equ 8 + ROffFirstSwitchCode
TerminateSwitchCode equ 12 + ROffFirstSwitchCode
RawSwitchCode equ 16 + ROffFirstSwitchCode
FirstPassupSwitchCode equ MaxSystemSwitchCode + nMaxCallbacks * 4
        DFB RStackStart, 600
        VSAlign 16
        DFL ExitStackEnd
        DFL RStackEnd
        DFL SwapBuffer
        EVSeg BSS16

        VSegm EBSS
        DFD OldVectors, 10
DTASize equ 40h
        DFB DefaultDTA, 44 ;43          ;DTA, when client started
        DFB MouseCallbackStruct, 34h    ;DC structure for mouse callback
        DFD MouseHandlerESP             ;saved ESP for mouse handler
        DFD ImageBaseAddr
        EVSeg EBSS

%macro DefTrap3 3
%ifndef Trap3Count
        db (100h-(%2)) & 0FFh
        dw (Off %+ %1 %+ TrapH) & 0FFFFh
        db %3
        zsetv zcat3(Off,%1,Trap3), Trap3Pos*4
%endif
        %assign NTraps3 NTraps3+1
        %assign Trap3Pos Trap3Pos+(%2)
%endmacro
%assign Trap3Pos 0
%assign NTraps3 0
%define Trap3Count
%include "traps.inc"
%undef Trap3Count
%assign NTrapsP3 Trap3Pos
NTraps3c equ NTraps3

        VSegm BSS
%ifdef VMM
        DFD swap_file_size      ;current size of the swap file
        DFD free_swap_cluster   ;index of possible first free cluster in the swap file
%endif
        DFD n_fplists
        DFD Exception0DStack
        DFD TotalPagesCount
        DFB FirstTrap3, NTrapsP3 * 4 + 8 ;area for default interrupts, exceptions, etc handlers
        DFB HIntHandlers, 32 * 7
        DFD SavedRealVectors, 256
        DFD LDTFree, 256
        DFD LDTDOS, 256
        DFD nFreePages
%ifndef Release
        DFD ExtraDW
%endif
        DFD FMemRover
;DFD VFreePagesCount
        DFW IDT, 1024
        DFB TSS, 6Ch         ;TSS without IO MAP
        DFD ClientIDT, 512
        DFD ClientExc, 64
;DFD FirstEIP
;DFD FirstCS
;DFD FirstFlags
;DFD FirstESP
;DFD FirstSS
%assign N000 CBTStruct_size * nMaxCallbacks
        DFB CallbacksTable, N000

;KernelStack1 EQU TSS.TSS_ESP1
        DFD RIntFlags, 8    ;bit vector :1 - jump to saved real vector, else - to current

        DFD UserStackStart, 100
        DFL UserStackEnd
        DFB KernelStack1Start, 512
        DFL KernelStack1End
        DFB KernelStackStart, 200h
        DFL KernelStack
%ifdef VMM
  DFD SavedPMStack, 2
  DFB LockedStack, 1000h
  DFL LockedStackBottom
%endif
%ifndef Release
%assign Vl 500 / 10
        DFD TVector, Vl      ;for test only
%endif
        DFB Aborted
        DFB PrintToMem
%assign PageReserved1Count 1000h - ((LC) & 0FFFh)
%ifndef Release
;DFD  APages, 1024
%endif
        DFB PageReserved1, PageReserved1Count
;all Traps3 must be in separate page, with read only attribute,
;in special 32-bit segment with nonzero base
;packed traps format:
;  DB 0EA   ;call far inst            <-  Current call
;  DW PrevGateSelector                <-I
;  DB Reserved or Trap ID               I Current IP - ignored by CPU
;  DB 0EA   ;next trap call far inst  <-I
;  DW CurGateSelector                 <-  Current CS

        DFD PageDir, 1024    ;Directory page
        DFD Page2, 1024     ;Kernel page table page
LockedStackSize equ 1000h
        DFL FirstZeroInit
;DFB LockedStackStart, LockedStackSize
;DFL LockedStack
        DFD nEntriesInTable, 1024
%ifdef VMM
        DFD PageDirAlias, 1024      ;page table for direct access to all page tables
        DFD PageExtinfoTable, 1024  ;page table for page extinfo array
%endif
        DFD LDT, (17 + 4) * 2
        DFL LastInit
        DFD LDTNext, 4000h - (17 + 4) * 2
        DFD Page0, 1024     ;VCPI page allocated in the dos memory and cannot be
                      ;relocated
        DFD fplist, 1024      ;current list of free page entries
;PagesDir equ PageDir
        DFD PageTableWin, 1024
        DFD FreePageWin, 1024
        DFD SDir0Win, 1024
        DFD SDir1Win, 1024
;DFD PageDirAlias, 1024
;DFD PageExtinfo_table, 1024
%assign F (800000h - LC) >> 2
%ifdef VMM
%assign F F - 1024 * 32
%endif
        DFD MCBVector, F          ;Must be Last !
%ifdef VMM
        DFD swap_file_bitmap, 1024 * 32
        DFD PageTables, 1024 * 1024
        DFD PageExtinfo, 1024 * 1024
%endif
        DFL ClientPages

;PageDirEntry   = Page2[(OffPageDir-KernelBase) shr 10]
;Page0Entry      = Page2[(OffPage0-KernelBase) shr 10]
;Page2Entry      = Page2[(OffPage2-KernelBase) shr 10]
;FirstZeroInit_entry = Page2[(OffFirstZeroInit-KernelBase) shr 10]
;FpListEntry     = Page2[(OffFpList-KernelBase) shr 10]
;PageTableEntry = Page2[(OffPageTableWin - KernelBase) shr 10]
;FreePageEntry  = Page2[(OffFreePageWin-KernelBase) shr 10]
Page2Index equ (OffPage2 - KernelBase) >> 12
;SDir0Entry      = Page2[(OffSDir0Win-KernelBase) shr 10]
;SDir1Entry      = Page2[(OffSDir1Win-KernelBase) shr 10]
;FpListPte       = Page2[(OffFpList-KernelBase) shr 10]      ;fplist_entry
%ifdef VMM
;PageDirAliasPte= Page2[(OffPageDirAlias-KernelBase) shr 10]      ;fplist_entry
;PageExtinfoTablePte=Page2[(OffPageExtinfoTable-KernelBase) shr 10]      ;fplist_entry
%endif
        EVSeg BSS

%assign SelectorNum 8
%macro DefSelector 1-3 1,0
        zsetv zcat2(%1,Selector), SelectorNum+(%3)
        %assign SelectorNum SelectorNum+8*(%2)
%endmacro
        DefSelector Data0
        DefSelector Data, 1, 1
        DefSelector Data3, 1, 3
        DefSelector Code0
        DefSelector Code, 1, 1
        DefSelector Code3, 1, 3
        DefSelector DPMIEntryGate, 1, 3
        DefSelector BIOSData, 1, 3
        DefSelector VCPICallGate, 1, 1
        DefSelector VCPITrapGate, 1, 3
        DefSelector InvalidateTLBGate, 1, 3
        DefSelector PageMoveGate, 1, 3
        DefSelector LoadLDTGate, 1, 1
%ifdef VMM
  DefSelector SwitchTo0Gate, 1, 1
%endif
        DefSelector Trap3, 1, 3         ;PL3 selector for traps
        DefSelector LockedStack, 1, 3   ;not flat selector for client locked stack
        DefSelector TSS
        DefSelector LDT
        DefSelector VCPI, 3       ;for VCPI server
        DefSelector FirstGate, NTraps3 ;gates

%assign Code1Selector CodeSelector
Data1Selector equ DataSelector
%assign Trap3Pos 0
%assign NTraps3 0
