;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

Segm IData16
Trap3Pos = 0
NTraps3  = 0
DefTrap3 MACRO Name, N, NParams
        DB (100h-N) and 0FFh
        DW Off&Name&TrapH and 0FFFFh
        DB NParams
        Off&Name&Trap3 = Trap3Pos*4
        NTraps3  = NTraps3+1
        Trap3Pos = Trap3Pos+N
        ENDM
Traps3SetupTable label byte
        DefTrap3 DefInt   100h 0
        DefTrap3 DefaultExc  16   8
        DefTrap3 RetFromExc  1    6
        DefTrap3 Iret2KernelAsinc  1  0
        DefTrap3 PMCallbackIret  1    0
IFDEF VMM
        DefTrap3 Iret2KernelAsincL 1  0
        DefTrap3 PMCallbackIretL 1    0
        DefTrap3 PassupIretL     1    0
        DefTrap3 IretPL          1    0
ENDIF
        DefTrap3 PassupIret      1    0
        DefTrap3 PMRawSwitch     1    0
        DefTrap3 PMSaveState     1    0      ;must be last!
        NTrapsP3 = Trap3Pos
Eseg IData16
segm Data
LByte Exception0DFlag
        DB 1
align 16
LDWord GDT
        Descr 0, 0, 0                          ;dummy descriptor
        Descr 0, 0FFFFFh, 0CF93h               ;Flat data descriptor
        Descr 0, 0FFFFFh, 0CFB3h               ;Flat data(PL1) descriptor
        Descr 0, 0FFFFFh, 0CFF3h               ;Flat data(PL3) descriptor
        Descr 0, 0FFFFFh, 0CF9Bh               ;Flat code0  descriptor
        Descr 0, 0FFFFFh, 0CFBBh               ;Flat code(PL1) descriptor
        Descr 0, 0FFFFFh, 0CFFBh               ;Flat code(PL3) descriptor
        GDescr OffDPMIIntEntry,    Code1Selector, <0E0h+SS_GATE_PROC3>
        Descr 400h, 0FFFFh, 0F3h               ;data descriptor for 40h bios area
LDWord  VCPICallDesc
        GDescr OffVCPICallHandler, Code0Selector, <0A0h+SS_GATE_PROC3>
        GDescr OffVCPITrapHandler, Code0Selector, <0E0h+SS_GATE_PROC3>
        GDescr OffInvalidateTLBHandler, Code0Selector, <0E0h+SS_GATE_PROC3>
        GDescr OffPageMoveHandler, Code0Selector, <0E0h+SS_GATE_PROC3>
        GDescr OffLoadLDTHandler, Code0Selector, <0E0h+SS_GATE_PROC3>
        IFDEF VMM
          GDescr OffSwitchTo00,     Code0Selector, <0A0h+SS_GATE_PROC3>
        ENDIF
        Descr OffFirstTrap3, NTrapsP3*4+2, 40FBh       ;Trap3 descriptor
        DD    0, 0
        ;Descr OffLockedStackStart, LockedStackSize-1, 40F3h ;Locked stack
        Descr OffTSS, <(size TSS_DEF+4)>, <(0080h+SS_FREE_TSS3)> ;TSS
LWord   LDTLimit
        Descr OffLDT, <(17+4)*8-1>, <0E0H+SS_LDT> ;LDT
        ;GDescr ROffL0234, VCPISelector+8, <0A0h+SS_GATE_PROC3>
        Descr 0, 0FFFFFh, 0CF9Bh                ;Flat code0 descriptor for VCPI emulator
        Descr 0, 0FFFFh, 09Bh                   ;cs:16 bit descriptor
        RRT 2
        Descr 0, 0FFFFh, 093h                   ;16 bit data descriptor
        RRT 2
        DD NTraps3*2 dup(?)
LLabel GDTEnd
LDWord  PassupIntMap
        DW 0FF00h,1000h,18h,0,0,0,0,0FFh,0,0,0,0,0,0,0,0
LDWord  PassupIntPMap
        DW 00000h,1000h,18h,0,0,0,0,000h,0,0,0,0,0,0,0,0
ifndef Release
LDWord  Seed       ;for test only
        DD 1
LDWord  LogLine
        DD 8
endif
LDWord VCPICall
        DD ?            ;inittialized by RSetup
LDword VCPICallHi
        DW VCPISelector
LByte CPUType
        DB 0
LByte   XMSBlockNotAllocated ;1 when XMS server is active and XMS block
        DB 0                 ;not allocated
LByte   VCPIMemAvailable     ;1 when VCPI server is active and
        DB 0                 ;last page alloc call was succeful
LByte NExtraRPages
        DB 0
IFDEF VMM
LByte LockedMode
        DB 0,0
LDWord swap_file_handle
        DD 0
LDWord sw_pti
        DD OffClientPages shr 12
ELSE
        DB 0, 0    ;padding
ENDIF
LDWord TotalVCPIPages
        DD 0
LByte   RootMCB, size MCBStruct
        MCBStruct <OffRootMCB, OffRootMCB, -400000h, OffClientPages>
        LastMappedPage equ RootMCB.MCB_StartOffset
LDWord MemRover
        DD OffRootMCB
LDWord nmemblocks
        DD 1
LDWord LDTBottom
        DD OffLDT+17*8+4*8
LDWord MCBVectorEnd
        DD OffMCBVector
LDWord nEntriesInFplist     ;nFreePagesOnDir
        DD 1023
eseg Data
VSegm BSS
IFDEF VMM
DFD swap_file_size      ;current size of the swap file
DFD free_swap_cluster   ;index of possible first free cluster in the swap file
ENDIF
DFD n_fplists
DFD Exception0DStack
DFD TotalPagesCount
DFB FirstTrap3, NTrapsP3*4+8 ;area for default interrupts, exceptions, etc handlers
DFB HIntHandlers, 32*7
DFD SavedRealVectors, 256
DFD LDTFree, 256
DFD LDTDOS, 256
DFD nFreePages
ifndef Release
DFD ExtraDW
endif
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
N000=size CBTStruct*nMaxCallbacks
DFB CallbacksTable, N000

KernelStack1 EQU TSS.TSS_ESP1
DFD RIntFlags, 8    ;bit vector :1 - jump to saved real vector, else - to current

DFD UserStackStart, 100
DFL UserStackEnd
DFB KernelStack1Start, 512
DFL KernelStack1End
DFB KernelStackStart, 200h
DFL KernelStack
IFDEF VMM
  DFD SavedPMStack, 2
  DFB LockedStack, 1000h
  DFL LockedStackBottom
ENDIF
ifndef Release
Vl = 500/10
DFD TVector, Vl      ;for test only
endif
DFB   Aborted
DFB   PrintToMem
PageReserved1Count = 1000h - ((LC) and 0FFFh)
ifndef Release
;DFD  APages, 1024
endif
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
DFD Page2,   1024     ;Kernel page table page
LockedStackSize = 1000h
DFL FirstZeroInit
;DFB LockedStackStart, LockedStackSize
;DFL LockedStack
DFD nEntriesInTable, 1024
IFDEF VMM
DFD PageDirAlias, 1024      ;page table for direct access to all page tables
DFD PageExtinfoTable, 1024  ;page table for page extinfo array
ENDIF
DFD LDT, (17+4)*2
DFL LastInit
DFD LDTNext, 4000h-(17+4)*2
DFD Page0,   1024     ;VCPI page allocated in the dos memory and cannot be
                      ;relocated
DFD fplist, 1024      ;current list of free page entries
PagesDir equ PageDir
DFD PageTableWin, 1024
DFD FreePageWin, 1024
DFD SDir0Win,       1024
DFD SDir1Win,       1024
;DFD PageDirAlias, 1024
;DFD PageExtinfo_table, 1024
F = (800000h-LC) shr 2
IFDEF VMM
  F = F - 1024*32
ENDIF
DFD MCBVector, F          ;Must be Last !
IFDEF VMM
DFD swap_file_bitmap, 1024*32
DFD PageTables, 1024*1024
DFD PageExtinfo, 1024*1024
ENDIF
DFL ClientPages

PageDirEntry   = Page2[(OffPageDir-KernelBase) shr 10]
Page0Entry      = Page2[(OffPage0-KernelBase) shr 10]
Page2Entry      = Page2[(OffPage2-KernelBase) shr 10]
FirstZeroInit_entry = Page2[(OffFirstZeroInit-KernelBase) shr 10]
FpListEntry     = Page2[(OffFpList-KernelBase) shr 10]
PageTableEntry = Page2[(OffPageTableWin - KernelBase) shr 10]
FreePageEntry  = Page2[(OffFreePageWin-KernelBase) shr 10]
Page2Index      = (OffPage2-KernelBase) shr 12
SDir0Entry      = Page2[(OffSDir0Win-KernelBase) shr 10]
SDir1Entry      = Page2[(OffSDir1Win-KernelBase) shr 10]
FpListPte       = Page2[(OffFpList-KernelBase) shr 10]      ;fplist_entry
IFDEF VMM
PageDirAliasPte= Page2[(OffPageDirAlias-KernelBase) shr 10]      ;fplist_entry
PageExtinfoTablePte=Page2[(OffPageExtinfoTable-KernelBase) shr 10]      ;fplist_entry
ENDIF
EVSeg BSS
Segm Data
SelectorNum=8
DefSelector MACRO Name, N, S
        IFNB <S>
          Name&Selector = SelectorNum+S
        else
          Name&Selector = SelectorNum
        ENDIF
        IFB <N>
          SelectorNum   = SelectorNum+8
        ELSE
          SelectorNum   = SelectorNum+8*N
        ENDIF
        ENDM
DefSelector Data0
DefSelector Data,,1
DefSelector Data3,,3
DefSelector Code0
DefSelector Code,,1
DefSelector Code3,,3
DefSelector DPMIEntryGate,,3
DefSelector BIOSData,,3
DefSelector VCPICallGate,,1
DefSelector VCPITrapGate,,3
DefSelector InvalidateTLBGate,,3
DefSelector PageMoveGate,,3
DefSelector LoadLDTGate,,1
IFDEF VMM
  DefSelector SwitchTo0Gate,,1
ENDIF
DefSelector Trap3,,3         ;PL3 selector for traps
DefSelector LockedStack,,3   ;not flat selector for client locked stack
DefSelector TSS
DefSelector LDT
DefSelector VCPI, 3       ;for VCPI server
DefSelector FirstGate, NTraps3 ;gates

Code1Selector = CodeSelector
Data1Selector = DataSelector
eseg Data
