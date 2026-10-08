;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

        SEGM IData16
%assign Trap3Pos 0
%assign NTraps3 0
%assign Trap3Pos 0
%assign NTraps3 0
Traps3SetupTable:
%include "traps.inc"
        ESEG IData16
        SEGM Data
        LByte Exception0DFlag
        db 1
        align 16
        LDWord GDT
        Descr 0, 0, 0                          ;dummy descriptor
        Descr 0, 0FFFFFh, 0CF93h               ;Flat data descriptor
        Descr 0, 0FFFFFh, 0CFB3h               ;Flat data(PL1) descriptor
        Descr 0, 0FFFFFh, 0CFF3h               ;Flat data(PL3) descriptor
        Descr 0, 0FFFFFh, 0CF9Bh               ;Flat code0  descriptor
        Descr 0, 0FFFFFh, 0CFBBh               ;Flat code(PL1) descriptor
        Descr 0, 0FFFFFh, 0CFFBh               ;Flat code(PL3) descriptor
        GDescr OffDPMIIntEntry, Code1Selector, 0E0h + SS_GATE_PROC3
        Descr 400h, 0FFFFh, 0F3h               ;data descriptor for 40h bios area
        LDWord VCPICallDesc
        GDescr OffVCPICallHandler, Code0Selector, 0A0h + SS_GATE_PROC3
        GDescr OffVCPITrapHandler, Code0Selector, 0E0h + SS_GATE_PROC3
        GDescr OffInvalidateTLBHandler, Code0Selector, 0E0h + SS_GATE_PROC3
        GDescr OffPageMoveHandler, Code0Selector, 0E0h + SS_GATE_PROC3
        GDescr OffLoadLDTHandler, Code0Selector, 0E0h + SS_GATE_PROC3
%ifdef VMM
          GDescr OffSwitchTo00, Code0Selector, 0A0h + SS_GATE_PROC3
%endif
        Descr OffFirstTrap3, NTrapsP3 * 4 + 2, 40FBh       ;Trap3 descriptor
        dd 0, 0
        ;Descr OffLockedStackStart, LockedStackSize-1, 40F3h ;Locked stack
        Descr OffTSS, (TSS_DEF_size + 4), (0080h + SS_FREE_TSS3) ;TSS
        LWord LDTLimit
        Descr OffLDT, (17 + 4) * 8 - 1, 0E0H + SS_LDT ;LDT
        ;GDescr ROffL0234, VCPISelector+8, <0A0h+SS_GATE_PROC3>
        Descr 0, 0FFFFFh, 0CF9Bh                ;Flat code0 descriptor for VCPI emulator
        Descr 0, 0FFFFh, 09Bh                   ;cs:16 bit descriptor
        RRT 2
        Descr 0, 0FFFFh, 093h                   ;16 bit data descriptor
        RRT 2
        times NTraps3 * 2 dd 0
        LLabel GDTEnd
        LDWord PassupIntMap
        dw 0FF00h, 1000h, 18h, 0, 0, 0, 0, 0FFh, 0, 0, 0, 0, 0, 0, 0, 0
        LDWord PassupIntPMap
        dw 00000h, 1000h, 18h, 0, 0, 0, 0, 000h, 0, 0, 0, 0, 0, 0, 0, 0
%ifndef Release
        LDWord Seed       ;for test only
        dd 1
        LDWord LogLine
        dd 8
%endif
        LDWord VCPICall
        dd 0 ;inittialized by RSetup
        LDWord VCPICallHi
        dw VCPISelector
        LByte CPUType
        db 0
        LByte XMSBlockNotAllocated ;1 when XMS server is active and XMS block
        db 0 ;not allocated
        LByte VCPIMemAvailable     ;1 when VCPI server is active and
        db 0 ;last page alloc call was succeful
        LByte NExtraRPages
        db 0
%ifdef VMM
        LByte LockedMode
        db 0, 0
        LDWord swap_file_handle
        dd 0
        LDWord sw_pti
        dd OffClientPages >> 12
%else
        db 0, 0 ;padding
%endif
        LDWord TotalVCPIPages
        dd 0
        LByte RootMCB, MCBStruct_size
        dd OffRootMCB
        dd OffRootMCB
        dd -(400000h)
        dd OffClientPages
;        LastMappedPage equ RootMCB.MCB_StartOffset
        LDWord MemRover
        dd OffRootMCB
        LDWord nmemblocks
        dd 1
        LDWord LDTBottom
        dd OffLDT + 17 * 8 + 4 * 8
        LDWord MCBVectorEnd
        dd OffMCBVector
        LDWord nEntriesInFplist     ;nFreePagesOnDir
        dd 1023
        ESEG Data
