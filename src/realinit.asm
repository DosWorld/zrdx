;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

;real mode init routines
Segm IData16
IFDEF VMM
LByte SwapFileName
        DB 'zrdx!.swp',0
LByte SwapFileEM
        DB 'swap I/O error', 0
ENDIF
LByte IntroMsg
        DB 'Zurenava DOS extender, version 0.50. Copyright(C) 1998-1999, Sergey Belyakov', 13, 10, '$'
LByte FirstError
        DB 'ZRDX init error:$'
LByte VersionEM
        DB 'DOS 3.0+ requred$'
LByte EnvironEM
        DB 'bad environment$'
LByte WrongCPUEM
        DB '80386+ CPU not detected$'
LByte FileAccessEM
        DB "can't open EXE$"
LByte AlreadyVMEM
        DB 'already in V86 mode without VCPI or DPMI$'
LByte VCPIServerEM
        DB 'unexpected VCPI host fault$'
LByte DOSMemoryEM
        DB 'DOS memory allocation$'
LByte IntMapEM
        DB 'unsupported hardware interrupts mapping$'
;LByte UnXMSEM
;        DB 'unexpected XMS fault$'
;LByte OutOfXMSEM
;        DB 'out of XMS memory$'
LByte SwitchModeEM
        DB "can't enter protected mode via DPMI$"
LLabel SetVectEM
        DB "Can't set interrupt handler$"
LLabel ErrAllocCBackEM
        DB "Can't allocate realmode callback$"
;LLabel A20EM
;        DB "Can't enable line A20$"
LLabel CRLF
        DB 13, 10,'$'
ESeg IData16
Segm IText16
assume cs:DGROUP16, ds:DGROUP16, ss:DGROUP16
RInit PROC
InitDPMI:
@@TryDPMI:
        mov  ax, 1687h
        int  2Fh
        or   ax, ax
        $ifnot jz
@@NoDPMI:
          ret
        $endif
        shr  bx, 1
        jnc  @@NoDPMI
        PSPSize    = 100h
        ;RMCodeSize = 0 ;SegSizeEText16
        mov  bp, cs
        ;mov  ss, bp
        lea  ax, [bp+si+(ROffLoaderEnd+200h+15)/16+MouseRHandlerPSize]
        lea  bx, [bp+MouseRHandlerPSize]
        lea  bp, [bx+si]
        add  MemBlock0SizeR[PSP], si
        cmp  ax, ds:[2]
        mov  si, OffDOSMemoryEM
        ja   RInitError
        mov  ss, bp
        mov  sp, ROffLoaderEnd+200h
        push bp
        push ROffILoaderEntry0    ;!!!!!!!!!!!!!!
        push es
        push di
        mov  di, ROffLoaderEnd-4
        mov  si, ROffLoaderEnd-4+PSPSize               ;!!!!!!!!!!!!!!
        mov  cx, (ROffLoaderEnd-ROffPermanentPart+3)/4
        mov  es, bp
        std
        rep  movsd
        mov  ds, bp
        mov  word ptr PatchPoint8R[-2], bp
        mov  ax, 1
        mov  es, bx
        mov  si, ROffSwitchModeEM
        cld                 ;workaround for MS NT5(windows 2000) beta 3
        retf
        LS = 0

VCPIRICall:
        int 67h
        ret
VCPIRIEmulator:
        cmp al, 1
        $ifnot jne
          ;fill PageTable
          xor  eax, eax
          mov  al, 67h     ;access rights
          mov  cx, 110h    ;number of real mode pages
          cld
          $do
            stosd
            add eax, 1000h
          $enddo loop
          ;mov  ebx, OffVCPIPMEmulator
          ret
        $endif
        ;cmp al, 0Ah
        ;$ifnot jne
        mov bl, 8
        mov cl, 70h
        ret
        ;$endif
        ;jmp RawSwitcherToPM

@@CriticalInitVCPI:
        mov  dx, ds
        IFNDEF VMM
          add  dx, (ROffRStackEnd+1FFFh+PSPSize)/16
        ELSE
        ;allocate additional space for vmm disk transfer buffer
          add  dx, (ROffRStackEnd+1000h+1FFFh+PSPSize)/16
        ENDIF
        mov  dl, 0              ;page aligment
        mov  cl, 0
        cmp  cx, dx
        $ifnot ja
          mov cx, dx
        $endif
        mov  ax, ds
        sub  ax, cx             ;get para size of dpmi real mode segment
                                ;and vcpi page0
        neg  ax
        mov  MemBlock0SizeR[PSP], ax
        sub  cx, dx
        mov  NExtraRPagesR[PSP], ch
        mov  di, dx
        dec  dh
        mov  es, dx
        push 0FFh
        mov  ax, 0F2h
        push ax         ;
        push ds

        DB   PushWCode
LLabel EnvSize
        DW   0
        push ax
        mov  ax, (17+2)*8+7
        xchg ax, ds:[2Ch]
        push ax
        push -1
        push 40F2h
        push di
        push -1
        push 0FAh
        push di
        mov  PatchPoint8R[PSP][-2], di

        mov  cx, 4
        mov  di, ROffLDT+PSP+17*8
        $do
        xor  eax, eax
        pop  ax
        shl  eax, 4
        mov  ds:[di].2, eax
        pop  word ptr ds:[di].5
        pop  word ptr ds:[di]
        add  di, 8
        $enddo loop
        mov  byte ptr LDTFreeR[16/8][PSP], 11110b ;set bitmask for allocated descriptors

        mov  di, WinSize+1000h+(OffLastInit-KernelBase)-4  ;skip first page and
        mov  si, OffProtectedStart+(OffLastInit-KernelBase)+PSP-4
        std
        xor  eax, eax
        mov  cx, (OffLastInit-KernelBase)/4
        ;move dpmi kernel up to page aligned location
        rep  movsd         ;move server code to new location
        mov  di, WinSize+1000h-4
        mov  si, WinSize+PSP-4
        mov  cx, WinSize/4
        rep  movsd         ;move extender to window
        mov  cx, 400h-1
        rep  stosd         ;clear page0
        cld
        mov  ds, dx
        LSX  = -KernelBase+1000h+WinSize
        LSR  = -OffProtectedStart+1000h+WinSize
        mov  si, OffGDT+VCPISelector+LSX
        mov  ax, 0DE01h
        call VCPIRIcall
        mov  VCPICallR+LSR, ebx

        mov  si, dx
        shr  si, 8-2      ;shift of Page0 in pagetable
        add  ds:PatchPoint5R[-4][LSR], si
        mov  eax, [si][(OffPageDir+LSX)/1024]
        and  ax, 0F000h
        mov  cs:SwitchTableCR3R, eax
        mov  di, OffPageDir+LSX
        call @@MovePageRef
        mov  ds:[0], eax       ;mov Page0Ref[LSX], eax
        org  $-2               ;set reference to page 0 in the page directory
        DW   Page0Entry+LSX
        mov  eax, [si][(OffPage2+LSX)/1024][-4]
        call @@MovePageRef1    ;set reference to page 2 in the page directory

        mov  di, OffPage2+LSX
        add  si, WinSize/1024
        mov  cl, (OffLastInit-KernelBase+0FFFh)/1000h
        $do
        call @@MovePageRef     ;set references to kernel in the page 2 table
        $enddo loop
        jmp  SwitchToPM
        ;mov  esi, ROffSwitchTable
        ;RRT
        ;mov  ax, 0DE0Ch
        ;cli
        ;call VCPIRICall
@@MovePageRef:lodsd
@@MovePageRef1:
        and ah, 0F0h
        mov al, 67h
        stosd
        ret
LLabel  PermanentPart

LLabel  DefaultFlatDescriptor
        ;IFDEF XLE
        ;Descr 0, 0FFFFFh, 0CF9Bh               ;Flat code descriptor
        ;ELSE
        Descr 0, 0FFFFFh, 0CF93h               ;Flat data descriptor
        ;ENDIF
LLabel ILoaderEntry0
        jc   RInitError
LLabel ILoaderEntry
        movzx esp, sp                   ;workaround for windows 3000:)
        mov  SavedPSPR[LS], es          ;save psp to extender
        mov  lSavedPSPR[LS], es         ;save psp to loader
        mov  edi, ROffDefaultFlatDescriptor+LS
        mov  ax, cs
        and  al, 3
        shl  al, 5
        or   [di].5, al   ;set dpl to current cpl
        IFDEF XLE
        call @@AllocAndSetSel
        mov  word ptr FlatSelectorR[LS],  ax
        mov  fs, ax
        or   byte ptr [di].5, 8         ;data to code
        call @@AllocAndSetSel
        mov  word ptr LECodeSelectorR[LS], ax
        ELSE
        xor  ax, ax
        mov  cx, 1
        push ds
        pop  es
        int  31h                         ;allocate selector for flat
        jc   PInitErrorSel
        mov  word ptr FlatSelectorR[LS],  ax      ;save flat selector to extender
        xchg bx, ax
        mov  ax, 0Ch
        int  31h                     ;set flat descriptor
        jc   PInitErrorSel
        mov  fs, bx
        ENDIF
        mov  dx, ExtenderSize/4
        mov  cx, ExtenderFullSize
        mov  bp, ExtenderStart+LS
        call @@AllocAndMove          ;allocate and move extender
        mov  word ptr ExtenderSelR[LS], bx
        mov  bp, bx                 ;save code selector for extender
        mov  ds, ax
        push bx                     ;save code selector on the stack
        push ax                     ;save data selector on the stack
        ;es, ds - selector of the extender
assume es:EGroup
        mov  SelReference0A16[-4], ax
        mov  SelReference1A16[-2], ax
        pusha
        mov  ah, 0fh
        int  10h
        mov  OrigVideoModeA16, al
        popa
        mov  si, Offset EGroup:FirstExtHandler
        $do
        mov  di, word ptr [si].2 ;
        mov  bl, [di].EID_IntNum
        mov  ax, 204h
        int  31h
        mov  [di].EID_OldIntVectHi, cx
        mov  [di].EID_OldIntVect, edx
        mov  cx, bp
        movzx edx, si
        mov  ax, 205h
        int  31h        ;set vector XXX
        jc   PInitErrorSetVect
        add  si, ExtHandlerStep
        cmp  si, offset EGroup:FirstExtHandler+NHookedInterrupts*ExtHandlerStep
        $enddo jb
        mov  word ptr ClientInt0VectorA16[4], cx ;set default selector
                                                 ;for client int0 handler
;----------------- set interrupt vector for coprocessor ---------------------
        mov  ax, 400h
        int  31h
        mov  bl, dl
        add  bl, 5
        mov  dx, Offset EGroup:int75handler
        mov  cx, bp
        mov  ax, 205h
        int  31h
        jc  PInitErrorSetVect
;----------------------allocate a callback for mouse-------------------------
        mov  word ptr MouseHookProcA16[4], bp      ;setup selector for default mouse hook proc
        mov  esi, offset EGroup:MouseCallbackHandler
        mov  di, OffMouseCallbackStruct   ;high part of edi already zero
        mov  ds, bp                       ;code selector of the extender
        mov  ax, 303h
        int  31h
        mov  si, ROffErrAllocCBackEM+LS
        jc   PInitError

        shl  ecx, 16
        mov  cx, dx
        mov  fs:[large MouseCallBackPlace], ecx
        RRT
        mov  es:MouseCallBackPlace1A16, ecx
IFDEF EDebug
;---------------------------- init debugger --------------------------------
        push ss
        pop  ds
        push ss
        pop  es
        mov  dx, 10000/4
        mov  cx, 20000
        mov  bp, DebuggerStart+LS-100h
        call @@AllocAndMove
        push bx
        push large 100h
        mov  cx, fs
        call fword ptr [esp]
        add  sp, 6
ENDIF
;----------------------------- setup loader ---------------------------------
        mov  dx, LoaderSize/4
        mov  cx, LoaderFullSize
        mov  bp, LoaderStart+LS
        push ss
        pop  ds
        push ds
        pop  es
        call @@AllocAndMove
        pop  ds                    ;restore extender data selector
        pop  bp                    ;restore extender code selector
        mov  ss, ax
        mov  sp, OffLoaderStackEnd - size DC_Struct - 2
        mov  ss:LoaderCodeMemHandleA16, ebp
        push bx
        DB PushWCode
        DW Offset LGroup:LoaderEntry
        retf

PInitErrorSetVect:
        mov si, ROffSetVectEM+LS
LLabel PInitErrorE
PInitError:
        push ss
        pop  es
        push ss
        pop  ds
        sub  sp, DCStructSize
        mov  di, sp
        push si
        mov  si, ROffLoadErrMsg+LS
        call DispStrWithDPMI
        pop  si
        call DispStrWithDPMI
        mov  si, ROffCRLF+LS
        call DispStrWithDPMI
        mov  ax, 4Cffh
        int  21h
DispStrWithDPMI:
        mov  [di].DC_DS, 0              ;put transfer segment here
LLabel PatchPoint8
        xor  ecx, ecx
        mov  word ptr [di].DC_EDX, si
        mov  byte ptr [di].DC_EAX[1], 9
        mov  dword ptr [di].DC_SP, ecx
        mov  bx, 21h
        mov  ax, 300h
        int  31h
        retn

PInitErrorSel:
        mov si, ROffErrAllocSelEM+LS
        jmp PInitError

;@@SetExcH:
;        mov ax, 203h
;        int 31h
;        jc  PInitErrorSetVect
;        retn
IFDEF XLE
@@AllocAndSetSel:
        xor  ax, ax
        mov  cx, 1
        push ds
        pop  es
        int  31h                         ;allocate selector for flat
        jc   PInitErrorSel
        xchg bx, ax
        mov  ax, 0Ch
        int  31h                     ;set flat descriptor
        jc   PInitErrorSel
        xchg ax, bx
        retn
ENDIF
@@AllocAndMove:
        push cx
        xor  bx, bx
        mov  ax, 501h
        int  31h          ;allocate block
        push si
        push di
        mov  si, ROffErrNoDPMIMemoryEM+LS
        jc   PInitError
        cmp  bp, ExtenderStart+LS
        $ifnot jne
          xor si, si
          mov di, ExtenderFullSize
          mov ax, 600h          ;lock block for extender
          int 31h
          mov si, ROffErrLockEM+LS
          jc PInitError
        $endif
        pop  esi
        mov  di, ROffDefaultFlatDescriptor+LS
        pop  word ptr [di]      ;set descriptor in memory
        mov  [di].4, bl
        mov  [di].7, bh
        mov  [di].2, cx
        IFNDEF XLE
          or   byte ptr[di].5, 8      ;data to code segment
        ENDIF
        mov  byte ptr[di].6, 40h
        mov  cx, 1
        xor  ax, ax
        int  31h          ;allocate selector
        jc   PInitErrorSel
        mov  bx, ax
        mov  ax, 0Ch
        int  31h          ;setup selector
        jc   PInitErrorSel
        mov  ax, 0Ah
        int  31h          ;create data alias
        jc   PInitErrorSel
        mov  es, ax
        xor  di, di
        cld
        xchg ebp, esi
        mov  cx, dx
        rep  movsd
        ret

.8086
;RInitMemError:
;---------------------------------- startup ----------------------------------
RSetup:
        shr byte ptr InitFlags[PSP], 1    ;test for intro banner
        $ifnot jc
          mov  dx, ROffIntroMsg+PSP;PLShift
          mov  ah, 9
          int  21h
        $endif
        mov  ah, 30h
        int  21h
        mov  si, ROffVersionEM
        cmp  al, 3
        jb   RInitError

        mov  si, ROffWrongCPUEM
;Check for CPU
        pushf
        cli
        pushf
        pop   ax
        and   ax, 0FFFH    ;clear bits 12-15
        push  ax
        popf
        pushf
        pop   ax
        mov   bh, 0F0h
        sub   ah, bh       ;check if all bits 12-15 in flags are set
        $ifnot jnc
          or    ah, bh       ;try to set bits 12-15
          push  ax
          popf
          pushf
          pop   ax
        $endif
        popf
        test  ah, bh      ;if bits 12-15 are cleared then 286
        jz    RInitError
.386P
        movzx  esp, sp      ;workaround for windows 3000:))
        pushfd
        cli
        pushfd
        pop  eax
        xor  eax, 240000h
        push eax
        popfd
        pushfd
        pop  ecx
        xor  eax, ecx
        popfd
        shr  eax, 19
        mov  ah, 3
        jc   @@EndCPUDetect
        mov  byte ptr PatchPoint4R[PSP][-1], 77h      ;set PCD in int31(800h)
        mov  ah, 4
        test al, 4
        jnz  @@EndCPUDetect
        mov  ax, 1          ;16-31 bits of eax are cleared before
        db   0Fh, 0A2h      ;cpuid
        and  ax, 0F00h
@@EndCPUDetect:
        mov  CPUTypeR[PSP], ah
        xor  di, di
        cld
        xor  ax, ax
        mov  si, ROffEnvironEM
        mov  ds, ds:[di+2Ch]      ;environment
        $do
          inc  di
          js   RInitError           ;out of environment
          cmp  word ptr ds:[di-1], ax
        $enddo jne
        inc  di
        inc  ax
        cmp  word ptr ds:[di], ax
        $ifnot jz
;----------------------- real mode init error handler ------------------------
RInitError:
        mov  dx, ROffFirstError
        mov  ah, 9
        push cs
        pop  ds
        int  21h
        mov  dx, si
        int  21h
        mov  dx, ROffCRLF
        int  21h
        mov  ax, 4CFFh              ;return with code 255
        int  21h
        $endif

        inc  di
        inc  di
        mov  dx, di
        $do
        inc  di
        js   RInitError
        cmp  byte ptr ds:[di-1], ah
        $enddo jne
        mov  si, ROffFileAccessEM
        mov  ax, 3D20h           ;read only, deny write
                                 ;the file may be opened by other instance
                                 ;with same open mode only
        int  21h
        jc   RInitError
        push es
        pop  ds
        add  EnvSizeR[PSP], di
        mov  word ptr FileHandleR[PSP], ax

;------------ Setup linear disp for all references to 16Stub -----------------
        xor  ebx, ebx
        push cs
        pop  bx
        shl  ebx, 4
        mov  si, -RRTEntryes*2
        $do
        mov  di, [si][SegStartRelocR0+RRTEntryes*2][PSP]
        inc  si
        add  dword ptr ds:[di], ebx
        inc  si
        $enddo jnz
        mov  ax, TransferBufferPSizeR[PSP]  ;patch transfer buffer size
        mov  PatchPointTStSzR[-2][PSP], ax  ;in loader code
        shr  byte ptr InitFlags[PSP], 1    ;DPMI/VCPI check sequence?
        $ifnot jnc
          call @@TryVCPI
          call @@TryDPMI
        $else jmp
          call @@TryDPMI
          call @@TryVCPI
        $endif
@@TryXMS:
;look for PE bit in cr0 - must be cleared for RAW/XMS!
        smsw ax
        shr  ax, 1
        mov  si, ROffAlreadyVMEM
        jc   RInitError
;patch 2 points for working under RAW/XMS
        mov  word ptr VCPIRICall[PSP],  JmpShortCode+1*100h
        mov  word ptr PatchPoint[PSP],  JmpShortCode+(RawSwitcherToPM-PatchPoint-2)*100h
        call FindXMSServer
        jnz  @@TryRAW
        mov  ah, 5
        call XMCR[PSP]         ;local enable A20
        or   ax, ax
        $ifnot jnz               ;extended memory not available if impossible to enable A20
          dec  XMSBlockNotAllocatedR[PSP]  ;disable XMS block allocation
          jmp  InitRX
        $endif
        mov  word ptr PatchPoint1[PSP], 05B4h    ;"mov ah, 5"
;change gate descriptor for PM->RM mode switch in XMS/RAW
InitRX:
        mov  VCPICallDescR[PSP], ROffL0234 + ((VCPISelector+8) shl 16)
        mov  byte ptr VCPICallDescR[PSP][6], 0
        jmp  InitRXV
FindXMSServer:
;check for XMS server
        mov  ax, 4300h
        int  2Fh
        cmp  al, 80h                ;is XMS available ?
        $ifnot jne
          mov  ax, 4310h
          int  2Fh                  ;get XMS entry - always succefful
          mov  word ptr XMCR[PSP], bx
          mov  word ptr XMCR[PSP][2], es
          inc  XMSBlockNotAllocatedR[PSP]  ;enable XMS block allocation
          xor  bx, bx        ;clear ZF
        $endif
        ret
@@TryVCPI:
;@@VTest:                    ;in V86 mode int 67h shall be supported anywhere
        mov  ax, 0DE00h     ;else V86 monitor is incorrect
        xor  ebp, ebp
        mov  fs, bp
        push cs
        push ROffint67Handler
        mov  di, 67h*4
        cmp  fs:[di], ebp
        pop  ebp
        $ifnot je
          int  67h
        $else jmp
          xchg fs:[di], ebp
          int  67h
          mov  fs:[di], ebp
        $endif
@@VETest:
        or  ah, ah
        $ifnot jz
          ret
        $endif
;---------------------- init DPMI host under VCPI----------------------------
        inc  VCPIMemAvailableR[PSP]
        call FindXMSServer
        mov  ax, 0DE03h
        int  67h
        mov  TotalVCPIPagesR[PSP], edx
;------------------- init DPMI host under VCPI/XMS/RAW -----------------------
InitRXV:
        push ds
        pop  es
        IFDEF VMM
;----------------- open swap file -------------------
          mov  dx, ROffSwapFileName+PSP
          mov  ah, 3Ch
          mov  cx, 20h
          int  21h            ;create swap file
          mov  si, OffSwapFileEM
          jc   RInitError
          mov  word ptr swap_file_handleR[PSP], ax
          xchg bx, ax
          mov  dx, 1
          xor  cx, cx
          mov  ax, 4200h
          int  21h             ;seek to 1
        ENDIF
;save all interrupt vectors
        mov  di, ROffSavedRealVectors+PSP
        xor  si, si
        mov  fs, si
        mov  ax, si
        mov  cx, 256*2
        rep  movs word ptr es:[di], fs:[si]
        mov  cx,  (OffLastInit-OffSavedRealVectors)/2
        rep  stosw
        PatchPoint7:
        jmp short PatchPoint7End  ;this code may by replaced with "mov eax,"
        ;mov  eax, 0
        ;org  $-4
        DW   offset cs:int15Handler, seg DGroup16
        xchg fs:[15h*4], eax
        mov  OldInt15R[PSP], eax
        PatchPoint7End:

        comment %
        mov di, ROffGDT+PSP
        mov si, offset ds:InitGDT+PSP
        mov cx, (LastDescriptor-FirstDescriptor)/2
        rep movsw    %
        mov di, ROffGDT+FirstGateSelector+PSP
        mov si, offset ds:Traps3SetupTable+PSP
        mov bx, ROffFirstTrap3+PSP
        mov cx, NTraps3
        $do
        lodsb
        mov dl, al
        $do
        ;-- set PL3 hook code for gates --
        mov byte ptr[bx], CallFarCode
        lea ax, [di-(ROffGDT+PSP)+3]
        mov [bx+5], ax
        mov byte ptr [bx+3], dl
        add bx, 4
        inc dl
        $enddo jnz

;------------------------- set gate descriptor in GDT -----------------------
        movsw               ;low word of entry disp
        mov ax, Code1Selector
        stosw               ;selector
        mov ah, (0E0h+SS_GATE_PROC3)
        lodsb               ;load dword count
        stosw               ;access rights: DPL = 3
        mov   ax, KernelBase shr 10h
        stosw               ;high word of entry disp
        $enddo loop
        ; -- special set trap selector for int 31h --
        mov word ptr ds:[ROffFirstTrap3+PSP+4*31h+5], DPMIEntryGateSelector
        mov  FirstTrap3R[OffPMSaveStateTrap3+7][PSP], RetfCode

;-------------------------- init some TSS fields ------------------------
        mov  TSSR[PSP].TSS_ESP0,   OffKernelStack
        mov  TSSR[PSP].TSS_ESP1,   OffKernelStack1End
        mov  TSSR[PSP].TSS_SS0,    Data0Selector
        mov  TSSR[PSP].TSS_SS1,    Data1Selector
        mov  TSSR[PSP].TSS_IOBASE, 0FFFFh

;----------------------------- Fill  client IDT ------------------------------
        mov  di,   ROffClientIDT+PSP
        mov  dx,   OffDefIntTrap3
        $do
        mov  [di], dx
        mov  word ptr [di].4, Trap3Selector
        mov  word ptr IDTR[di-ROffCLientIDT].0, dx
        mov  word ptr IDTR[di-ROffCLientIDT].2, Trap3Selector
        mov  dword ptr IDTR[di-ROffCLientIDT].4, (0E0h+SS_GATE_TRAP3) shl 8
        add  di, 8
        add  dx, 4
        dec  cl
        $enddo jnz
;------------------------ fill default exceptions table ---------------------
        mov  cl, 16
        mov  eax, OffDefaultExcTrap3
        mov  di, ROffClientExc+PSP
        $do
        mov  word  ptr [di].4, Trap3Selector
        mov  dword ptr [di], eax
        add  ax, 4
        add  di, 8
        $enddo loop
;------------------------ Init hardware interrupts ---------------------------
        mov  ax, 0DE0Ah
        call VCPIRICall
        mov  bh, cl
        ;mov HardwareIntMapR[PSP], bx
        mov  esi, OffHIntHandlers and 0FFFFh
        ;mov dx, JmpShortCode + ((23*4) shl 8)
        mov  ax, 8
        movzx di, bl
        call Init8Handlers
        mov  al, 70h
        movzx di, bh
        call Init8Handlers
        xor  di, di
        mov  ax, di
        call Init8HandlersAbs
        call Init8HandlersAbs
;--------------------- generate code for real mode hooks ----------------
        mov  di, offset DGROUP16:FirstSwitchCode+PSP
        mov  ax, ROffRMS_RMHandler - ROffFirstSwitchCode-3
        mov  cx, (MaxSystemSwitchCode+nMaxCallbacks*4)/4
        $do
        mov  byte ptr ds:[di], NearCallCode
        mov  [di].1, ax
        add  di, 4
        sub  ax, 4
        $enddo loop
;------- calculate top of dos memory area, may be used by dpmi host --------
        mov  ah, 48h
        mov  bx, -1
        int  21h
        mov  ax, TransferBufferPSizeR[PSP]
        add  ax, MemReserveR[PSP]
;ax - total size to reserve
        mov  cx, ds:[2]       ;top of task memory
        cmp  bx, ax
        $ifnot jae
          sub  cx, ax         ;decrase top if no another memory for transfer buffer
          $ifnot ja
            xor cx, cx
          $endif
        $endif
        push cx
;--------------------- hook autopassup interrupts ---------------
        mov di, ROffAutoPassupRJmps
        xor si, si
        mov cx, 100h
        $do
          bt  word ptr PassupIntMapR[PSP], si
          $ifnot jnc
            shl si, 2
            cli
            mov eax, fs:[si]
            mov byte ptr cs:[di], JmpFarCode
            mov cs:[di].1, eax
            mov fs:[si].2, cs
            mov fs:[si], di
            add di, 5
            sti
            shr si, 2
          $endif
          inc si
        $enddo loop
;---------------------- install terminate handler --------------------------
        mov  eax, 0
        org $-4
        DW offset dgroup16:TerminateRHandler
        DW seg dgroup16
        xchg ds:[0Ah], eax
        mov  dword ptr OldInt22R[PSP], eax
        pop  cx
        jmp  @@CriticalInitVCPI

@@TryRAW:
        mov  ah, 88h
        int  15h
        movzx ebp, ax
        or   ax, ax
        $ifnot jz
        cli
        call IsA20Enabled
        $ifnot je
@@PS2:
        mov  cx, 5000
        in   al, 92h
        test al, 2
        jnz  @@NotPS2
        or   al, 2
        jmp  short $+2
        jmp  short $+2
        out  92h, al
        $do
        jmp  short $+2
        jmp  short $+2
        in   al, 92h
        test al, 2
        $enddo loopz
        ;jz   @@NotPS2
        call IsA20Enabled
        je   @@A20OK
@@NotPS2:
        mov  si, ROffA20EnableSq+PSP
        $do  jmp
        movzx dx, al
        jmp  short $+2
        outsb
        $while
        xor  cx, cx
        $do
        jmp  short $+2
        jmp  short $+2
        in   al, 64h
        test al, 2
        $enddo loopnz
        jnz  @@A20Err
        lodsb
        or   al, al
        $enddo jnz
        mov  dx, gs:[46Ch]    ;timer counter
        $do                   ;wait for A20 about 2 timer ticks
          call IsA20Enabled
          je   @@A20OK
          sti
          mov  ax, gs:[46Ch]
          sub  ax, dx
          cli
          cmp  ax, 2
        $enddo jb
@@A20Err:
        sti
        xor  ebp, ebp
        jmp  @@RawPatch
        $endif
@@A20OK:
        sti
        mov  di, 1
        mov  edx, 100000h       ;default value for top of the extended memory
        call CheckVDisk
        $ifnot jnz
          mov  dx, es:[di+2Eh-1];vdisk top in k
          shl  edx, 10          ;convert to bytes
        $endif
        les  di, gs:[di-1+19h*4]
        ;add  di, 12h
        call CheckVDisk
        $ifnot jnz
          mov  eax, es:[di+2Ch]
          and  eax, 0FFFFFFh
          cmp  eax, edx
          $ifnot jbe
            xchg eax, edx
          $endif
        $endif
        $endif
;ebp - size of mem window, <= 0FFFFh
;edx - top of mem window
        mov  eax, MaxXMSAllocateR[PSP]
        ;shr  eax, 10                        ;convert to kilobytes
        cmp  eax, ebp
        $ifnot jb
          mov  eax, ebp                     ;now hi word of eax is undefined!
        $endif
        sub  bp, ax                         ;bp = leaved_mem_size
        xchg ax, bp                         ;move allocation size to ebp
        mov  PatchPoint_int15R[1][PSP], ax  ;set new extended size, returned by my int 15 handler
        shl  eax, 10
        add  edx, eax                       ;shift my memory base to bottom of the window
@@RawPatch:
        mov  word ptr PatchPoint7[PSP], 0B866h       ;mov eax, ??????
;@@NoA20Patch:
        ;mov  word ptr PatchPoint1[PSP], JmpShortCode + ((Exit2-PatchPoint1-2) shl 8)
        mov  bx, ROffFreeXMSCount+PSP
        call TranslateMemLimits
        jmp  InitRX
CheckVDisk:
        cmp  dword ptr es:[di+12h], 'SIDV'
        $ifnot jne
          cmp  byte ptr es:[di+12h+4], 'K'
        $endif
        retn
LLabel A20EnableSq
        DB 64h, 0D1h, 60h, 0DFh, 64h, 0FFh, 0

IsA20Enabled:
        xor  bx, bx
        mov  gs, bx
        dec  bx
        mov  es, bx
        inc  bx
        mov  al, es:[bx+10h]
        mov  ah, al
        inc  al
        xchg al, gs:[bx]
        cmp  ah, es:[bx+10h]
        mov  gs:[bx], al
        ret
RInit   ENDP

;al - unmapped real mode interrup number
;di - mapped interrupt number
Init8Handlers PROC
        test di, 111b
        jne @@HIntMapError
        push di
        shr di, 3
        mov byte ptr PassupIntPMapR[PSP][di], 0FFh
        pop di
        cmp di, 18h
        $ifnot jae
          cmp di, 8
          jne @@HIntMapError
          retn
        $endif
Init8HandlersAbs:
        $do
        mov byte ptr HIntHandlersR[PSP][si-(OffHIntHandlers and 0FFFFh)], PushBCode
        mov byte ptr HIntHandlersR[PSP][si-(OffHIntHandlers and 0FFFFh)].1, al

        mov ecx, (OffInterruptH and 0FFFFh) -7       ;only interrupt
        cmp di, 10h
        $ifnot jae
          mov cx, (OffException0DHandler and 0FFFFh)-7
          cmp di, 0Dh
          $toendif je
          IFDEF VMM
            mov cx, (OffException0EHandler and 0FFFFh)-7
            cmp di, 0Eh
            $toendif je
          ENDIF
          mov cx, (OffExceptionOrInterruptH and 0FFFFh)-7
          bt word ptr PassupIntPMapR[PSP], di
          $ifnot jc
            mov cx, (OffExceptionWithCodeH and 0FFFFh)-7
            cmp di, 8
            $ifnot jae
              cmp di, 2
              je  @@NotExc
              mov cx, (OffExceptionWOCodeH and 0FFFFh)-7
            $endif
          $endif
        $endif
        mov byte ptr HIntHandlersR[PSP][si-(OffHIntHandlers and 0FFFFh)].2, JmpNearCode
        sub ecx, esi
        mov dword ptr HIntHandlersR[PSP][si-(OffHIntHandlers and 0FFFFh)].3, ecx
        shl di, 2
        mov FirstTrap3R[PSP][di].3, al   ;set new redirected real mode number
        shl di, 1
        mov IDTR[PSP][di], si
        mov IDTR[PSP][di].2, Code0Selector
        mov IDTR[PSP][di].4, (0E0h+SS_GATE_INT3) shl 8
        mov IDTR[PSP][di].6, (OffHIntHandlers shr 16)
        add si, 7
        shr di, 3
@@NotExc:
        inc di
        inc ax
        test di, 111b
        $enddo jnz
        retn
@@HIntMapError:
        mov si, ROffIntMapEM
        jmp RInitError

        ENDP
eseg IText16

