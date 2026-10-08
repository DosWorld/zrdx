;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

;real mode init routines
        SEGM IData16
%ifdef VMM
        LByte SwapFileName
        db 'zrdx!.swp', 0
        LByte SwapFileEM
        db 'swap I/O error', 0
%endif
        LByte IntroMsg
        db 'Zurenava DOS extender, version 0.51OSE. Copyright(C) 1998-1999, Sergey Belyakov', 13, 10, 'Copyright(C) 2026, Viacheslav Komenda', 13, 10, '$'
        LByte FirstError
        db 'ZRDX init error:$'
        LByte VersionEM
        db 'DOS 3.0+ requred$'
        LByte EnvironEM
        db 'bad environment$'
        LByte WrongCPUEM
        db '80386+ CPU not detected$'
        LByte FileAccessEM
        db "can't open EXE$"
        LByte AlreadyVMEM
        db 'already in V86 mode without VCPI or DPMI$'
        LByte VCPIServerEM
        db 'unexpected VCPI host fault$'
        LByte DOSMemoryEM
        db 'DOS memory allocation$'
        LByte IntMapEM
        db 'unsupported hardware interrupts mapping$'
;LByte UnXMSEM
;        DB 'unexpected XMS fault$'
;LByte OutOfXMSEM
;        DB 'out of XMS memory$'
        LByte SwitchModeEM
        db "can't enter protected mode via DPMI$"
        LLabel SetVectEM
        db "Can't set interrupt handler$"
        LLabel ErrAllocCBackEM
        db "Can't allocate realmode callback$"
;LLabel A20EM
;        DB "Can't enable line A20$"
        LLabel CRLF
        db 13, 10, '$'
        ESEG IData16
        SEGM IText16
;assume cs:DGROUP16, ds:DGROUP16, ss:DGROUP16
RInit:
InitDPMI:
TryDPMI@167:
        mov ax, 1687h
        int 2Fh
        or ax, ax
        _ifnot jz
NoDPMI@167:
          ret
        _endif
        shr bx, 1
        jnc NoDPMI@167
PSPSize equ 100h
        ;RMCodeSize = 0 ;SegSizeEText16
        mov bp, cs
        ;mov  ss, bp
        lea ax, [bp + si + ((ROffLoaderEnd + 200h + 15) / 16) + MouseRHandlerPSize]
        lea bx, [bp + MouseRHandlerPSize]
        lea bp, [bx + si]
        add [ROffMemBlock0Size + PSP], si
        cmp ax, [2]
        mov si, OffDOSMemoryEM
        ja near RInitError
        mov ss, bp
        mov sp, ROffLoaderEnd + 200h
        push bp
        push ROffILoaderEntry0    ;!!!!!!!!!!!!!!
        push es
        push di
        mov di, ROffLoaderEnd - 4
        mov si, ROffLoaderEnd - 4 + PSPSize               ;!!!!!!!!!!!!!!
        mov cx, (ROffLoaderEnd - ROffPermanentPart + 3) / 4
        mov es, bp
        std
        rep movsd
        mov ds, bp
        mov [ROffPatchPoint8 - 2], bp
        mov ax, 1
        mov es, bx
        mov si, ROffSwitchModeEM
        cld                 ;workaround for MS NT5(windows 2000) beta 3
        retf
LS equ 0

VCPIRICall:
        int 67h
        ret
VCPIRIEmulator:
        cmp al, 1
        _ifnot jne
          ;fill PageTable
          xor eax, eax
          mov al, 67h     ;access rights
          mov cx, 110h    ;number of real mode pages
          cld
          _do
            stosd
            add eax, 1000h
          _enddo loop
          ;mov  ebx, OffVCPIPMEmulator
          ret
        _endif
        ;cmp al, 0Ah
        ;$ifnot jne
        mov bl, 8
        mov cl, 70h
        ret
        ;$endif
        ;jmp RawSwitcherToPM

CriticalInitVCPI@167:
        mov dx, ds
%ifndef VMM
          add dx, (ROffRStackEnd + 1FFFh + PSPSize) / 16
%else
        ;allocate additional space for vmm disk transfer buffer
          add dx, (ROffRStackEnd + 1000h + 1FFFh + PSPSize) / 16
%endif
        mov dl, 0              ;page aligment
        mov cl, 0
        cmp cx, dx
        _ifnot ja
          mov cx, dx
        _endif
        mov ax, ds
        sub ax, cx             ;get para size of dpmi real mode segment
                                ;and vcpi page0
        neg ax
        mov [ROffMemBlock0Size + PSP], ax
        sub cx, dx
        mov [ROffNExtraRPages + PSP], ch
        mov di, dx
        dec dh
        mov es, dx
        push 0FFh
        mov ax, 0F2h
        push ax         ;
        push ds

        db PushWCode
        LLabel EnvSize
        dw 0
        push ax
        mov ax, (17 + 2) * 8 + 7
        xchg ax, [2Ch]
        push ax
        push -(1)
        push 40F2h
        push di
        push -(1)
        push 0FAh
        push di
        mov [ROffPatchPoint8 + PSP - 2], di

        mov cx, 4
        mov di, ROffLDT + PSP + 17 * 8
        _do
        xor eax, eax
        pop ax
        shl eax, 4
        mov [di + 2], eax
        pop word [di + 5]
        pop word [di]
        add di, 8
        _enddo loop
        mov byte [ROffLDTFree + (16 / 8) + PSP], 11110b ;set bitmask for allocated descriptors

        mov di, WinSize + 1000h + (OffLastInit - KernelBase) - 4  ;skip first page and
        mov si, OffProtectedStart + (OffLastInit - KernelBase) + PSP - 4
        std
        xor eax, eax
        mov cx, (OffLastInit - KernelBase) / 4
        ;move dpmi kernel up to page aligned location
        rep movsd         ;move server code to new location
        mov di, WinSize + 1000h - 4
        mov si, WinSize + PSP - 4
        mov cx, WinSize / 4
        rep movsd         ;move extender to window
        mov cx, 400h - 1
        rep stosd         ;clear page0
        cld
        mov ds, dx
LSX equ -(KernelBase) + 1000h + WinSize
LSR equ -(OffProtectedStart) + 1000h + WinSize
        mov si, OffGDT + VCPISelector + LSX
        mov ax, 0DE01h
        call VCPIRICall
        mov [ROffVCPICall + LSR], ebx

        mov si, dx
        shr si, 8 - 2      ;shift of Page0 in pagetable
        add [ROffPatchPoint5 - 4 + LSR], si
        mov eax, [si + ((OffPageDir + LSX) / 1024)]
        and ax, 0F000h
        mov [cs:ROffSwitchTableCR3], eax
        mov di, OffPageDir + LSX
        call MovePageRef@167
        mov [(OffPage2 + ((OffPage0 - KernelBase) >> 10)) + LSX], eax       ;mov Page0Ref[LSX], eax
        mov eax, [si + ((OffPage2 + LSX) / 1024) - 4]
        call MovePageRef1@167    ;set reference to page 2 in the page directory

        mov di, OffPage2 + LSX
        add si, WinSize / 1024
        mov cl, (OffLastInit - KernelBase + 0FFFh) / 1000h
        _do
        call MovePageRef@167     ;set references to kernel in the page 2 table
        _enddo loop
        jmp SwitchToPM
        ;mov  esi, ROffSwitchTable
        ;RRT
        ;mov  ax, 0DE0Ch
        ;cli
        ;call VCPIRICall
MovePageRef@167:
        lodsd
MovePageRef1@167:
        and ah, 0F0h
        mov al, 67h
        stosd
        ret
        LLabel PermanentPart

        LLabel DefaultFlatDescriptor
        Descr 0, 0FFFFFh, 0CF93h               ;Flat data descriptor
        LLabel ILoaderEntry0
        jc near RInitError
        LLabel ILoaderEntry
        movzx esp, sp                   ;workaround for windows 3000:)
        mov [ROffSavedPSP + LS], es          ;save psp to extender
        mov [ROfflSavedPSP + LS], es         ;save psp to loader
        mov edi, ROffDefaultFlatDescriptor + LS
        mov ax, cs
        and al, 3
        shl al, 5
        or [di + 5], al   ;set dpl to current cpl
        xor ax, ax
        mov cx, 1
        push ds
        pop es
        int 31h                         ;allocate selector for flat
        jc near PInitErrorSel
        mov [ROffFlatSelector + LS], ax      ;save flat selector to extender
        xchg bx, ax
        mov ax, 0Ch
        int 31h                     ;set flat descriptor
        jc near PInitErrorSel
        mov fs, bx
        mov dx, ExtenderSize / 4
        mov cx, ExtenderFullSize
        mov bp, ExtenderStart + LS
        call AllocAndMove@167          ;allocate and move extender
        mov [ROffExtenderSel + LS], bx
        mov bp, bx                 ;save code selector for extender
        mov ds, ax
        push bx                     ;save code selector on the stack
        push ax                     ;save data selector on the stack
        ;es, ds - selector of the extender
;assume es:EGroup
        mov [dword OffSelReference0 - 4], ax
        mov [dword OffSelReference1 - 2], ax
        pusha
        mov ah, 0fh
        int 10h
        mov [dword OffOrigVideoMode], al
        popa
        mov si, OffFirstExtHandler
        _do
        mov di, [si + 2] ;
        mov bl, [di + EID_IntNum]
        mov ax, 204h
        int 31h
        mov [di + EID_OldIntVectHi], cx
        mov [di + EID_OldIntVect], edx
        mov cx, bp
        movzx edx, si
        mov ax, 205h
        int 31h        ;set vector XXX
        jc PInitErrorSetVect
        add si, ExtHandlerStep
        cmp si, OffFirstExtHandler + NHookedInterrupts * ExtHandlerStep
        _enddo jb
        mov [dword OffClientInt0Vector + 4], cx ;set default selector
                                                 ;for client int0 handler
;----------------- set interrupt vector for coprocessor ---------------------
        mov ax, 400h
        int 31h
        mov bl, dl
        add bl, 5
        mov dx, int75handler
        mov cx, bp
        mov ax, 205h
        int 31h
        jc PInitErrorSetVect
;----------------------allocate a callback for mouse-------------------------
        mov [dword OffMouseHookProc + 4], bp      ;setup selector for default mouse hook proc
        mov esi, MouseCallbackHandler
        mov di, OffMouseCallbackStruct   ;high part of edi already zero
        mov ds, bp                       ;code selector of the extender
        mov ax, 303h
        int 31h
        mov si, ROffErrAllocCBackEM + LS
        jc PInitError

        shl ecx, 16
        mov cx, dx
        mov [fs:dword OffMouseCallbackPlace], ecx
        RRT
        mov [es:dword OffMouseCallbackPlace1], ecx
%ifdef EDebug
;---------------------------- init debugger --------------------------------
        push ss
        pop ds
        push ss
        pop es
        mov dx, 10000 / 4
        mov cx, 20000
        mov bp, DebuggerStart + LS - 100h
        call AllocAndMove@167
        push bx
        push dword 100h
        mov cx, fs
        call dword far [esp]
        add sp, 6
%endif
;----------------------------- setup loader ---------------------------------
        mov dx, LoaderSize / 4
        mov cx, LoaderFullSize
        mov bp, LoaderStart + LS
        push ss
        pop ds
        push ds
        pop es
        call AllocAndMove@167
        pop ds                    ;restore extender data selector
        pop bp                    ;restore extender code selector
        mov ss, ax
        mov sp, OffLoaderStackEnd - DC_Struct_size - 2
        mov [ss:dword OffLoaderCodeMemHandle], ebp
        push bx
        db PushWCode
        dw LoaderEntry
        retf

PInitErrorSetVect:
        mov si, ROffSetVectEM + LS
        LLabel PInitErrorE
PInitError:
        push ss
        pop es
        push ss
        pop ds
        sub sp, DCStructSize
        mov di, sp
        push si
        mov si, ROffLoadErrMsg + LS
        call DispStrWithDPMI
        pop si
        call DispStrWithDPMI
        mov si, ROffCRLF + LS
        call DispStrWithDPMI
        mov ax, 4Cffh
        int 21h
DispStrWithDPMI:
        mov word [di + DC_DS], 0              ;put transfer segment here
        LLabel PatchPoint8
        xor ecx, ecx
        mov [di + DC_EDX], si
        mov byte [di + DC_EAX + 1], 9
        mov [di + DC_SP], ecx
        mov bx, 21h
        mov ax, 300h
        int 31h
        ret

PInitErrorSel:
        mov si, ROffErrAllocSelEM + LS
        jmp PInitError

;@@SetExcH:
;        mov ax, 203h
;        int 31h
;        jc  PInitErrorSetVect
;        retn
AllocAndMove@167:
        push cx
        xor bx, bx
        mov ax, 501h
        int 31h          ;allocate block
        push si
        push di
        mov si, ROffErrNoDPMIMemoryEM + LS
        jc PInitError
        cmp bp, ExtenderStart + LS
        _ifnot jne
          xor si, si
          mov di, ExtenderFullSize
          mov ax, 600h          ;lock block for extender
          int 31h
          mov si, ROffErrLockEM + LS
          jc PInitError
        _endif
        pop esi
        mov di, ROffDefaultFlatDescriptor + LS
        pop word [di]      ;set descriptor in memory
        mov [di + 4], bl
        mov [di + 7], bh
        mov [di + 2], cx
          or byte [di + 5], 8      ;data to code segment
        mov byte [di + 6], 40h
        mov cx, 1
        xor ax, ax
        int 31h          ;allocate selector
        jc PInitErrorSel
        mov bx, ax
        mov ax, 0Ch
        int 31h          ;setup selector
        jc PInitErrorSel
        mov ax, 0Ah
        int 31h          ;create data alias
        jc PInitErrorSel
        mov es, ax
        xor di, di
        cld
        xchg ebp, esi
        mov cx, dx
        rep movsd
        ret

        .8086
;RInitMemError:
;---------------------------------- startup ----------------------------------
RSetup:
..start:
        mov ah, 30h
        int 21h
        mov si, ROffVersionEM
        cmp al, 3
        jb RInitError

        mov si, ROffWrongCPUEM
;Check for CPU
        pushf
        cli
        pushf
        pop ax
        and ax, 0FFFH    ;clear bits 12-15
        push ax
        popf
        pushf
        pop ax
        mov bh, 0F0h
        sub ah, bh       ;check if all bits 12-15 in flags are set
        _ifnot jnc
          or ah, bh       ;try to set bits 12-15
          push ax
          popf
          pushf
          pop ax
        _endif
        popf
        test ah, bh      ;if bits 12-15 are cleared then 286
        jz RInitError
cpu 386
        movzx esp, sp      ;workaround for windows 3000:))
        pushfd
        cli
        pushfd
        pop eax
        xor eax, 240000h
        push eax
        popfd
        pushfd
        pop ecx
        xor eax, ecx
        popfd
        shr eax, 19
        mov ah, 3
        jc EndCPUDetect@167
        mov byte [ROffPatchPoint4 + PSP - 1], 77h      ;set PCD in int31(800h)
        mov ah, 4
        test al, 4
        jnz EndCPUDetect@167
        mov ax, 1          ;16-31 bits of eax are cleared before
        db 0Fh, 0A2h ;cpuid
        and ax, 0F00h
EndCPUDetect@167:
        mov [ROffCPUType + PSP], ah
        xor di, di
        cld
        xor ax, ax
        mov si, ROffEnvironEM
        mov ds, [di + 2Ch]      ;environment
        _do
          inc di
          js RInitError           ;out of environment
          cmp [di - 1], ax
        _enddo jne
        inc di
        inc ax
        cmp [di], ax
        _ifnot jz
;----------------------- real mode init error handler ------------------------
RInitError:
        mov dx, ROffFirstError
        mov ah, 9
        push cs
        pop ds
        int 21h
        mov dx, si
        int 21h
        mov dx, ROffCRLF
        int 21h
        mov ax, 4CFFh              ;return with code 255
        int 21h
        _endif

        inc di
        inc di
        mov dx, di
        _do
        inc di
        js RInitError
        cmp [di - 1], ah
        _enddo jne
        mov si, ROffFileAccessEM
        push dx
        push si
        mov si, dx
        mov bx, 20h
        xor cx, cx
        mov dx, 1
        mov ax, 716Ch
        stc
        int 21h
        pop si
        pop dx
        _ifnot jnc
          mov ax, 3D20h           ;read only, deny write
                                 ;the file may be opened by other instance
                                 ;with same open mode only
          int 21h
          jc RInitError
        _endif
        push es
        pop ds
        add [ROffEnvSize + PSP], di
        mov [ROffFileHandle + PSP], ax
        pushad
        mov bx, ax
        mov ax, 4202h
        mov cx, -(1)
        mov dx, -(14)
        int 21h
        jc CfgDone
        sub sp, 14
        push ss
        pop ds
        mov si, sp
        mov dx, si
        mov cx, 14
        mov ah, 3Fh
        int 21h
        jc CfgFree
        cmp ax, 14
        jne CfgFree
        cmp dword [si], 'ZRXC'
        jne CfgFree
        add si, 4
        mov di, InitFlags + PSP
        mov cx, 5
        cld
        rep movsw
CfgFree:
        add sp, 14
        push es
        pop ds
CfgDone:
        mov ax, 4200h
        xor cx, cx
        xor dx, dx
        int 21h
        popad
        shr byte [InitFlags + PSP], 1    ;test for intro banner
        _ifnot jc
          mov dx, ROffIntroMsg + PSP ;PLShift
          mov ah, 9
          int 21h
        _endif

;------------ Setup linear disp for all references to 16Stub -----------------
        xor ebx, ebx
        push cs
        pop bx
        shl ebx, 4
        mov si, -(RRTEntryes) * 2
        _do
        mov di, [si + SegStartRelocR0 + (RRTEntryes * 2) + PSP]
        inc si
        add [di], ebx
        inc si
        _enddo jnz
        mov ax, [ROffTransferBufferPSize + PSP]  ;patch transfer buffer size
        mov [ROffPatchPointTStSz - 2 + PSP], ax  ;in loader code
        shr byte [InitFlags + PSP], 1    ;DPMI/VCPI check sequence?
        _ifnot jnc
          call TryVCPI@167
          call TryDPMI@167
        _else jmp
          call TryDPMI@167
          call TryVCPI@167
        _endif
TryXMS@167:
;look for PE bit in cr0 - must be cleared for RAW/XMS!
        smsw ax
        shr ax, 1
        mov si, ROffAlreadyVMEM
        jc RInitError
;patch 2 points for working under RAW/XMS
        mov word [VCPIRICall + PSP], JmpShortCode + 1 * 100h
        mov word [PatchPoint + PSP], JmpShortCode + (RawSwitcherToPM - PatchPoint - 2) * 100h
        call FindXMSServer
        jnz near TryRAW@167
        mov ah, 5
        call far [ROffXMC + PSP]         ;local enable A20
        or ax, ax
        _ifnot jnz               ;extended memory not available if impossible to enable A20
          dec byte [ROffXMSBlockNotAllocated + PSP]  ;disable XMS block allocation
          jmp InitRX
        _endif
        mov word [PatchPoint1 + PSP], 05B4h    ;"mov ah, 5"
;change gate descriptor for PM->RM mode switch in XMS/RAW
InitRX:
        mov dword [ROffVCPICallDesc + PSP], ROffL0234 + ((VCPISelector + 8) << 16)
        mov byte [ROffVCPICallDesc + PSP + 6], 0
        jmp InitRXV
FindXMSServer:
;check for XMS server
        mov ax, 4300h
        int 2Fh
        cmp al, 80h                ;is XMS available ?
        _ifnot jne
          mov ax, 4310h
          int 2Fh                  ;get XMS entry - always succefful
          mov [ROffXMC + PSP], bx
          mov [ROffXMC + PSP + 2], es
          inc byte [ROffXMSBlockNotAllocated + PSP]  ;enable XMS block allocation
          xor bx, bx        ;clear ZF
        _endif
        ret
TryVCPI@167:
;@@VTest:                    ;in V86 mode int 67h shall be supported anywhere
        mov ax, 0DE00h     ;else V86 monitor is incorrect
        xor ebp, ebp
        mov fs, bp
        push cs
        push ROffint67Handler
        mov di, 67h * 4
        cmp [fs:di], ebp
        pop ebp
        _ifnot je
          int 67h
        _else jmp
          xchg [fs:di], ebp
          int 67h
          mov [fs:di], ebp
        _endif
VETest@167:
        or ah, ah
        _ifnot jz
          ret
        _endif
;---------------------- init DPMI host under VCPI----------------------------
        inc byte [ROffVCPIMemAvailable + PSP]
        call FindXMSServer
        mov ax, 0DE03h
        int 67h
        mov [ROffTotalVCPIPages + PSP], edx
;------------------- init DPMI host under VCPI/XMS/RAW -----------------------
InitRXV:
        push ds
        pop es
%ifdef VMM
;----------------- open swap file -------------------
          mov dx, ROffSwapFileName + PSP
          mov ah, 3Ch
          mov cx, 20h
          int 21h            ;create swap file
          mov si, OffSwapFileEM
          jc RInitError
          mov [ROffswap_file_handle + PSP], ax
          xchg bx, ax
          mov dx, 1
          xor cx, cx
          mov ax, 4200h
          int 21h             ;seek to 1
%endif
;save all interrupt vectors
        mov di, ROffSavedRealVectors + PSP
        xor si, si
        mov fs, si
        mov ax, si
        mov cx, 256 * 2
        rep fs movsw
        mov cx, (OffLastInit - OffSavedRealVectors) / 2
        rep stosw
PatchPoint7:
        jmp short PatchPoint7End  ;this code may by replaced with "mov eax,"
        ;mov  eax, 0
        ;org  $-4
        dw int15handler, DGROUP16
        xchg [fs:(15h * 4)], eax
        mov [ROffOldInt15 + PSP], eax
PatchPoint7End:

        mov di, ROffGDT + FirstGateSelector + PSP
        mov si, Traps3SetupTable + PSP
        mov bx, ROffFirstTrap3 + PSP
        mov cx, NTraps3
        _do
        lodsb
        mov dl, al
        _do
        ;-- set PL3 hook code for gates --
        mov byte [bx], CallFarCode
        lea ax, [di - ROffGDT - PSP + 3]
        mov [bx + 5], ax
        mov [bx + 3], dl
        add bx, 4
        inc dl
        _enddo jnz

;------------------------- set gate descriptor in GDT -----------------------
        movsw               ;low word of entry disp
        mov ax, Code1Selector
        stosw               ;selector
        mov ah, (0E0h + SS_GATE_PROC3)
        lodsb               ;load dword count
        stosw               ;access rights: DPL = 3
        mov ax, KernelBase >> 10h
        stosw               ;high word of entry disp
        _enddo loop
        ; -- special set trap selector for int 31h --
        mov word [ROffFirstTrap3 + PSP + (4 * 31h) + 5], DPMIEntryGateSelector
        mov byte [dword ROffFirstTrap3 + OffPMSaveStateTrap3 + 7 + PSP], RetfCode

;-------------------------- init some TSS fields ------------------------
        mov dword [ROffTSS + PSP + TSS_ESP0], OffKernelStack
        mov dword [ROffTSS + PSP + TSS_ESP1], OffKernelStack1End
        mov word [ROffTSS + PSP + TSS_SS0], Data0Selector
        mov word [ROffTSS + PSP + TSS_SS1], Data1Selector
        mov word [ROffTSS + PSP + TSS_IOBASE], 0FFFFh

;----------------------------- Fill  client IDT ------------------------------
        mov di, ROffClientIDT + PSP
        mov dx, OffDefIntTrap3
        _do
        mov [di], dx
        mov word [di + 4], Trap3Selector
        mov [di + ROffIDT - ROffClientIDT + 0], dx
        mov word [di + ROffIDT - ROffClientIDT + 2], Trap3Selector
        mov dword [di + ROffIDT - ROffClientIDT + 4], (0E0h + SS_GATE_TRAP3) << 8
        add di, 8
        add dx, 4
        dec cl
        _enddo jnz
;------------------------ fill default exceptions table ---------------------
        mov cl, 16
        mov eax, OffDefaultExcTrap3
        mov di, ROffClientExc + PSP
        _do
        mov word [di + 4], Trap3Selector
        mov [di], eax
        add ax, 4
        add di, 8
        _enddo loop
;------------------------ Init hardware interrupts ---------------------------
        mov ax, 0DE0Ah
        call VCPIRICall
        mov bh, cl
        ;mov HardwareIntMapR[PSP], bx
        mov esi, OffHIntHandlers & 0FFFFh
        ;mov dx, JmpShortCode + ((23*4) shl 8)
        mov ax, 8
        movzx di, bl
        call Init8Handlers
        mov al, 70h
        movzx di, bh
        call Init8Handlers
        xor di, di
        mov ax, di
        call Init8HandlersAbs
        call Init8HandlersAbs
;--------------------- generate code for real mode hooks ----------------
        mov di, OffFirstSwitchCode + PSP
        mov ax, ROffRMS_RMHandler - ROffFirstSwitchCode - 3
        mov cx, (MaxSystemSwitchCode + nMaxCallbacks * 4) / 4
        _do
        mov byte [di], NearCallCode
        mov [di + 1], ax
        add di, 4
        sub ax, 4
        _enddo loop
;------- calculate top of dos memory area, may be used by dpmi host --------
        mov ah, 48h
        mov bx, -(1)
        int 21h
        mov ax, [ROffTransferBufferPSize + PSP]
        add ax, [ROffMemReserve + PSP]
;ax - total size to reserve
        mov cx, [2]       ;top of task memory
        cmp bx, ax
        _ifnot jae
          sub cx, ax         ;decrase top if no another memory for transfer buffer
          _ifnot ja
            xor cx, cx
          _endif
        _endif
        push cx
;--------------------- hook autopassup interrupts ---------------
        mov di, ROffAutoPassupRJmps
        xor si, si
        mov cx, 100h
        _do
          bt word [ROffPassupIntMap + PSP], si
          _ifnot jnc
            shl si, 2
            cli
            mov eax, [fs:si]
            mov byte [cs:di], JmpFarCode
            mov [cs:di + 1], eax
            mov [fs:si + 2], cs
            mov [fs:si], di
            add di, 5
            sti
            shr si, 2
          _endif
          inc si
        _enddo loop
;---------------------- install terminate handler --------------------------
        db 66h, 0B8h
        dw TerminateRHandler
        dw DGROUP16
        xchg [0Ah], eax
        mov [ROffOldInt22 + PSP], eax
        pop cx
        jmp CriticalInitVCPI@167

TryRAW@167:
        mov ah, 88h
        int 15h
        movzx ebp, ax
        or ax, ax
        _ifnot jz, near
        cli
        call IsA20Enabled
        _ifnot je
PS2@167:
        mov cx, 5000
        in al, 92h
        test al, 2
        jnz NotPS2@167
        or al, 2
        jmp $ + 2
        jmp $ + 2
        out 92h, al
        _do
        jmp $ + 2
        jmp $ + 2
        in al, 92h
        test al, 2
        _enddo loopz
        ;jz   @@NotPS2
        call IsA20Enabled
        je A20OK@167
NotPS2@167:
        mov si, ROffA20EnableSq + PSP
        _do jmp
        movzx dx, al
        jmp $ + 2
        outsb
        _while
        xor cx, cx
        _do
        jmp $ + 2
        jmp $ + 2
        in al, 64h
        test al, 2
        _enddo loopnz
        jnz A20Err@167
        lodsb
        or al, al
        _enddo jnz
        mov dx, [gs:46Ch]    ;timer counter
        _do                   ;wait for A20 about 2 timer ticks
          call IsA20Enabled
          je A20OK@167
          sti
          mov ax, [gs:46Ch]
          sub ax, dx
          cli
          cmp ax, 2
        _enddo jb
A20Err@167:
        sti
        xor ebp, ebp
        jmp RawPatch@167
        _endif
A20OK@167:
        sti
        mov di, 1
        mov edx, 100000h       ;default value for top of the extended memory
        call CheckVDisk
        _ifnot jnz
          mov dx, [es:di + 2Eh - 1] ;vdisk top in k
          shl edx, 10          ;convert to bytes
        _endif
        les di, [gs:di - 1 + (19h * 4)]
        ;add  di, 12h
        call CheckVDisk
        _ifnot jnz
          mov eax, [es:di + 2Ch]
          and eax, 0FFFFFFh
          cmp eax, edx
          _ifnot jbe
            xchg eax, edx
          _endif
        _endif
        _endif
;ebp - size of mem window, <= 0FFFFh
;edx - top of mem window
        mov eax, [ROffMaxXMSAllocate + PSP]
        ;shr  eax, 10                        ;convert to kilobytes
        cmp eax, ebp
        _ifnot jb
          mov eax, ebp                     ;now hi word of eax is undefined!
        _endif
        sub bp, ax                         ;bp = leaved_mem_size
        xchg ax, bp                         ;move allocation size to ebp
        mov [ROffPatchPoint_int15 + 1 + PSP], ax  ;set new extended size, returned by my int 15 handler
        shl eax, 10
        add edx, eax                       ;shift my memory base to bottom of the window
RawPatch@167:
        mov word [PatchPoint7 + PSP], 0B866h       ;mov eax, ??????
;@@NoA20Patch:
        ;mov  word ptr PatchPoint1[PSP], JmpShortCode + ((Exit2-PatchPoint1-2) shl 8)
        mov bx, ROffFreeXMSCount + PSP
        call TranslateMemLimits
        jmp InitRX
CheckVDisk:
        cmp dword [es:di + 12h], 'VDIS'
        _ifnot jne
          cmp byte [es:di + 12h + 4], 'K'
        _endif
        ret
        LLabel A20EnableSq
        db 64h, 0D1h, 60h, 0DFh, 64h, 0FFh, 0

IsA20Enabled:
        xor bx, bx
        mov gs, bx
        dec bx
        mov es, bx
        inc bx
        mov al, [es:bx + 10h]
        mov ah, al
        inc al
        xchg al, [gs:bx]
        cmp ah, [es:bx + 10h]
        mov [gs:bx], al
        ret


;al - unmapped real mode interrup number
;di - mapped interrupt number
Init8Handlers:
        test di, 111b
        jne near HIntMapError@169
        push di
        shr di, 3
        mov byte [di + ROffPassupIntPMap + PSP], 0FFh
        pop di
        cmp di, 18h
        _ifnot jae
          cmp di, 8
          jne HIntMapError@169
          ret
        _endif
Init8HandlersAbs:
        _do
        mov byte [si + ROffHIntHandlers + PSP - (OffHIntHandlers & 0FFFFh)], PushBCode
        mov [si + ROffHIntHandlers + PSP - (OffHIntHandlers & 0FFFFh) + 1], al

        mov ecx, (OffInterruptH & 0FFFFh) - 7       ;only interrupt
        cmp di, 10h
        _ifnot jae
          mov cx, (OffException0DHandler & 0FFFFh) - 7
          cmp di, 0Dh
          _toendif je
%ifdef VMM
            mov cx, (OffException0EHandler & 0FFFFh) - 7
            cmp di, 0Eh
            _toendif je
%endif
          mov cx, (OffExceptionOrInterruptH & 0FFFFh) - 7
          bt word [ROffPassupIntPMap + PSP], di
          _ifnot jc
            mov cx, (OffExceptionWithCodeH & 0FFFFh) - 7
            cmp di, 8
            _ifnot jae
              cmp di, 2
              je NotExc@169
              mov cx, (OffExceptionWOCodeH & 0FFFFh) - 7
            _endif
          _endif
        _endif
        mov byte [si + ROffHIntHandlers + PSP - (OffHIntHandlers & 0FFFFh) + 2], JmpNearCode
        sub ecx, esi
        mov [si + ROffHIntHandlers + PSP - (OffHIntHandlers & 0FFFFh) + 3], ecx
        shl di, 2
        mov [di + ROffFirstTrap3 + PSP + 3], al   ;set new redirected real mode number
        shl di, 1
        mov [di + ROffIDT + PSP], si
        mov word [di + ROffIDT + PSP + 2], Code0Selector
        mov word [di + ROffIDT + PSP + 4], (0E0h + SS_GATE_INT3) << 8
        mov word [di + ROffIDT + PSP + 6], (OffHIntHandlers >> 16)
        add si, 7
        shr di, 3
NotExc@169:
        inc di
        inc ax
        test di, 111b
        _enddo jnz
        ret
HIntMapError@169:
        mov si, ROffIntMapEM
        jmp RInitError


        ESEG IText16
