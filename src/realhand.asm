;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

SEGM Text16
;assume cs:dgroup16, ds:nothing, es:nothing, ss:nothing
assume cs:Text16, ds:nothing, es:nothing, ss:nothing
LLabel MouseRHandler
DW     20
DB     'ZRDX0.50'
InitFlags DW 2
LWord TransferBufferPSize
        DW 0400h ;400h
LDWord MaxXMSAllocate
        DD 0ffffffffh
LWord MemReserve
        DW 0F000h ;0F000h
        DW 0
LByte MouseBusyFlag
        DB 1           ;mouse busy flag
LLabel MouseRHandlerEntry
        shr  byte ptr cs:MouseBusyFlagR, 1
        $ifnot jnc
        db   JmpFarCode
LDWord MouseCallbackPlace
        DW 0, 0
        mov  byte ptr cs:MouseBusyFlagR, 1
        $endif
        retf
LLabel MouseRHandlerEnd
MouseRHandlerPSize = (ROffMouseRHandlerEnd - ROffMouseRHandler+15)/16
MouseRHandlerSize = MouseRHandlerPSize*16
LWord OldInt22
      DW ?,?
LDWord XMC             ;address of XMS entry point
        DD ?
LWord XMHandle         ;handle of XMS memory block
        DW 0      ;NULL if no XMS memory allocated
LDWord FreeXMSCount
        DD 0
LDWord FirstFreeXMS
        DD 0
LDWord TotalXMSPages
        DD 0
LWord DefIDT           ;default IDT for real mode
        DW 3FFh, 0, 0
LDWord PMCR0
        DD 0
LDWord RMCR0
        DD 0
;protected mode IDT
LWord IDTRSize
        DW 100h*8-1    ;IDT always full
LWord IDTRBase
        DD OffIDT      ;fixed address
;protected mode GDT
LWord GDTRSize
        DW OffGDTEnd-OffGDT-1
LDWord GDTRBase
        DD OffGDT      ;fixed address
;pointer to current real mode stack for DPMI client
LDword RMStack
        DW OffRStackEnd
        DW seg DGroup16
;pointer to current protected mode stack for DPMI client
LDWord PMStack
        DD ?, ?
LLabel SwitchTable
LDword SwitchTableCR3
        DD ?             ;Must be set during init
SwitchTableGDT DD OffGDTRSize
;FirstRelocR = $ - 4      ;First relocation address in module
;LastRelocR  = $
        RRT      ;place it to real mode relocation table
SwitchTableIDT DD OffIDTRSize
        RRT      ;too
SwitchTableLDTR DW LDTSelector ;
SwitchTableTR   DW TSSSelector
LDWord SwitchTableEIP
        DD OffPMInit   ;fixed offset of first PM entry, shell be
                               ;changed by real mode handlers
SwitchTableCS   DW Code0Selector
LByte   HasExitMessage
        DB 0
int15handler:
        cmp ah, 88h
        $ifnot je
        DB JmpFarCode
LLabel OldInt15
        DD 0
        $endif
LLabel PatchPoint_int15
        mov  ax, 0
        push bp
        mov  bp, sp
        and  byte ptr ss:[bp+6], not 1  ;clear carry
        pop  bp
LLabel int67Handler
        iret

DPROC AllocXMSBlock
        push cs
        pop  ds
        ;allocate and lock lagest available block
        mov  ah, 88h
        call @@XMSICall           ;get free mem v3+
        or   bl, bl
        $ifnot je
          mov  ah, 8
          call @@XMSICall         ;get free mem v2-
          movzx eax, ax
        $endif
        mov  ebp, MaxXMSAllocateR
        cmp  ebp, eax
        $ifnot jb
          xchg  ebp, eax
        $endif
        mov  edx, ebp
        or   edx, edx
        $ifnot je
          mov ah, 89h               ;alloc block v3+
          call @@XMSICall
          $ifnot jne
            mov  ah, 9h             ;alloc block v2-
            call @@XMSICall
            jz   @@NoXMS
          $endif
          mov  XMHandleR, dx   ;save handle
          mov  ah, 0Ch
          call @@XMSICall           ;lock block
          $ifnot jnz
            mov  ah, 0Ah
            mov  dx, XMHandleR
            call @@XMSICall         ;free not succefully locked block
          @@NoXMS:
            xor  ebp, ebp
          $endif
          shl  edx, 16
          mov  dx, bx               ;prepare 32 bit pointer in edx
        $endif
        mov  bx, ROffFreeXMSCount
        call TranslateMemLimits
        iret

;ebp - mem size in B, edx - mem base in B
;
TranslateMemLimits:
        shl  ebp, 10
        add  ebp, edx
        rcr  ebp, 1                 ;in case of 4G upper bound:-)
        shr  ebp, 11
        add  edx, 0FFFh
        shr  edx, 12
        sub  ebp, edx
        $ifnot jae
          xor  ebp, ebp
        $endif
        mov  ds:[bx], ebp
        mov  ds:[bx][ROffTotalXMSPages-ROffFreeXMSCount], ebp
        mov  ds:[bx][ROffFirstFreeXMS-ROffFreeXMSCount], edx
        retn
@@XMSICall:
        call XMCR
        or   ax, ax
        retn
        ENDP

;warning - this code executed after program termination
;in a free dos memory block on small and unstable stack
TerminateRHandler:
        pushf
        cli
        push ax bx
        mov  ax, ss
        mov  bx, sp
        push cs
        pop  ss
        mov  sp, ROffExitStackEnd
        TSFrameSize = 8*4+4*2
        push ds es fs gs
        pushad
        push TerminateSwitchCode+3;push switch code for RMS handler
        jmp RMS_RMHandlerL        ;go to PM terminate handler
;continued after DPMI host are cleaned
TerminateRHandler2:

        $do     ;free VCPI pages with DPMI host internal code&data
          pop  edx
          test dh, VCPIPageBit
          $ifnot jz
            and  dx, 0F000h
            mov  ax, 0DE05h
            int  67h
          $endif
        $enddo loop
ExitXMS:
        mov  ah, 0Dh              ;unlock
        mov  dx, XMHandleR
        or   dx, dx
        $ifnot jz
          call XMCR               ;unlock and free XMS if handle is not null
          mov  ah, 0Ah            ;free
          call XMCR
        $endif
PatchPoint1:
        ;mov  ah, 05H            ;local disable A20 always
        jmp  short Exit2         ;may be patched with "mov ah, 05h" (B4 05)
                                 ;if A20 local enabled with XMS
        call XMCR
Exit2:
        cmp HasExitMessageR, 0
        $ifnot je
        IFNDEF Release
          mov ax, 3
          int 21h
        ENDIF
        mov dx, ROffRStackStart+4
        mov ah, 9
        int 21h        ;display exit message
        $endif
        mov sp, ROffExitStackEnd - TSFrameSize
        popad
        pop gs fs es ds
        mov ss, ax     ;restore dos stack
        mov sp, bx
        pop bx ax
        popf
        jmp dword ptr cs:OldInt22R  ;chain to next int 22 handler

LLabel Int214C
        mov  ax, 4CFFh
        int  21h
UnexpectedPMError:
        sti
        jmp $
;ESEG Text16
;Segm Text
;assume cs:Text
;ENDP

DPROC RMSaveState
        push si di ds es
        pushf
        mov  si, ROffRMStack
        or   al, al
        cld
        push cs
        $ifnot jne
          pop  ds
        $else jmp
          xchg si, di
          push es
          pop  ds
          pop  es
        $endif
        rept 5
        movsw
        endm
        popf
        pop es ds di si
        retf
        ENDP
;emulator of switch to VM86, really switch to RM with same stack frame
;ESeg Text
;Segm Text16
;assume cs:Text16
DPROC RawSwitchToRM1
LLabel  L0234
        push eax
        mov  ax, VCPISelector+16
        mov  ds, ax
        mov  byte ptr ss:GDT[TSSSelector][5], 80h+SS_FREE_TSS3 ;mark current TSS as FREE
        mov  es, ax
        mov  gs, ax
        mov  dword ptr ds:RStackStartR, ebx
        mov  fs, ax
        pop  ebx
        mov  ss, ax
        mov  esp, ROffRStackStart+40h
        ror  ebx, 4               ;convert linear to segment
        mov  eax, ds:RMCR0R
        and  eax, not 80000000h   ;disable paging in protected mode
        mov  cr0, eax
        xor  eax, eax
        mov  cr3, eax             ;clear cr3 in protected mode
        mov  eax, cr0
        and  al, not 1
        lidt qword ptr ds:DefIDTR
        mov  cr0, eax             ;switch to real mode
        DB   jmpFarCode
        DW   ROffL0235
        DW   seg dgroup16
        F = 0
LLabel L0235
        mov  ds,  bx
        shr  ebx, 28
        lss  esp, fword ptr ds:[bx+F].VMI_ESP
        mov  es,  word ptr  ds:[bx+F].VMI_ES
        mov  fs,  word ptr  ds:[bx+F].VMI_FS
        mov  gs,  word ptr  ds:[bx+F].VMI_GS
        mov  ds,  word ptr  ds:[bx+F].VMI_DS
        mov  ebx, dword ptr cs:RStackStartR
;this code always entered after PM to VM VCPI switch to restore eax, flags
;and jump to entry point
pop_eax_iret:
        pop  eax
        iret
ENDP

nPassups = 20
LLabel AutoPassupRJmps
        DB  5*nPassups dup(?)  ;reserve space for DPMI autopassup traps

;RM handler for switch from VM to PM
;must be placed on bottom of
LLabel RMS_RMHandler
RMS_RMHandlerL:
        push gs fs ds es       ;save all registers, are not transferred
        push eax ebp esi       ;by VCPI
        pushf
SwitchToPM:
        cli
        mov  bp, ss
        shl  ebp, 16
        mov  bp, sp             ;store RM ss:sp to ebx
PatchPoint:
        mov  esi, OffSwitchTable
        RRT
        mov  ax, 0DE0Ch
        int  67h

;raw switcher to PM from RM with same format, as VCPI
RawSwitcherToPM:
comment ^
;---------- check CPU mode -----------
        smsw ax
        xor si, si
        test al, 1
        jnz  UnexpectedPMError  ;processor already in virtual mode
;------------------------------ check A20 -----------------------------------
        mov  ds, si
        dec  si
        mov  es, si
        inc  si
        mov  ax, es:[si+10h]      ;FFFF:10 == 0:0
        push word ptr ds:[si]    ;save 0:0
        dec  ax
        mov  ds:[si], ax
        cmp  ax, es:[si+10h]      ;write to 0:0 is reflected on FFFF:10 ?
        pop  word ptr ds:[si]    ;restore 0:0
        je  UnexpectedPMError    ;A20 was disabled  ^
;-------------------------- load system tables ------------------------------
        push cs
        mov  si, ROffSwitchTable+12
        pop  ds
        pushf                    ;set IOPL 3 and NT 0
        mov  eax, dword ptr ds:[si-12]
        mov  CR3, eax
        lgdt qword ptr ds:[si+ROffGDTRSize-ROffSwitchTable-12]
        pop  ax
        ;or   ah, 30h
        and  ah, not 40h
        push ax
        popf
        mov  eax, CR0
        or   eax, 80000001h      ;enable paging and PM
        lidt qword ptr ds:[si+ROffIDTRSize-ROffSwitchTable-12]
        mov  CR0, eax
        DB   JmpFarCode
        DW   ROffRawSwitchPMEntry0
        DW   VCPISelector+8
LLabel RawSwitchPMEntry0
        lldt ds:[si]
        ltr  ds:[si+2]
        jmp  fword ptr ds:[si+4]
ESeg Text16
MaxSystemSwitchCode = 5*4
nMaxCallbacks = 20
VSegm BSS16
        DFB FirstSwitchCode, MaxSystemSwitchCode+nMaxCallbacks*4
RMIretSwitchCode      = 0+ROffFirstSwitchCode
Dos1CallRetSwitchCode = 4+ROffFirstSwitchCode
RMIERetSwitchCode     = 8+ROffFirstSwitchCode
TerminateSwitchCode   = 12+ROffFirstSwitchCode
RawSwitchCode         = 16+ROffFirstSwitchCode
FirstPassupSwitchCode = MaxSystemSwitchCode+nMaxCallbacks*4
        DFB RStackStart, 600
        VSAlign 16
        DFL ExitStackEnd
        DFL RStackEnd
        DFL SwapBuffer
EVSeg BSS16


