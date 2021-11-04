;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

;Protected mode handlers for DPMI host
;handler for:
;hardware interrupts from PM                 PMHIntHand
;hardware interrupts from RM                 RMHIntHand
;default software interrupts from PM         PMSDefIntHand
;default hardware interrupts from PM         PMHDefIntHand
;iret from hardware interrupt                PMHIretHand
;iret from first hardware interrupt          PMH1IretHand
;iret form hardware interrupt to host
;retf from PM hook procedure                 PMHookHand
;iret from RM hardware interrupt redirection RMHIretHand
;iret from RM software interrupt redirection RMSIretHand
;leave RM for translation service            RMTransRetHand

;raw mode switch from RM                     RMRawSwitchHand
;comments:
;   int 31h hooked in module DPMI
;   software interrupts traps points immediately to ring 3 and not hooked

SEGM Text
;assume cs:DGroup, ss:DGroup, ds:DGroup, es:nothing
assume cs:Text, ss:nothing, ds:nothing, es:nothing
;switch stack and return
comment %
DPROC Iret2FirstPMTrapH
        push ss:FirstSS     ;for first interrupt
        push ss:FirstESP    ;cannot change one of this values
        push ss:FirstFlags
        push ss:FirstCS
        push ss:FirstEIP
        mov  ss:FirstCS, 0
        iretd
        ENDP
        %
;default exception handler - terminate program
DPROC DefaultExcTrapH
        cli
        mov  [esp][8], ebp
        pop  ebp
        add  esp, 4
        movzx ebp, byte ptr ss:FirstTrap3[ebp+3-7];load redirected int number
        and  ebp, 0Fh
        mov  [esp][4], ebp    ;save error code on the stack
        jmp  FatalExc1
ENDP
;return from exception - switch to
DPROC RetFromExcTrapH
        add esp, 3*4    ;drop return address and errcode
        iretd
ENDP

;Hardware interrupts gate for interrupts in 0-20h
;stack frame:
              ;esp   ;if not from 0
              ;ss
              ;eflags
              ;cs
              ;eip
              ;error code(for exceptions)
              ;int/exception number
              ;saved ebp
DPROC PMHIntHandX

LLabel ExceptionWOCodeH              ;the entry for exceptions without error code
        push dword ptr ss:[esp]      ;push exception number again
        and  dword ptr ss:[esp+4], 0 ;store 0 as error code for common format
LLabel ExceptionWithCodeH            ;the entry for exceptions without error code
ExceptionWithCodeHL:
@@ExceptionWithCodeH:
        push ebp
        cmp  esp, OffKernelStack-8*4
        jne   ExceptionFrom0
ExceptionFromNot0:
        test byte ptr ss:[esp][4*4], 2  ;check rpl in return CS
        jz   FatalExc
        push es edi esi ds
        push ss
        pop  ds
        mov  esi, OffKernelStack-4
        les  edi, fword ptr ds:[esi][-4]  ;load client ss:esp
        sub  edi, 4+20
        std
        REPT 6
        movsd      ;move exception frame to the client stack
        ENDM
        mov  dword ptr es:[edi], Trap3Selector   ;store to the client stack
        sub  edi, 4                    ;return address
        mov  dword ptr es:[edi], OffRetFromExcTrap3
        pop  ds esi
        push es
        push edi
        imul edi, ss:[esp][4*4+4], 8       ;Exception number * 4
        push ss:ClientExc[edi][4]
        push ss:ClientExc[edi]
        les  edi, ss:[esp+4*4]    ;restore original es:edi from kernel stack
        retf                      ;switch to client excepyion handler
ExceptionFrom0:  ;XXX
FatalExc:
        cmp  byte ptr [esp+4], 1
        $ifnot jne
          F = 12
          and  [esp+F][1].IFrFlags, not 1      ;clear TF
          pop  ebp
          add  esp, 8
          iretd
        $endif
FatalExc1:
        F = 16
        ;Log
        ;hlt
        push dword ptr [esp+4]  ;exception number
        push dword ptr [esp+F].IFrCS
        push dword ptr [esp+F+4].IFrSS
        push es
        push ds

        push ss
        pop  ds
        push ss
        pop  es

        push dword ptr [esp+F+16].IFrEIP
        pushfd
        push ebp
        mov ebp, [esp+F+28].IFrESP
        push ebp
        push edi
        push esi
        push edx
        push ecx
        push ebx
        push eax

        ;Log
        IFDEF Release
        mov  PrintToMem,1
        mov  HasExitMessage, 1
        RRT  1
        mov  edi, OffRStackStart+4
        RRT
        ELSE
        mov  edi, 0B8000h
        ENDIF
        mov  ecx, 4
        lea  esi, [esp+40]
        cld
        $do
        lodsd
        and eax, 1F8h
        push dword ptr LDT[eax]
        push dword ptr LDT[eax].4
        push 8
        call PrintNX
        push 8
        call PrintNX
        $enddo loop
        mov ax, 0D0Ah
        stosw
        mov  ecx, 8
        $do
        push 8
        call PrintNX
        $enddo loop
        mov ax, 0D0Ah
        stosw
        mov  cx, 2
        $do
        push 8
        call PrintNX
        $enddo loop
        comment #
        movzx eax, dx
        and  al, not 7h
        and  eax, 0FFh
        push dword ptr LDT[eax]
        push dword ptr LDT[eax].4
        push 8
        call PrintNX
        push 8
        call PrintNX
                #
        mov cx, 5
        $do
        push 4
        call PrintNX
        $enddo loop
        ;mov  cl, 8
        ;sub  esp, 12
        ifndef Release
        push ExtraDW
        push 8
        call PrintNX
        endif
        ;IFNDEF Release
        ;hlt
        ;ENDIF
        mov al, '$'
        stosb
        ;mov al, 20h
        ;out 20h, al
        ;out 0A0h, al
        L098:
IFNDEF Release
        mov  PrintToMem,0
ENDIF
        ;Log
        mov  bx, ROffInt214C
        mov  esp, ROffExitStackEnd-NExitPages*4-TSFrameSize
        RRT
        jmp SimpleSwitchToVM

LLabel Exception0DHandler
        cmp  esp, OffKernelStack-6*4
        je   @@InterruptH
        F  = 8
        ;test [esp+F].IFrCS, 2         ;exception from kernel(CPL < 2)?
        ;jz   @@ExceptionWithCodeH
        shr  ss:Exception0DFlag, 1
        $ifnot jc
          mov  esp, ss:Exception0DStack
          jmp  @@ExceptionWithCodeH
        $endif
        mov  ss:Exception0DStack, esp
        push esi ds
        lds  esi, fword ptr ss:[esp+F+8].IFrEIP         ;cs:eip
        push eax
        mov  eax, ds:[esi]
        cmp  al, 0Fh
        jne  @@ToCommonTrap
        ;mov  ah, ds:[esi+1]
        cmp  ah, 20h
        jb   @@ToCommonTrap
        cmp  ah, 23h
        ja   @@ToCommonTrap
        shl  eax, 8
        ;mov  al, ds:[esi+2]
        mov  al, 0C3h           ; RetNCode
        ror  eax, 8
        xchg eax, [esp]
        call esp
        add  esp, 4
        mov  esi, cr0
        mov  ss:PMCR0, esi
        RRT
        pop  ds
        pop  esi
        add  [esp+F].IFrEIP, 3
        add  esp, 8                  ;drop exception number and error code
        mov  ss:Exception0DFlag, 1
        iretd
@@ToCommonTrap:
        pop  eax ds esi
        mov  ss:Exception0DFlag, 1
        jmp  @@ExceptionWithCodeH

LLabel ExceptionOrInterruptH     ;the entry for exceptions/interrupts in range 8-F
        cmp esp, OffKernelStack-6*4
        jne @@ExceptionWithCodeH
LLabel InterruptH                ;the entry for interrupts
InterruptHL:
@@InterruptH:
        push ebp
;hardware interrupts from PL0 not allowed
InterruptFromNot0:
        push eax
        F = 12                     ;eax, ebp, int number
        mov  eax, [esp+F].IFrCS    ;
        test al, 10b
        ;IFNDEF Release
        ;$ifnot jnz
        ;  pop eax eax eax
        ;  push 12
        ;  call RegDump
        ;  mov ecx, 1000000000
        ;  loop $
          ;cli
          ;jmp $
        ;  jmp @@ExceptionWithCodeH
        ;$endif
        ;ENDIF
        jz   @@HIntFrom01          ;Interrupt from PL1, not from 2 or 3
        push ds
        F = F+4    ;shift of interrupt frame
IFDEF VMM
        cmp  ss:LockedMode, 0
        $ifnot jne
          mov  ebp, Data3Selector   ;selector of locked stack
          mov  ds, ebp
          sub  KernelStack1, 20     ;reserve space for current iret frame
          mov  ds:LockedMode, 1
          mov  ebp, KernelStack1
          mov  ss:[ebp].IFrCS, eax
          mov  eax, [esp+F].IFrEIP
          mov  ss:[ebp].IFrEIP, eax
          mov  eax, [esp+F].IFrSS
          mov  ss:[ebp].IFrSS, eax
          mov  eax, [esp+F].IFrESP
          mov  ss:[ebp].IFrESP, eax
          ;we don't need to transfer eflags, because it will be transferred
          ;during IretTrapH
          mov  eax, OffLockedStackBottom-12;
          mov  ds:[eax].IFrCS, Trap3Selector
          mov  ebp, OffIretPLTrap3
        $else jmp
ENDIF
          sub  [esp+F].IFrESP, 12
          mov  ebp, eax
          lds  eax, fword ptr [esp+F].IFrESP  ;client ss
          mov  ds:[eax].IFrCS, ebp            ;client cs
          mov  ebp, [esp+F].IFrEIP            ;
IFDEF VMM
        $endif
ENDIF
        mov  ds:[eax].IFrEIP, ebp         ;
        mov  ebp, [esp+F].IFrFlags ;
        mov  ds:[eax].8, ebp       ;
        and  ebp, not 4300h        ;clear  IF and TF and NT
        push ds  ;[esp+F].IFrSS    ;
        push eax                   ;push .IFrESP,
        push ebp                          ;eflags
        movzx ebp, byte ptr [esp+24]      ;load interrup number
        shl  ebp, 3
        push dword ptr ss:ClientIDT[ebp].4
        push dword ptr ss:ClientIDT[ebp]
        mov  ebp, [esp+28]
        mov  eax, [esp+24]
        mov  ds,  [esp+20]
        iretd

;interrupt from level 1
@@HIntFrom01:
;----------------- save all registers in kernel stack ---------------------
        ;Log
        mov  ebp, OffKernelStack
        mov  esp, [ebp-8]         ;get esp for level 1
                      ;exceptions & interrupts not allowed there !
        mov  eax, [ebp-12]      ;eflags for interrupted server code
        push eax
        push dword ptr[ebp-20]  ;eip,
        push edi esi
        push dword ptr [ebp-28] ;ebp from stack
        push ebx edx ecx
        push dword ptr [ebp-32] ;eax from stack

        push ds es fs gs        ;because stack0 is dropped
                                ;push cs not needed, cs == Code1Selector
        movzx ecx, byte ptr [ebp-24] ;get interrupt number
        mov  ebp, ss:KernelStack1
        push ebp           ;save stack1 frame
        ;cmp  esp, XXX     ;test for stack overflow
        ;jb   KernelStack1Overflow
;--------------------------- jmp to client vector --------------------------
        and  ah, not 43h  ;IFBitMask;return to client with server flags and IF=0
        mov  ss:KernelStack1, esp        ;new frame started from there
IFDEF VMM
        cmp  ss:LockedMode, 0
        $ifnot jne
          mov  ebx, Data3Selector
          mov  ds, ebx
          mov  ss:LockedMode, 1
          mov  ebx, OffLockedStackBottom-12
          mov  dword ptr [ebx], OffIret2KernelAsincLTrap3 ;address of trap
        $else jmp
ENDIF
          lds  ebx, ss:[ebp-8]          ;client ss:esp
          sub  ebx, 12
          mov  dword ptr [ebx], OffIret2KernelAsincTrap3 ;address of trap
IFDEF VMM
        $endif
ENDIF
        ;Log
        push ds          ;new client ss
        push ebx         ;new client esp
        push eax         ;new client eflags
        mov  [ebx+8], eax
        mov  dword ptr [ebx+4], Trap3Selector
        push ss:ClientIDT[ecx*8][4]
        push ss:ClientIDT[ecx*8]
        iretd
        ENDP
IFDEF VMM
LLabel Iret2KernelAsincLTrapH
        mov  ss:LockedMode, 0 ;disable locked mode
ENDIF
;PL1 handler
;return to interrupted by hardware int place of server PL1 code
DPROC Iret2KernelAsincTrapH
        cli
        ;Log
        add  esp, 16         ;drop call gate stack frame
        pop  ss:KernelStack1 ;restore level 1 stack pointer in the TSS
        pop  gs fs es ds
        pop  eax ecx edx ebx ebp esi edi
        push dword ptr [esp]         ;push eip again
        mov  dword ptr [esp+4], Code1Selector ;replace it with CS to complete simple iret frame
        ;Log
        iretd                   ;from PL1 to PL1
        ENDP

IFDEF VMM
DPROC   IretPLTrapH
        pushfd                       ;transfer current eflags to old frame
        cli
        pop  [esp+16].IFrFlags
        add  esp, 16                 ;drop current return frame and switch to prev frame
        mov  ss:LockedMode, 0        ;disable locked mode
        add  ss:KernelStack1, 20     ;free frame in kernel stack1
        iretd
        ENDP
ENDIF

;prepare RM stack and switch to
;rewrite return address to client stack
;save client ss:esp
DPROC DefIntTrapH
        push ebx
        push ebp
        pushfd
        cli
        mov  ebp, OffPMStack-4
        RRT
        push eax
        F = 4*4     ;ebx, eax
        ;sub  dword ptr [esp+F].CFrESP, 8
        mov  eax, ds
        mov  ebx, [esp+F].CFrESP
        mov  ds, dword ptr [esp+F].CFrSS
        sub  ebx, 8
        mov  dword ptr ss:[ebp+4][4], ds
        push ss
        mov  ss:[ebp+4][0], ebx
        mov  ds:[ebx].PMI_DS, ax
        mov  dword ptr ds:[ebx].PMI_ES, es
        mov  eax, Data1Selector
        mov  ebp, ds:[ebx].PMI_EFlags
        mov  dword ptr ds:[ebx].PMI_FS, fs
        mov  dword ptr ds:[ebx].PMI_GS, gs
        pop  ds

        @@RSeg  equ EAX
        @@RSegW equ AX
        @@RDisp equ EBX
        @@RDispW equ BX
        mov  @@RSeg, RMStack[0]
        RRT
        movzx @@RDisp, @@RSegW
        shr  @@RSeg, 16
        dec  @@RDispW
        push @@RSeg
        sub  @@RDisp, size VMIStruct-1
        shl  @@RSeg, 4
        add  @@RSeg, @@RDisp
        add  @@RDisp, VMI_EAX
        @@CS equ @@RSeg
        mov  [@@CS].VMI_ESP, @@RDisp
        ;mov  ebx, 0F000h
        xor  ebx, ebx
        pop  [@@CS].VMI_SS
        mov  word ptr [@@CS].VMI_EndFlags, bp  ;flags to real iret frame
        mov  [eax].VMI_DS, ebx
        mov  [eax].VMI_ES, ebx
        mov  ebp, [esp+F].CFrEIP   ;assume call from Trap3 segment
        mov  [eax].VMI_FS, ebx
        mov  [eax].VMI_GS, ebx
        mov  dword ptr [@@CS].VMI_EndIP, 0
        org $-4
        DW   RMIretSwitchCode, seg dgroup16

        movzx  ebx, byte ptr ss:FirstTrap3[OffDefIntTrap3+ebp+3-7];load redirected int number
        bt   RIntFlags, ebx
        $ifnot jc
        mov  ebx, [ebx*4]       ;CS:IP from current real mode vector
        $else jmp
        mov  ebx, SavedRealVectors[ebx*4]  ;CS:IP from saved real vector
        $endif
        mov  dword ptr [eax].VMI_IP, ebx
        pop  dword ptr [eax].VMI_EAX ;write eax for restoring after mode switch
        pop  ebx
        and  bh, not 43h   ;clear IF and TF and NT for initial flags
        pop  ebp
        mov  [eax].VMI_Flags, bx ;write flags for restoring
        pop  ebx
SwitcherToVM:
        VCPICallTrap
        ENDP
DPROC PMSaveStateTrapH
        push esi edi ds es
        pushfd
        mov  esi, OffRMStack
        RRT
        cmp  al, 0
        cld
        push ss
        $ifnot jne
          pop  ds
        $else jmp
          xchg esi, edi
          push es
          pop  ds
          pop  es
        $endif
        movsd
        movsd
        movsd
        popfd
        pop es ds edi esi
        retf
        ENDP

DPROC PMRawSwitchTrapH
        movzx eax, ax
        push  ebp
        pushfd
        cli
        mov  ebp, OffPMStack
        RRT
        push eax
        F = 12
        mov  eax, [esp+F].CFrESP
        mov  ss:[ebp], eax
        mov  eax, [esp+F].CFrSS
        mov  ss:[ebp][4], eax
        movzx ecx, cx
        movzx edx, dx
        push edx           ;RM ss
        shl  edx, 4
        movzx eax, bx      ;esp
        dec  ax
        sub  bx, 10
        sub  eax, size VMIShortStruct-1
        ;jc RStackOverflow
        add  eax, edx
        mov  ebp, eax
        pop  [ebp].VMI_SS
        pop  [ebp].VMI_DS
        pop  edx                 ;init flags
        mov  [ebp].VMI_Flags, dx
        mov  [ebp].VMI_CS, si
        mov  [ebp].VMI_IP, di
        mov  [ebp].VMI_ESP, ebx
        mov  [ebp].VMI_ES, ecx
        pop  ebp
        VCPICallTrap
        ENDP
DPROC VCPICallHandler
        pushfd
        and  byte ptr [esp][1], not 40h    ;clear NT in current eflags
        popfd
        mov  esp, eax
        mov  eax, Data0Selector
        mov  ds, eax
        DB   PushWCode
        DD   seg dgroup16
        mov  eax, RMCR0
        RRT
        mov  cr0, eax
        mov  eax, 0DE0Ch
        DB   PushWCode
        DD   Offset dgroup16:pop_eax_iret
        mov  byte ptr GDT[TSSSelector][5], 80h+SS_FREE_TSS3 ;mark current TSS as FREE
        call fword ptr ds:VCPICall
        ENDP
DPROC VCPITrapHandler
ifndef Release
        ;cmp  al, 5
        ;$ifnot jne
        ;  push edx
        ;  shr  edx, 12
        ;  btr  APages, edx
        ;  pop  edx
        ;  jnc  @@Err
        ;$endif
endif
        ;cmp  al, 4
        ;pushfd
        call fword ptr ds:VCPICall
        ;popfd
ifndef Release
        ;$ifnot jne
        ;  push edx
        ;  shr  edx, 12
        ;  bts  APages, edx
        ;  pop  edx
        ;  jc  @@Err
        ;$endif
endif
        retf
        ENDP
DPROC   LoadLDTHandler
        lldt bp
        retf
        ENDP
DPROC InvalidateTLBHandler
        push eax
        mov  eax, SwitchTableCR3
        RRT
        mov  cr3, eax
        pop  eax
        retf
        ENDP
ENDP
DPROC ExitDPMIHost
;Entry for PL0, switches to PL1
LLabel TerminateHandler
        push Data1Selector
        pop  ds
        push ds
        pop  es
        push ds
        push KernelStack1
        push Code1Selector
        push OffTerminateHandler1
        retf
LLabel TerminateHandler1
        push  Data3Selector     ;save sp for interrup handler
        push  OffUserStackEnd
        mov   RMStack, 0
        org   $-4
        RRT
        DW ROffExitStackEnd-TSFrameSize    ;mov RealStack, dgroup16<<16+ROffExitStackEnd
        DW DGROUP16
;-------------------restore all RM vectors hooked by server --------------------
; and RM vectors hooked by client
        xor  ecx, ecx
        mov  edi, ecx
        dec  cl        ;mov ecx, 0FFh
        mov  ebx, OffDefIntTrap3+4*255
        mov  esi, OffClientIDT
        $do
        mov  dword ptr [esi+ecx*8].4, Trap3Selector
        mov  [esi+ecx*8], ebx
        btr  RIntFlags, ecx
        cmp  ecx, 2Fh
        jbe  @@AbsSet
        bt   dword ptr PassupIntMap, ecx
        $ifnot jnc
          @@AbsSet:
          mov eax, SavedRealVectors[ecx*4]
          mov [edi+ecx*4], eax
        $endif
        sub ebx, 4
        dec ecx
        $enddo jns

;free all pages, allocated by DPMI 800h
        mov ebx, LastMappedPage
        mov edx, 3FFh
        IFNDEF VMM
        call FreeHMPages
        call CleanMemVector
        call ReturnFreePool
        ENDIF
;only for PL0
        mov  esp, ROffExitStackEnd-NExitPages*4-TSFrameSize
        RRT
        cld
        mov  esi, OffPage2
        mov  edi, esp
        xor  ecx, ecx
        mov  cl, NExitPages
        mov  ebp, esp
        rep  movsd            ;move
        mov  cl, NExitPages
        mov  bx, offset DGROUP16:TerminateRHandler2

;entry: esp - VM stack, ebx - VM start ip
SimpleSwitchToVM:
        mov  eax, seg Dgroup16
        pushf
        push ax
        push bx
        sub  esp, 6
        push eax eax eax eax eax  ;fs gs es ds ss
        push large (ROffExitStackEnd-NExitPages*4-TSFrameSize-10) ;esp
        push eax                   ;skip eflags
        mov  eax, esp
        jmp  SwitcherToVM
        ENDP

DPROC PrintByteP
        cmp  cs:PrintToMem, 0
        $ifnot je
          stosb
        $else jmp
          mov ah, 2Fh
          stosw
        $endif
        retn
        ENDP
PrintByte MACRO
          call PrintByteP
          ENDM

;Digit, n
DPROC PrintNX
        push eax ecx
        pushfd
        cld
@@Digit EQU DWORD PTR ss:[esp+12].8
@@N     EQU DWORD PTR ss:[esp+12].4
        ;mov ax, 2F00h+' '
        mov al, ' '
        PrintByte
        ;stosw
        mov eax, @@Digit
        mov cl, 8
        sub cl, byte ptr @@N
        shl cl, 2
        rol eax, cl
        mov ecx, @@N
        $do
        rol eax, 4
        push eax
        and al, 1111b
        add al, '0'
        cmp al, '9'
        $ifnot jbe
        add al, 'A'-'9'-1
        $endif
        ;mov ah, 2Fh
        PrintByte
        ;stosw
        pop eax
        $enddo loop
        popfd
        pop ecx eax
        retn 8
        ENDP

;PM handler for VM->PM   VCPI switch
DPROC RMS_PMHandler
        mov  eax,  Data0Selector
        movzx esi, bp
        mov  ds,  eax
        mov  es,  eax
        mov  ss,  eax
        IFNDEF VMM
          mov  esp, OffKernelStack
        ELSE
          mov  esp, OffKernelStack-4
          push eax                   ;push Data0Selector
        ENDIF
        mov  eax, PMCR0
        RRT
        mov  cr0, eax
        add  bp,  size RMSStruct    ;ajust RM sp to top of normal RM stack
        mov  RMStack, ebp
        RRT
        shr  ebp, 12
        and  ebp, not 1111b
        add  ebp, esi              ;ebp->LA of RMS
        movzx eax, [ebp].RMS_SwitchCode
        sub  eax, offset DGROUP16:FirstSwitchCode+3
        cmp  eax, FirstPassupSwitchCode
        jae  PassupIntHandler
        cmp  eax, MaxSystemSwitchCode
        jae  RMCallbackHandler
        jmp  RMS_JmpTable[eax]
        ENDP
align 4
LDWord RMS_JmpTable
        DD OffRMIretHandler
        DD OffDos1CallRet
        DD OffRMIERetHandler
        DD OffTerminateHandler
        DD OffRMRawSwitchHandler

DPROC RMIretHandler
        lds  esi, fword ptr PMStack
        RRT
        lea  eax, [esi+size PMIStruct]
        push ds
        push eax
        pushfd
        pop  eax
        mov  ax, [ebp].RMS_Flags     ;load transferred flags
        or   ah, 30h                 ;force IOPL=3
        and  ah, not 40h             ;and NT = 0
        push eax
        push [esi].PMI_CS
        push [esi].PMI_EIP
        mov  fs, [esi].PMI_FS      ;restore segment registers from client stack
        mov  gs, [esi].PMI_GS
        mov  es, [esi].PMI_ES
        mov  ds, [esi].PMI_DS
        mov  eax, [ebp].RMS_EAX    ;and transfer RON from RM stack
        mov  esi, [ebp].RMS_ESI
        mov  ebp, [ebp].RMS_EBP
        iretd
        ENDP

DPROC RMRawSwitchHandler
        movzx edx, dx
        push edx
        push ebx
        pushfd
        movzx eax, word ptr [ebp].RMS_ESI
        or   byte ptr[esp+1], 30h     ;set iopl==3
        push eax
        push edi
        mov  es, ecx
        mov  ds, word ptr [ebp].RMS_EAX
        mov  ebp, [ebp].RMS_EBP
        iretd
        ENDP

DPROC RMCallbackHandler
IFDEF VMM
        cmp  LockedMode, 0
        $ifnot jne
          mov LockedMode, 1
          mov esi, PMStack
          RRT
          mov SavedPMStack, esi
          mov esi, PMStack[4]
          RRT
          mov SavedPMStack[4], esi
          mov esi, Data3Selector
          mov es, esi
          mov esi, OffLockedStackBottom-12
          mov [esi].IFrEIP, OffPMCallbackIretLTrap3
        $else jmp
ENDIF
          sub  PMStack, 12
          RRT  1
          les  esi, fword ptr PMStack
          RRT
          mov  es:[esi].IFrEIP, OffPMCallbackIretTrap3
IFDEF VMM
        $endif
ENDIF
        mov  es:[esi].IFrCS, Trap3Selector
        pushfd
        pop  es:[esi].IFrFlags;, eax
        push es
        push esi
        pushfd
        or   byte ptr ss:[esp].1, 30h
        S = MaxSystemSwitchCode*3
        lea  eax, CallbacksTable[eax+eax*2-S]
        movzx esi, [eax].CBT_CS
        push  esi
        push [eax].CBT_EIP
        lds  esi, fword ptr [eax].CBT_SPtrOff
        @@DC equ ds:[esi]
        mov  @@DC.DC_EBX, ebx
        mov  @@DC.DC_ECX, ecx
        mov  @@DC.DC_EDX, edx
        mov  @@DC.DC_EDI, edi
        mov  ecx, [ebp].RMS_EAX
        mov  @@DC.DC_EAX, ecx
        mov  ecx, [ebp].RMS_ESI
        mov  @@DC.DC_ESI, ecx
        mov  ecx, [ebp].RMS_EBP
        mov  @@DC.DC_EBP, ecx
        mov  ecx, dword ptr [ebp].RMS_ES
        mov  dword ptr @@DC.DC_ES, ecx
        mov  ecx, dword ptr [ebp].RMS_FS
        mov  dword ptr @@DC.DC_FS, ecx
        mov  cx,  [ebp].RMS_Flags
        mov  @@DC.DC_Flags, cx
        mov  ecx, ss:RMStack
        RRT
        mov  dword ptr @@DC.DC_SP, ecx
        mov  eax, Data3selector
        push ds
        mov  ds, eax
        mov  edi, esi
        mov  al, 0; Data3selector
        pop  es
        mov  fs, eax
        mov  gs, eax
        lea  esi, [ebp+size RMSStruct]
        iretd
        ENDP

IFDEF VMM
LLabel PMCallbackIretLTrapH
        mov ebp, OffSavedPMStack
        mov eax, ss:[ebp][0]
        mov edx, ss:[ebp][4]
        ;mov ss:LockedMode, 0
        mov byte ptr ss:[ebp][OffLockedMode-OffSavedPMStack], 0
        jmp PMCallbackIretTrapE
ENDIF

DPROC  PMCallbackIretTrapH
        mov eax, [esp].CFrESP
        mov edx, [esp].CFrSS
PMCallbackIretTrapE:
        cli
        push es
        pop  ds
        mov  ebp, OffPMStack
        RRT
        mov  ss:[ebp][0], eax
        mov  ss:[ebp][4], edx
        movzx eax, [edi].DC_SP
        movzx ebp, [edi].DC_SS
        push ebp
        shl ebp, 4
        dec ax
        sub eax, size VMIStruct-6-1
        ;jb @@RStackOverflow
        add ebp, eax
        pop [ebp].VMI_SS
        add eax, VMI_EAX
        mov [ebp].VMI_ESP, eax
        mov ax, [edi].DC_Flags
        mov [ebp].VMI_Flags, ax
        mov edx, dword ptr [edi].DC_IP
        jmp SwitchToVMWithTransfer

        ENDP

;VMS  - struct for switch from VM to PM
;PMI  - client stack frame for interrupts, executed in PM
;RMIE - client stack frame for real mode interrupt emulation (DPMI service 30X)

DPROC RMIERetHandler
        les eax, fword ptr PMStack
        RRT
        lds esi, fword ptr es:[eax].RMIE_EDI
        @@DC equ ds:[esi]
        mov @@DC.DC_EBX, ebx
        mov @@DC.DC_ECX, ecx
        mov @@DC.DC_EDX, edx
        mov ecx, [ebp].RMS_EAX
        mov @@DC.DC_EDI, edi
        mov edx, [ebp].RMS_ESI
        mov @@DC.DC_EAX, ecx
        mov ebx, [ebp].RMS_EBP
        mov @@DC.DC_ESI, edx
        mov ecx, dword ptr [ebp].RMS_ES
        mov @@DC.DC_EBP, ebx
        mov edx, dword ptr [ebp].RMS_FS
        mov dword ptr @@DC.DC_ES, ecx
        mov bx,  [ebp].RMS_Flags
        mov ecx, ss:RMStack
        RRT
        mov dword ptr @@DC.DC_FS, edx
        mov @@DC.DC_Flags, bx

        mov dword ptr @@DC.DC_SP, ecx

        push es                        ;client SS
        push es
        add  eax, size RMIEStruct
        pop  ds
        push eax                       ;new esp
        pushfd
        @@CS equ ds:[eax-size RMIEStruct]
        mov  edx, @@CS.RMIE_RealStack
        mov  ecx, @@CS.RMIE_EFlags
        ;or   ch, 30h                  ;set iopl = 3
        and  cl, not 1h                ;set CF = 0
        mov  ss:RMStack, edx
        RRT
        mov  [esp], cx                 ;don't restore high word of eflags from CS
        push @@CS.RMIE_CS
        push @@CS.RMIE_EIP
        mov  fs,  @@CS.RMIE_FS
        mov  gs,  @@CS.RMIE_GS
        mov  es,  @@CS.RMIE_ES
        mov  edi, esi                  ;@@CS.RMIE_EDI
        mov  esi, @@CS.RMIE_ESI
        mov  ebx, @@CS.RMIE_EBX
        mov  ecx, @@CS.RMIE_ECX
        mov  edx, @@CS.RMIE_EDX
        mov  ebp, @@CS.RMIE_EBP
        lds  eax, fword ptr @@CS.RMIE_EAX
        iretd
        ENDP

DPROC PassupIntHandler
        sub  word ptr RMStack, 10      ;save on RM stack all RM segment registers
        RRT  1
IFDEF VMM
        cmp  LockedMode, 0
        $ifnot jne
          mov LockedMode, 1
          mov esi, PMStack
          RRT
          mov SavedPMStack, esi
          mov esi, PMStack[4]
          RRT
          mov SavedPMStack[4], esi
          mov esi, Data3Selector
          mov es, esi
          mov esi, OffLockedStackBottom-12
          mov [esi].IFrEIP, OffPassupIretLTrap3
        $else jmp
ENDIF
          sub  PMStack, 12
          RRT  1
          les  esi, fword ptr PMStack
          RRT
          mov  es:[esi].IFrEIP, OffPassupIretTrap3
IFDEF VMM
        $endif
ENDIF
        push es
        push esi
        pushfd
        push ds:ClientIDT[eax*8-(FirstPassupSwitchCode)*8].4
        push ds:ClientIDT[eax*8-(FirstPassupSwitchCode)*8].0
        mov  eax, [esp].8               ;load default EFlags
        mov  es:[esi].IFrCS,  Trap3Selector
        mov  ax, [ebp].RMS_Flags
        and  ah, not 40h                ;start client with NT = 0 and IOPL = 3
        or   ah, 30h
        mov  [esp].8, eax
                                       ;replace flags
        mov  ax, word ptr [ebp+size RMSStruct].4 ;with flags from RM int stack frame
        mov  es:[esi].IFrFlags, eax
        xor  eax, eax
        mov  esi, ss:[ebp].RMS_ESI
        ;mov  ds, eax
        ;mov  es, eax
        mov  fs, eax
        mov  gs, eax
        mov  eax, ss:[ebp].RMS_EAX
        mov  ebp, ss:[ebp].RMS_EBP
        iretd
        ENDP
IFDEF VMM
LLabel  PassupIretLTrapH
        push ebp
        mov  ebp, OffPMStack
        RRT
        pushfd
        cli
        push ss
        pop  ds
        push eax
        mov  eax, SavedPMStack
        mov  ss:[ebp], eax
        mov  eax, SavedPMStack[4]
        mov  LockedMode, 0
        jmp  short PassupIretTrapE
ENDIF
DPROC PassupIretTrapH
        push ebp
        mov  ebp, OffPMStack
        RRT
        pushfd
        cli
        push eax
                                   ;set client PMStack from PL1 stack bottom
        F = 3*4
        mov  eax, [esp+F].CFrESP
        mov  ss:[ebp], eax
        mov  eax, [esp+F].CFrSS
        push ss
        pop  ds
PassupIretTrapE:
        mov  ss:[ebp].4, eax
        mov  eax, RMStack
        RRT
        movzx ebp, ax
        dec  bp
        sub  ebp, size VMIStruct-12-1-10
        ;jc RMStackFault
        shr  eax, 16
        push eax
        shl  eax, 4
        add  eax, ebp
        pop  [eax].VMI_SS
        add  ebp, VMI_EAX
        mov  [eax].VMI_ESP, ebp
        movzx ebp, [eax+VMI_IP-size RMSStruct].RMS_ES
        mov  [eax].VMI_ES, ebp
        mov  bp, [eax+VMI_IP-size RMSStruct].RMS_DS
        mov  [eax].VMI_DS, ebp
        mov  bp, [eax+VMI_IP-size RMSStruct].RMS_FS
        mov  [eax].VMI_FS, ebp
        mov  bp, [eax+VMI_IP-size RMSStruct].RMS_GS
        mov  [eax].VMI_GS, ebp
        pop  [eax].VMI_EAX
        pop  ebp                  ;restore EFlags
        mov  [eax].VMI_Flags, bp
        pop  ebp
;        add  esp, 8               ;drop return address
;        pop  PMStack              ;retore client stack base
;        RRT
;        pop  PMStack[4]
;        RRT
        VCPICallTrap
        ENDP

ESeg Text
