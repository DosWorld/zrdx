;             This file is part of the ZRDX OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

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
;assume cs:Text, ss:nothing, ds:nothing, es:nothing
;switch stack and return
;default exception handler - terminate program
        DPROC DefaultExcTrapH
        cli
%ifdef NativeHost
        cmp dword [ss:OffNativeCurrent], 0
        je .legacy
        push eax
        mov ax, [esp + 4 + CFrCS]
        cmp ax, [ss:OffNativeThunkSel]
        pop eax
        je NativeFatalClient
        cmp byte [ss:OffNativeMode], 0
        je NativeDefaultWordException
.legacy:
%endif
        mov [esp + 8], ebp
        pop ebp
        add esp, 4
        movzx ebp, byte [ebp + OffFirstTrap3 + 3 - 7] ;load redirected int number
        and ebp, 0Fh
        mov [esp + 4], ebp    ;save error code on the stack
%ifdef NativeHost
        cmp dword [ss:OffNativeCurrent], 0
        jne NativeDefaultExceptionRaw
%endif
        jmp FatalExc1

;return from exception - switch to
        DPROC RetFromExcTrapH
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 0
        je NativeExceptionReturn16
%endif
ExceptionReturnFrame:
        add esp, 3 * 4    ;drop return address and errcode
        ; A client-supplied context must never select an inner privilege
        ; or request a hardware task/VM86 return from this CPL0 gate.
        or byte [esp + 4], 3
        or byte [esp + 16], 3
        and dword [esp + 8], ~24000h
        LLabel ExceptionReturnIret
        iretd


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
ExceptionWOCodeHL:
        push dword [esp]      ;push exception number again
        and dword [esp + 4], 0 ;store 0 as error code for common format
        LLabel ExceptionWithCodeH            ;the entry for exceptions without error code
ExceptionWithCodeHL:
ExceptionWithCodeH@84:
        push ebp
        cmp esp, OffKernelStack - 8 * 4
        jne ExceptionFrom0
ExceptionFromNot0:
        ClientFrameSP 12 + IFrESP, 12 + IFrSS
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 1
        ja .ordinary
        cmp dword [ss:OffNativeCurrent], 0
        je .ordinary
        test byte [esp + 12 + IFrCS], 2
        jz .ordinary
        push eax
        push edx
        mov eax, [esp + 12]
        mov dx, [ss:eax*8 + OffClientExc + 4]
        cmp dx, Trap3Selector
        jne .system_default
        lea edx, [eax*4 + OffDefaultExcTrap3]
        cmp [ss:eax*8 + OffClientExc], edx
        je .default
        jmp short .client_handler
.system_default:
        cmp dx, [ss:OffNativeThunkSel]
        jne .client_handler
        cmp dword [ss:eax*8 + OffClientExc], NativeFatalStub - NativeInt16
        jne .client_handler
.default:
        pop edx
        pop eax
        jmp NativeDefaultExceptionRaw
.client_handler:
        pop edx
        pop eax
        xor ebp, ebp
        jmp NativeLockedEvent
.ordinary:
%endif
        test byte [esp + (4 * 4)], 2  ;check rpl in return CS
        jz FatalExc
        push es
        push edi
        push esi
        push ds
        push ss
        pop ds
        mov esi, OffKernelStack - 4
        les edi, [esi - 4]  ;load client ss:esp
        sub edi, 4 + 20
        std
%rep 6
        movsd      ;move exception frame to the client stack
%endrep
        cld
        mov dword [es:edi], Trap3Selector   ;store to the client stack
        sub edi, 4                    ;return address
        mov dword [es:edi], OffRetFromExcTrap3
        pop ds
        pop esi
        push es
        push edi
        imul edi, [esp + (4 * 4) + 4], 8       ;Exception number * 4
        push dword [ss:edi + OffClientExc + 4]
        push dword [ss:edi + OffClientExc]
        les edi, [esp + (4 * 4)]    ;restore original es:edi from kernel stack
        retf                      ;switch to client excepyion handler
ExceptionFrom0: ;XXX
FatalExc:
%ifdef NativeHost
        cmp dword [ss:esp + 12], OffExceptionReturnIret
        jne .ordinary
        cmp word [ss:esp + 16], Code1Selector
        je NativeFatalClient       ;invalid user return must spare the parent
.ordinary:
%endif
        cmp byte [esp + 4], 1
        _ifnot jne
%assign F 12
          and dword [esp + F + 1 + IFrFlags], ~(1)      ;clear TF
          pop ebp
          add esp, 8
          iretd
        _endif
FatalExc1:
%assign F 16
        ;Log
        ;hlt
        push dword [esp + 4]  ;exception number
        push dword [esp + F + IFrCS]
        push dword [esp + F + 4 + IFrSS]
        push es
        push ds

        push ss
        pop ds
        push ss
        pop es

        push dword [esp + F + 16 + IFrEIP]
        pushfd
        push ebp
        mov ebp, [esp + F + 28 + IFrESP]
        push ebp
        push edi
        push esi
        push edx
        push ecx
        push ebx
        push eax

        ;Log
%ifdef Release
        mov byte [OffHasExitMessage], 1
        RRT 1
        mov edi, OffRStackStart + 4
        RRT
%else
        mov edi, 0B8000h
%endif
        push 4
        pop ecx
        lea esi, [esp + 40]
        cld
        _do
        lodsd
        and eax, 1F8h
        push dword [eax + OffLDT]
        push dword [eax + OffLDT + 4]
        push 8
        call PrintNX
        push 8
        call PrintNX
        _enddo loop
        mov ax, 0D0Ah
        stosw
        mov cl, 8               ;the preceding LOOP left ECX zero
        _do
        push 8
        call PrintNX
        _enddo loop
        mov ax, 0D0Ah
        stosw
        mov cl, 2
        _do
        push 8
        call PrintNX
        _enddo loop
        mov cl, 5
        _do
        push 4
        call PrintNX
        _enddo loop
        ;mov  cl, 8
        ;sub  esp, 12
%ifndef Release
        push dword [OffExtraDW]
        push 8
        call PrintNX
%endif
        ;IFNDEF Release
        ;hlt
        ;ENDIF
        mov al, '$'
        stosb
        ;mov al, 20h
        ;out 20h, al
        ;out 0A0h, al
L098:
%ifndef Release
        mov byte [OffPrintToMem], 0
%endif
        ;Log
        mov bx, ROffInt214C
        mov esp, ROffExitStackEnd - NExitPages * 4 - TSFrameSize
        RRT
        jmp SimpleSwitchToVM

        LLabel Exception0DHandler
        cmp esp, OffKernelStack - 6 * 4
        je near InterruptH@84
%ifdef NativeHost
        call NativeSoftwareGP
%endif
%assign F 8
        ;test [esp+F].IFrCS, 2         ;exception from kernel(CPL < 2)?
        ;jz   @@ExceptionWithCodeH
        shr byte [ss:OffException0DFlag], 1
        _ifnot jc
          mov esp, [ss:OffException0DStack]
          jmp ExceptionWithCodeH@84
        _endif
        mov [ss:OffException0DStack], esp
        push esi
        push ds
        lds esi, [esp + F + 8 + IFrEIP]         ;cs:eip
        push eax
        ; Do not let the decoder's own memory access fault on a valid
        ; client #GP at the end of its code segment.
        lsl eax, word [esp + 24]
        jnz ToCommonTrap@84
        sub eax, 2
        jc ToCommonTrap@84
        cmp esi, eax
        ja ToCommonTrap@84
        ; Preserve the old EAX-only FPU/CR2 compatibility without
        ; executing arbitrary client CR/DR instructions on the host stack.
        mov ax, [esi]
        cmp ax, 200Fh
        je ReadClientCR@84
        cmp ax, 220Fh
        jne ToCommonTrap@84
        cmp byte [esi + 2], 0C0h  ;only MOV CR0,EAX
        jne ToCommonTrap@84
        mov eax, cr0
        xor eax, [esp]
        test eax, ~0Eh           ;only MP/EM/TS may change
        jnz ToCommonTrap@84
        mov eax, [esp]
        mov cr0, eax
        jmp short ClientCRDone@84
ReadClientCR@84:
        cmp byte [esi + 2], 0D0h  ;MOV EAX,CR2
        jne ReadClientCR0@84
        mov eax, cr2
        jmp short ClientCRDone@84
ReadClientCR0@84:
        cmp byte [esi + 2], 0C0h
        jne ToCommonTrap@84
        mov eax, cr0
ClientCRDone@84:
        add esp, 4
        mov esi, cr0
        mov [ss:OffPMCR0], esi
        RRT
        pop ds
        pop esi
        add dword [esp + F + IFrEIP], 3
        add esp, 8                  ;drop exception number and error code
        mov byte [ss:OffException0DFlag], 1
        iretd
ToCommonTrap@84:
        pop eax
        pop ds
        pop esi
        mov byte [ss:OffException0DFlag], 1
        jmp ExceptionWithCodeH@84

        LLabel ExceptionOrInterruptH     ;the entry for exceptions/interrupts in range 8-F
        cmp esp, OffKernelStack - 6 * 4
        jne ExceptionWithCodeH@84
        LLabel InterruptH                ;the entry for interrupts
InterruptHL:
InterruptH@84:
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 1
        ja .ordinary
        cmp dword [ss:OffNativeCurrent], 0
        je .ordinary
        test byte [esp + 4 + IFrCS], 2
        jz .ordinary
        push dword [esp]           ;vector, then synthetic error code
        mov dword [esp + 4], 0
        push ebp
        mov ebp, 1
        ClientFrameSP 12 + IFrESP, 12 + IFrSS
        jmp NativeLockedEvent
.ordinary:
%endif
        push ebp
;hardware interrupts from PL0 not allowed
InterruptFromNot0:
        push eax
%assign F 12 ;eax, ebp, int number
        ClientFrameSP F + IFrESP, F + IFrSS
        mov eax, [esp + F + IFrCS]    ;
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
        jz HIntFrom01@84          ;Interrupt from PL1, not from 2 or 3
        push ds
%assign F F + 4 ;shift of interrupt frame
%ifdef VMM
        cmp byte [ss:OffLockedMode], 0
        _ifnot jne
          mov ebp, Data3Selector   ;selector of locked stack
          mov ds, ebp
          sub dword [OffTSS + TSS_ESP1], 20     ;reserve space for current iret frame
          mov byte [OffLockedMode], 1
          mov ebp, [OffTSS + TSS_ESP1]
          mov [ebp + IFrCS], eax
          mov eax, [esp + F + IFrEIP]
          mov [ebp + IFrEIP], eax
          mov eax, [esp + F + IFrSS]
          mov [ebp + IFrSS], eax
          mov eax, [esp + F + IFrESP]
          mov [ebp + IFrESP], eax
          ;we don't need to transfer eflags, because it will be transferred
          ;during IretTrapH
          mov eax, OffLockedStackBottom - 12 ;
          mov dword [eax + IFrCS], Trap3Selector
          mov ebp, OffIretPLTrap3
        _else jmp
%endif
          sub dword [esp + F + IFrESP], 12
          mov ebp, eax
          lds eax, [esp + F + IFrESP]  ;client ss
          mov [eax + IFrCS], ebp            ;client cs
          mov ebp, [esp + F + IFrEIP]            ;
%ifdef VMM
        _endif
%endif
        mov [eax + IFrEIP], ebp         ;
        mov ebp, [esp + F + IFrFlags] ;
        mov [eax + 8], ebp       ;
        and ebp, ~(4300h)        ;clear  IF and TF and NT
        push ds  ;[esp+F].IFrSS    ;
        push eax                   ;push .IFrESP,
        push ebp                          ;eflags
        movzx ebp, byte [esp + 24]      ;load interrup number
        shl ebp, 3
        push dword [ebp + OffClientIDT + 4]
        push dword [ebp + OffClientIDT]
        mov ebp, [esp + 28]
        mov eax, [esp + 24]
        mov ds, [esp + 20]
        iretd

;interrupt from level 1
HIntFrom01@84:
;----------------- save all registers in kernel stack ---------------------
        ;Log
        mov ebp, OffKernelStack
        mov esp, [ebp - 8]         ;get esp for level 1
                      ;exceptions & interrupts not allowed there !
        mov eax, [ebp - 12]      ;eflags for interrupted server code
        push eax
        push dword [ebp - 20]  ;eip,
        push edi
        push esi
        push dword [ebp - 28] ;ebp from stack
        push ebx
        push edx
        push ecx
        push dword [ebp - 32] ;eax from stack

        push ds        ;because stack0 is dropped
        push es        ;because stack0 is dropped
        push fs        ;because stack0 is dropped
        push gs        ;because stack0 is dropped
                                ;push cs not needed, cs == Code1Selector
        movzx ecx, byte [ebp - 24] ;get interrupt number
        mov ebp, [ss:OffTSS + TSS_ESP1]
        push ebp           ;save stack1 frame
        ;cmp  esp, XXX     ;test for stack overflow
        ;jb   KernelStack1Overflow
;--------------------------- jmp to client vector --------------------------
        and ah, ~(43h)  ;IFBitMask;return to client with server flags and IF=0
        mov [ss:OffTSS + TSS_ESP1], esp        ;new frame started from there
%ifdef VMM
        cmp byte [ss:OffLockedMode], 0
        _ifnot jne
          mov ebx, Data3Selector
          mov ds, ebx
          mov byte [ss:OffLockedMode], 1
          mov ebx, OffLockedStackBottom - 12
          mov dword [ebx], OffIret2KernelAsincLTrap3 ;address of trap
        _else jmp
%endif
%ifdef NativeHost
          cmp byte [ss:OffNativeMode], 1
          ja .ordinary_stack
          cmp dword [ss:OffNativeCurrent], 0
          je .ordinary_stack
          push edx
          mov ebx, [ebp - 8]
          mov edx, [ebp - 4]
          call NativePickLockedStack
          pop edx
          jmp short .stack_ready
.ordinary_stack:
%endif
          lds ebx, [ebp - 8]          ;client ss:esp
%ifdef NativeHost
.stack_ready:
%endif
%ifdef NativeHost
          cmp byte [ss:OffNativeMode], 0
          jne .wide_trampoline
          cmp dword [ss:OffNativeCurrent], 0
          je .wide_trampoline
          sub ebx, 6
          mov word [ebx], OffIret2KernelAsincTrap3
          mov word [ebx + 2], Trap3Selector
          mov [ebx + 4], ax
          jmp short .trampoline_done
.wide_trampoline:
%endif
          sub ebx, 12
          mov dword [ebx], OffIret2KernelAsincTrap3
%ifdef NativeHost
.trampoline_done:
%endif ;address of trap
%ifdef VMM
        _endif
%endif
        ;Log
        push ds          ;new client ss
        push ebx         ;new client esp
        push eax         ;new client eflags
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 0
        jne .wide_frame
        cmp dword [ss:OffNativeCurrent], 0
        jne .frame_ready
.wide_frame:
%endif
        mov [ebx + 8], eax
        mov dword [ebx + 4], Trap3Selector
%ifdef NativeHost
.frame_ready:
%endif
%ifdef NativeHost
        cmp dword [ss:OffNativeCurrent], 0
        jne .active_vector
        push Trap3Selector
        lea ebp, [ecx*4 + OffDefIntTrap3]
        push ebp
        iretd
.active_vector:
%endif
        push dword [ss:ecx*8 + OffClientIDT + 4]
        push dword [ss:ecx*8 + OffClientIDT]
        iretd

%ifdef VMM
        LLabel Iret2KernelAsincLTrapH
        mov byte [ss:OffLockedMode], 0 ;disable locked mode
%endif
;PL1 handler
;return to interrupted by hardware int place of server PL1 code
        DPROC Iret2KernelAsincTrapH
        cli
        ;Log
        add esp, 16         ;drop call gate stack frame
        pop dword [ss:OffTSS + TSS_ESP1] ;restore level 1 stack pointer in the TSS
        pop gs
        pop fs
        pop es
        pop ds
        pop eax
        pop ecx
        pop edx
        pop ebx
        pop ebp
        pop esi
        pop edi
        push dword [esp]         ;push eip again
        mov dword [esp + 4], Code1Selector ;replace it with CS to complete simple iret frame
        ;Log
        iretd                   ;from PL1 to PL1


%ifdef VMM
        DPROC IretPLTrapH
        pushfd                       ;transfer current eflags to old frame
        cli
        pop dword [esp + 16 + IFrFlags]
        add esp, 16                 ;drop current return frame and switch to prev frame
        mov byte [ss:OffLockedMode], 0        ;disable locked mode
        add dword [ss:OffTSS + TSS_ESP1], 20     ;free frame in kernel stack1
        iretd

%endif

;prepare RM stack and switch to
;rewrite return address to client stack
;save client ss:esp
NormalizeClientStack:
        pushfd
        push edx
        mov edx, [ss:eax + 4]
        lar edx, edx
        test edx, 400000h
        jnz .wide
        mov word [ss:eax + 2], 0
.wide:  pop edx
        popfd
        ret

        DPROC DefIntTrapH
        ClientFrameSP CFrESP, CFrSS
%ifdef NativeHost
        call NativeDefaultFrame
        jc NativeDefaultReturn
        cmp ah, 4Ch
        jne .reflect
        push ebp
        mov ebp, [esp + 4 + CFrEIP]
        cmp byte [ebp + OffFirstTrap3 + 3 - 7], 21h
        pop ebp
        jne .reflect
        jmp NativeClientExit
.reflect:
%endif
DefaultReflect@84:
        push ebx
        push ebp
        pushfd
        cli
        mov ebp, OffPMStack - 4
        RRT
        push eax
%assign F 4 * 4 ;ebx, eax
        ;sub  dword ptr [esp+F].CFrESP, 8
        mov eax, ds
        mov ebx, [esp + F + CFrESP]
        mov ds, [esp + F + CFrSS]
        sub ebx, 8
        mov [ebp + 4 + 4], ds
        push ss
        mov [ebp + 4 + 0], ebx
        mov [ebx + PMI_DS], ax
        mov [ebx + PMI_ES], es
        mov eax, Data1Selector
        mov ebp, [ebx + PMI_EFlags]
        mov [ebx + PMI_FS], fs
        mov [ebx + PMI_GS], gs
        pop ds

%define _rseg eax
%define _rsegw ax
%define _rdisp ebx
%define _rdispw bx
        mov _rseg, dword [OffRMStack + 0]
        RRT
        movzx _rdisp, _rsegw
        shr _rseg, 16
        dec _rdispw
        push _rseg
        sub _rdisp, VMIStruct_size - 1
        shl _rseg, 4
        add _rseg, _rdisp
        add _rdisp, VMI_EAX
%define _cs eax
        mov dword [eax + VMI_ESP], _rdisp
        ;mov  ebx, 0F000h
        xor ebx, ebx
        pop dword [eax + VMI_SS]
        mov [eax + VMI_EndFlags], bp  ;flags to real iret frame
        mov [eax + VMI_DS], ebx
        mov [eax + VMI_ES], ebx
        mov ebp, [esp + F + CFrEIP]   ;assume call from Trap3 segment
        mov [eax + VMI_FS], ebx
        mov [eax + VMI_GS], ebx
        db 0C7h, 40h, VMI_EndIP
        dw RMIretSwitchCode, DGROUP16

        movzx ebx, byte [ebp + OffFirstTrap3 + OffDefIntTrap3 + 3 - 7] ;load redirected int number
        bt dword [OffRIntFlags], ebx
        _ifnot jc
        mov ebx, [ebx*4]       ;CS:IP from current real mode vector
        _else jmp
        mov ebx, [ebx*4 + OffSavedRealVectors]  ;CS:IP from saved real vector
        _endif
        mov [eax + VMI_IP], ebx
        pop dword [eax + VMI_EAX] ;write eax for restoring after mode switch
        pop ebx
        and bh, ~(43h)   ;clear IF and TF and NT for initial flags
        pop ebp
        mov [eax + VMI_Flags], bx ;write flags for restoring
        pop ebx
SwitcherToVM:
        VCPICallTrap

        DPROC PMSaveStateTrapH
        push esi
        push edi
        push ds
        push es
        pushfd
        cli
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 0
        jne .wide_pointer
        movzx edi, di
.wide_pointer:
%endif
        mov esi, OffRMStack
        RRT
        cmp al, 0
        cld
        push ss
        _ifnot jne
          pop ds
        _else jmp
          xchg esi, edi
          push es
          pop ds
          pop es
        _endif
        movsd
        movsd
        movsd
%ifdef NativeHost
        movsd
        call NativeRestoreAlias
%endif
        popfd
        pop es
        pop ds
        pop edi
        pop esi
        retf


        DPROC PMRawSwitchTrapH
        ClientFrameSP CFrESP, CFrSS
        movzx eax, ax
        push ebp
        pushfd
        cli
        mov ebp, OffPMStack
        RRT
        push eax
%assign F 12
        mov eax, [esp + F + CFrESP]
        mov [ebp], eax
        mov eax, [esp + F + CFrSS]
        mov [ebp + 4], eax
        movzx ecx, cx
        movzx edx, dx
        push edx           ;RM ss
        shl edx, 4
        movzx eax, bx      ;esp
        dec ax
        sub bx, 10
        sub ax, VMIShortStruct_size - 1
        ;jc RStackOverflow
        add eax, edx
        mov ebp, eax
        pop dword [ebp + VMI_SS]
        pop dword [ebp + VMI_DS]
        pop edx                 ;init flags
        mov [ebp + VMI_Flags], dx
        mov [ebp + VMI_CS], si
        mov [ebp + VMI_IP], di
        mov [ebp + VMI_ESP], ebx
        mov [ebp + VMI_ES], ecx
        xor edx, edx             ;EAX must retain the switch-frame address
        mov [ebp + VMI_FS], edx
        mov [ebp + VMI_GS], edx
        pop ebp
        VCPICallTrap

        DPROC VCPICallHandler
        pushfd
        and byte [esp + 1], ~(40h)    ;clear NT in current eflags
        popfd
        mov esp, eax
        mov eax, Data0Selector
        mov ds, eax
        db PushWCode
        dd DGROUP16
        mov eax, [OffRMCR0]
        RRT
        mov cr0, eax
        mov eax, 0DE0Ch
        db PushWCode
        dd pop_eax_iret
        mov byte [OffGDT + TSSSelector + 5], 80h + SS_FREE_TSS3 ;mark current TSS as FREE
        call far [OffVCPICall]

        DPROC VCPITrapHandler
        cmp ah, 0Bh
        je near DebugService
%ifndef Release
        ;cmp  al, 5
        ;$ifnot jne
        ;  push edx
        ;  shr  edx, 12
        ;  btr  APages, edx
        ;  pop  edx
        ;  jnc  @@Err
        ;$endif
%endif
        ;cmp  al, 4
        ;pushfd
        call far [OffVCPICall]
        ;popfd
%ifndef Release
        ;$ifnot jne
        ;  push edx
        ;  shr  edx, 12
        ;  bts  APages, edx
        ;  pop  edx
        ;  jc  @@Err
        ;$endif
%endif
        retf

        ;Interrupts are disabled on entry. MOV DR uses a stack-local
        ;instruction, following the existing MOV CR emulation convention.
DebugService:
        pushad
        mov edi, [OffDebugUsed]
        test al, al
        jnz DebugHandle
        mov ebp, ebx
        shl ebp, 16
        mov bp, cx
        movzx eax, dl
        dec eax
        cmp al, 3
        ja DebugValue
        cmp al, 2
        je DebugValue
        test ebp, eax
        jnz DebugValue
        cmp dh, 2
        ja DebugValue
        test dh, dh
        jnz .type
        test eax, eax
        jnz DebugValue
.type:  mov esi, dr7
        xor ecx, ecx
.slot:  bt edi, ecx
        jc .next
        mov eax, esi
        push ecx
        shl ecx, 1
        shr eax, cl
        pop ecx
        test al, 3
        jz .found
.next:  inc ecx
        cmp ecx, 4
        jb .slot
        mov al, 16h
        jmp DebugError
.found: cmp byte [OffDebugSaved], 0
        jne .saved
        mov eax, dr0
        mov [OffSavedDebug], eax
        mov eax, dr1
        mov [OffSavedDebug + 4], eax
        mov eax, dr2
        mov [OffSavedDebug + 8], eax
        mov eax, dr3
        mov [OffSavedDebug + 12], eax
        mov eax, dr6
        mov [OffSavedDebug + 16], eax
        mov [OffSavedDebug + 20], esi
        mov byte [OffDebugSaved], 1
.saved: bts [OffDebugUsed], ecx
        mov [esp + PA_EBX], cx
        mov eax, dr6
        btr eax, ecx
        mov dr6, eax
        mov ebx, ecx
        shl ebx, 3
        push 0C3C0230Fh       ;MOV DRn,EAX; RET, confined to this stack
        or [esp + 2], bl
        mov eax, ebp
        call esp
        add esp, 4
        movzx eax, dl
        dec eax
        shl eax, 2
        movzx ebx, dh
        cmp bl, 2
        jne .encoded
        inc ebx               ;hardware RW=3 means read/write
.encoded:
        or eax, ebx
        mov ebx, 0fh
        shl ecx, 2
        add ecx, 16
        shl ebx, cl
        not ebx
        and esi, ebx
        shl eax, cl
        or esi, eax
        sub ecx, 16
        shr ecx, 1
        mov eax, 2           ;global enable survives VCPI task switches
        shl eax, cl
        or esi, eax
        or esi, 700h          ;reserved one + exact data traps on 386 (LE/GE)
        mov dr7, esi
        jmp short DebugOK
DebugHandle:
        movzx ecx, bx
        cmp ecx, 4
        jae DebugInvalid
        bt edi, ecx
        jnc DebugInvalid
        cmp al, 2
        jne .change
        mov eax, dr6
        bt eax, ecx
        setc al
        movzx eax, al
        mov [esp + PA_EAX], ax
        jmp short DebugOK
.change:
        cmp al, 1
        jne .reset
        btr [OffDebugUsed], ecx
        mov eax, 3
        shl ecx, 1
        shl eax, cl
        not eax
        mov ebx, dr7
        and ebx, eax
        mov dr7, ebx
        shr ecx, 1
.reset: mov eax, dr6
        btr eax, ecx
        mov dr6, eax
DebugOK:
        popad
        clc
        retf
DebugInvalid:
        mov al, 23h
        jmp short DebugError
DebugValue:
        mov al, 21h
DebugError:
        mov ah, 80h
        mov [esp + PA_EAX], ax
        popad
        stc
        retf

RestoreDebug:
        cmp byte [OffDebugSaved], 0
        je .done
        xor eax, eax
        mov dr7, eax
        mov eax, [OffSavedDebug]
        mov dr0, eax
        mov eax, [OffSavedDebug + 4]
        mov dr1, eax
        mov eax, [OffSavedDebug + 8]
        mov dr2, eax
        mov eax, [OffSavedDebug + 12]
        mov dr3, eax
        mov eax, [OffSavedDebug + 16]
        mov dr6, eax
        mov eax, [OffSavedDebug + 20]
        mov dr7, eax
.done:  ret

        DPROC LoadLDTHandler
        lldt bp
        retf

        DPROC InvalidateTLBHandler
        push eax
        mov eax, [OffSwitchTableCR3]
        RRT
        mov cr3, eax
        pop eax
        retf


        DPROC ExitDPMIHost
;Entry for PL0, switches to PL1
        LLabel TerminateHandler
        call RestoreDebug
        push Data1Selector
        pop ds
        push ds
        pop es
        push ds
        push dword [OffTSS + TSS_ESP1]
        push Code1Selector
        push OffTerminateHandler1
        retf
        LLabel TerminateHandler1
%ifdef NativeHost
        ; No client remains. Reuse the kernel dummy only during final
        ; cleanup, after its separately allocated root anchor can be freed.
        or byte [OffPage2 + (((OffKernelStack - 1) & ~0FFFh) - KernelBase) / 1024], 4
        InvalidateTLB
%endif
        push Data3Selector     ;save sp for interrup handler
        push OffUserStackEnd
        db 0C7h, 05h
        dd OffRMStack
        RRT
        dw ROffExitStackEnd - TSFrameSize ;mov RealStack, dgroup16<<16+ROffExitStackEnd
        dw DGROUP16
;-------------------restore all RM vectors hooked by server --------------------
; and RM vectors hooked by client
        xor ecx, ecx
        mov edi, ecx
        dec cl        ;mov ecx, 0FFh
        mov ebx, OffDefIntTrap3 + 4 * 255
        mov esi, OffClientIDT
        _do
        mov dword [esi + ecx*8 + 4], Trap3Selector
        mov [esi + ecx*8], ebx
        btr dword [OffRIntFlags], ecx
        cmp ecx, 2Fh
        jbe AbsSet@105
        bt dword [OffPassupIntMap], ecx
        _ifnot jnc
AbsSet@105:
          mov eax, [ecx*4 + OffSavedRealVectors]
          mov [edi + ecx*4], eax
        _endif
        sub ebx, 4
        dec ecx
        _enddo jns

;free all pages, allocated by DPMI 800h
        mov ebx, [OffRootMCB + MCB_StartOffset]
        mov edx, 3FFh
%ifndef VMM
        call FreeHMPages
        call CleanMemVector
        movzx eax, word [OffLDTLimit]
        add eax, OffLDT + 1
        sub eax, (OffLastInit + 0FFFh) & ~0FFFh
        _ifnot jbe
          push eax
          push (OffLastInit + 0FFFh) & ~0FFFh
          push 0
          call FreePages
        _endif
        call ReturnFreePool
%endif
;only for PL0
        mov esp, ROffExitStackEnd - NExitPages * 4 - TSFrameSize
        RRT
        cld
        mov esi, OffPage2
        mov edi, esp
        xor ecx, ecx
        mov cl, NExitPages
        mov ebp, esp
        rep movsd            ;move
        mov cl, NExitPages
        mov bx, TerminateRHandler2

;entry: esp - VM stack, ebx - VM start ip
SimpleSwitchToVM:
        mov eax, DGROUP16
        pushfw
        push ax
        push bx
        sub esp, 6
        push eax  ;fs gs es ds ss
        push eax  ;fs gs es ds ss
        push eax  ;fs gs es ds ss
        push eax  ;fs gs es ds ss
        push eax  ;fs gs es ds ss
        push (ROffExitStackEnd - NExitPages * 4 - TSFrameSize - 10) ;esp
        push eax                   ;skip eflags
        mov eax, esp
        jmp SwitcherToVM


%ifndef Release
        DPROC PrintByteP
        cmp byte [cs:OffPrintToMem], 0
        _ifnot je
          stosb
        _else jmp
          mov ah, 2Fh
          stosw
        _endif
        ret
%endif

%macro PrintByte 0
%ifdef Release
        stosb                    ;release diagnostics always use the RM buffer
%else
        call PrintByteP
%endif
%endmacro
%macro CloseSeg 1
        segment %1
        %assign _csz zcat2(SegSize,%1)
%if _csz - ($ - $$) >= 0
        times _csz - ($ - $$) db 0
%else
%assign _used ($ - $$)
%error segment %1 uses _used bytes; budget _csz
%endif
%endmacro

;Digit, n
        DPROC PrintNX
        push eax
        push ecx
        pushfd
        cld
;@@Digit EQU DWORD PTR ss:[esp+12].8
;@@N     EQU DWORD PTR ss:[esp+12].4
        ;mov ax, 2F00h+' '
        mov al, ' '
        PrintByte
        ;stosw
        mov eax, [esp + 12 + 8]
        mov cl, 8
        sub cl, [esp + 12 + 4]
        shl cl, 2
        rol eax, cl
        mov ecx, [esp + 12 + 4]
        _do
        rol eax, 4
        push eax
        and al, 1111b
        add al, '0'
        cmp al, '9'
        _ifnot jbe
        add al, 'A' - '9' - 1
        _endif
        ;mov ah, 2Fh
        PrintByte
        ;stosw
        pop eax
        _enddo loop
        popfd
        pop ecx
        pop eax
        ret 8


;PM handler for VM->PM   VCPI switch
        DPROC RMS_PMHandler
        mov eax, Data0Selector
        movzx esi, bp
        mov ds, eax
        mov es, eax
        mov ss, eax
%ifndef VMM
          mov esp, OffKernelStack
%else
          mov esp, OffKernelStack - 4
          push eax                   ;push Data0Selector
%endif
        mov eax, [OffPMCR0]
        RRT
        mov cr0, eax
        ;Real DOS may clear IOPL. Ring-1 cleanup uses CLI/STI as well.
        pushfd
        or byte [esp + 1], 30h
        popfd
        add bp, RMSStruct_size    ;ajust RM sp to top of normal RM stack
        mov [OffRMStack], ebp
        RRT
        shr ebp, 12
        and ebp, ~(1111b)
        add ebp, esi              ;ebp->LA of RMS
        movzx eax, word [ebp + RMS_SwitchCode]
        sub eax, OffFirstSwitchCode + 3
        cmp eax, FirstPassupSwitchCode
        jae near PassupIntHandler
        cmp eax, MaxSystemSwitchCode
        jae RMCallbackHandler
        jmp [eax + OffRMS_JmpTable]

        align 4
        LDWord RMS_JmpTable
        dd OffRMIretHandler
        dd OffDos1CallRet
        dd OffRMIERetHandler
        dd OffTerminateHandler
        dd OffRMRawSwitchHandler

        DPROC RMIretHandler
        lds esi, [OffPMStack]
        RRT
        lea eax, [esi + PMIStruct_size]
        push ds
        push eax
        mov eax, [esi + PMI_EFlags]
        mov ax, [ebp + RMS_Flags]     ;load transferred flags
        or ah, 30h                 ;force IOPL=3
        and ah, ~(40h)             ;and NT = 0
        push eax
        push dword [esi + PMI_CS]
        push dword [esi + PMI_EIP]
        mov fs, [esi + PMI_FS]      ;restore segment registers from client stack
        mov gs, [esi + PMI_GS]
        mov es, [esi + PMI_ES]
        mov ds, [esi + PMI_DS]
        mov eax, [ebp + RMS_EAX]    ;and transfer RON from RM stack
        mov esi, [ebp + RMS_ESI]
        mov ebp, [ebp + RMS_EBP]
%ifdef NativeHost
        call NativeFinishIret
%endif
        iretd


        DPROC RMRawSwitchHandler
%ifdef NativeHost
        cmp byte [OffNativePending], 0
        RRT 1
        je .client
        mov al, 1
        call NativeKernelStack
        push Data1Selector
        push OffKernelStack1End
        push Code1Selector
        push OffNativeResume
        retf
.client:
        call NativeRestoreAlias
        cmp byte [ss:OffNativeMode], 0
        jne .wide_target
        movzx ebx, bx
        movzx edi, di
.wide_target:
%endif
        movzx edx, dx
        push edx
        push ebx
        pushfd
        movzx eax, word [ebp + RMS_ESI]
        or byte [esp + 1], 30h     ;set iopl==3
        push eax
        push edi
        xor eax, eax
        mov fs, eax
        mov gs, eax
        mov es, ecx
        mov ds, [ebp + RMS_EAX]
        mov ebp, [ebp + RMS_EBP]
        iretd


        DPROC RMCallbackHandler
%ifdef VMM
        cmp byte [OffLockedMode], 0
        _ifnot jne
          mov byte [OffLockedMode], 1
          mov esi, [OffPMStack]
          RRT
          mov [OffSavedPMStack], esi
          mov esi, [OffPMStack + 4]
          RRT
          mov [OffSavedPMStack + 4], esi
          mov esi, Data3Selector
          mov es, esi
          mov esi, OffLockedStackBottom - 12
          mov dword [esi + IFrEIP], OffPMCallbackIretLTrap3
        _else jmp
%endif
%ifdef NativeHost
          cmp byte [OffNativeMode], 1
          ja .ordinary_stack
          cmp dword [OffNativeCurrent], 0
          je .ordinary_stack
          call NativeLockedBridge
          jmp short .stack_ready
.ordinary_stack:
%endif
          sub dword [OffPMStack], 12
          RRT 1
          les esi, [OffPMStack]
          RRT
%ifdef NativeHost
.stack_ready:
%endif
          mov dword [es:esi + IFrEIP], OffPMCallbackIretTrap3
%ifdef VMM
        _endif
%endif
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 0
        jne .wide_trampoline
        push eax
        pushfd
        pop eax
        mov word [es:esi + 2], Trap3Selector
        mov [es:esi + 4], ax
        pop eax
        jmp short .trampoline_ready
.wide_trampoline:
%endif
        mov dword [es:esi + IFrCS], Trap3Selector
        pushfd
        pop dword [es:esi + IFrFlags] ;, eax
%ifdef NativeHost
.trampoline_ready:
%endif
        push es
        push esi
        pushfd
        or byte [esp + 1], 30h
S equ MaxSystemSwitchCode * 3
        lea eax, [eax + eax*2 + OffCallbacksTable - S]
        movzx esi, word [eax + CBT_CS]
        push esi
        push dword [eax + CBT_EIP]
        lds esi, [eax + CBT_SPtrOff]
;        @@DC equ ds:[esi]
        mov [esi + DC_EBX], ebx
        mov [esi + DC_ECX], ecx
        mov [esi + DC_EDX], edx
        mov [esi + DC_EDI], edi
        mov ecx, [ebp + RMS_EAX]
        mov [esi + DC_EAX], ecx
        mov ecx, [ebp + RMS_ESI]
        mov [esi + DC_ESI], ecx
        mov ecx, [ebp + RMS_EBP]
        mov [esi + DC_EBP], ecx
        mov ecx, [ebp + RMS_ES]
        mov [esi + DC_ES], ecx
        mov ecx, [ebp + RMS_FS]
        mov [esi + DC_FS], ecx
        mov cx, [ebp + RMS_DS]
        mov [esi + DC_DS], cx
        mov cx, [ebp + RMS_GS]
        mov [esi + DC_GS], cx
        mov cx, [ebp + RMS_Flags]
        mov [esi + DC_Flags], cx
        mov ecx, [ss:OffRMStack]
        RRT
        mov [esi + DC_SP], ecx
        mov eax, Data3Selector
        push ds
        mov ds, eax
        mov edi, esi
        mov al, 0 ; Data3selector
        pop es
        mov fs, eax
        mov gs, eax
        lea esi, [ebp + RMSStruct_size]
%ifdef NativeHost
        call NativeCallbackSegments16
%endif
        iretd


%ifdef VMM
        LLabel PMCallbackIretLTrapH
        mov ebp, OffSavedPMStack
        mov eax, [ebp + 0]
        mov edx, [ebp + 4]
        ;mov ss:LockedMode, 0
        mov byte [ebp + OffLockedMode - OffSavedPMStack], 0
        jmp PMCallbackIretTrapE
%endif

        DPROC PMCallbackIretTrapH
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 1
        ja .ordinary_stack
        cmp dword [ss:OffNativeCurrent], 0
        je .ordinary_stack
        call NativeLockedBridgeLeave
        cmp byte [ss:OffNativeMode], 0
        jne PMCallbackIretTrapE
        movzx edi, di
        jmp short PMCallbackIretTrapE
.ordinary_stack:
%endif
        mov eax, [esp + CFrESP]
        mov edx, [esp + CFrSS]
PMCallbackIretTrapE:
        cli
        push es
        pop ds
        mov ebp, OffPMStack
        RRT
        mov [ebp + 0], eax
        mov [ebp + 4], edx
        movzx eax, word [edi + DC_SP]
        movzx ebp, word [edi + DC_SS]
        push ebp
        shl ebp, 4
        dec ax
        sub eax, VMIStruct_size - 6 - 1
        ;jb @@RStackOverflow
        add ebp, eax
        pop dword [ebp + VMI_SS]
        add eax, VMI_EAX
        mov [ebp + VMI_ESP], eax
        mov ax, [edi + DC_Flags]
        mov [ebp + VMI_Flags], ax
        mov edx, [edi + DC_IP]
        jmp SwitchToVMWithTransfer



;VMS  - struct for switch from VM to PM
;PMI  - client stack frame for interrupts, executed in PM
;RMIE - client stack frame for real mode interrupt emulation (DPMI service 30X)

        DPROC RMIERetHandler
        les eax, [OffPMStack]
        RRT
        lds esi, [es:eax + RMIE_EDI]
;        @@DC equ ds:[esi]
        mov [esi + DC_EBX], ebx
        mov [esi + DC_ECX], ecx
        mov [esi + DC_EDX], edx
        mov ecx, [ebp + RMS_EAX]
        mov [esi + DC_EDI], edi
        mov edx, [ebp + RMS_ESI]
        mov [esi + DC_EAX], ecx
        mov ebx, [ebp + RMS_EBP]
        mov [esi + DC_ESI], edx
        mov ecx, [ebp + RMS_ES]
        mov [esi + DC_EBP], ebx
        mov edx, [ebp + RMS_FS]
        mov [esi + DC_ES], ecx
        mov bx, [ebp + RMS_Flags]
        mov [esi + DC_FS], edx
        mov [esi + DC_Flags], bx


        push es                        ;client SS
        push es
        add eax, RMIEStruct_size
        pop ds
        push eax                       ;new esp
        pushfd
;        @@CS equ ds:[eax-size RMIEStruct]
        mov dl, [eax - RMIEStruct_size + RMIE_CS + 2]
        mov [ss:OffA20Reset], dl
        RRT
        mov edx, [eax - RMIEStruct_size + RMIE_RealStack]
        mov ecx, [eax - RMIEStruct_size + RMIE_EFlags]
        ;or   ch, 30h                  ;set iopl = 3
        and cl, ~(1h)                ;set CF = 0
        mov [ss:OffRMStack], edx
        RRT
        mov [esp], ecx                ;restore the protected caller's full flags
        push dword [eax - RMIEStruct_size + RMIE_CS]
        push dword [eax - RMIEStruct_size + RMIE_EIP]
        mov fs, [eax - RMIEStruct_size + RMIE_FS]
        mov gs, [eax - RMIEStruct_size + RMIE_GS]
        mov es, [eax - RMIEStruct_size + RMIE_ES]
%ifdef NativeHost
        mov edi, [eax - RMIEStruct_size + RMIE_OriginalDI]
%else
        mov edi, esi                  ;@@CS.RMIE_EDI
%endif
        mov esi, [eax - RMIEStruct_size + RMIE_ESI]
        mov ebx, [eax - RMIEStruct_size + RMIE_EBX]
        mov ecx, [eax - RMIEStruct_size + RMIE_ECX]
        mov edx, [eax - RMIEStruct_size + RMIE_EDX]
        mov ebp, [eax - RMIEStruct_size + RMIE_EBP]
        lds eax, [eax - RMIEStruct_size + RMIE_EAX]
%ifdef NativeHost
        call NativeFinishIret
%endif
        iretd


        DPROC PassupIntHandler
        sub word [OffRMStack], 10      ;save on RM stack all RM segment registers
        RRT 1
%ifdef VMM
        cmp byte [OffLockedMode], 0
        _ifnot jne
          mov byte [OffLockedMode], 1
          mov esi, [OffPMStack]
          RRT
          mov [OffSavedPMStack], esi
          mov esi, [OffPMStack + 4]
          RRT
          mov [OffSavedPMStack + 4], esi
          mov esi, Data3Selector
          mov es, esi
          mov esi, OffLockedStackBottom - 12
          mov dword [esi + IFrEIP], OffPassupIretLTrap3
        _else jmp
%endif
%ifdef NativeHost
          cmp byte [OffNativeMode], 1
          ja .ordinary_stack
          cmp dword [OffNativeCurrent], 0
          je .ordinary_stack
          call NativeLockedBridge
          jmp short .stack_ready
.ordinary_stack:
%endif
          sub dword [OffPMStack], 12
          RRT 1
          les esi, [OffPMStack]
          RRT
%ifdef NativeHost
.stack_ready:
%endif
          mov dword [es:esi + IFrEIP], OffPassupIretTrap3
%ifdef VMM
        _endif
%endif
        push es
        push esi
        pushfd
%ifdef NativeHost
        cmp dword [OffNativeCurrent], 0
        jne .active_vector
        push Trap3Selector
        lea eax, [eax*4 + OffDefIntTrap3 - FirstPassupSwitchCode*4]
        push eax
        jmp short .vector_ready
.active_vector:
%endif
        push dword [eax*8 + OffClientIDT - ((FirstPassupSwitchCode) * 8) + 4]
        push dword [eax*8 + OffClientIDT - ((FirstPassupSwitchCode) * 8) + 0]
%ifdef NativeHost
.vector_ready:
%endif
        mov eax, [esp + 8]               ;load default EFlags
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 0
        jne .wide_cs
        mov word [es:esi + 2], Trap3Selector
        jmp short .cs_ready
.wide_cs:
%endif
        mov dword [es:esi + IFrCS], Trap3Selector
%ifdef NativeHost
.cs_ready:
%endif
        mov ax, [ebp + RMS_Flags]
        and ah, ~(40h)                ;start client with NT = 0 and IOPL = 3
        or ah, 30h
        mov [esp + 8], eax
                                       ;replace flags
        mov ax, [ebp + RMSStruct_size + 4] ;with flags from RM int stack frame
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 0
        jne .wide_flags
        mov [es:esi + 4], ax
        jmp short .flags_ready
.wide_flags:
%endif
        mov [es:esi + IFrFlags], eax
%ifdef NativeHost
.flags_ready:
%endif
        xor eax, eax
        mov esi, [ebp + RMS_ESI]
        ;mov  ds, eax
        ;mov  es, eax
        mov fs, eax
        mov gs, eax
        mov eax, [ebp + RMS_EAX]
        mov ebp, [ebp + RMS_EBP]
        iretd

%ifdef VMM
        LLabel PassupIretLTrapH
        push ebp
        mov ebp, OffPMStack
        RRT
        pushfd
        cli
        push ss
        pop ds
        push eax
        mov eax, [OffSavedPMStack]
        mov [ebp], eax
        mov eax, [OffSavedPMStack + 4]
        mov byte [OffLockedMode], 0
        jmp short PassupIretTrapE
%endif
        DPROC PassupIretTrapH
        push ebp
        mov ebp, OffPMStack
        RRT
        pushfd
        cli
        push eax
                                   ;set client PMStack from PL1 stack bottom
%assign F 3 * 4
%ifdef NativeHost
        cmp byte [ss:OffNativeMode], 1
        ja .ordinary_stack
        cmp dword [ss:OffNativeCurrent], 0
        je .ordinary_stack
        push edx
        call NativeLockedBridgeLeave
        mov [ss:ebp], eax
        mov eax, edx
        pop edx
        jmp short .stack_ready
.ordinary_stack:
%endif
        mov eax, [esp + F + CFrESP]
        mov [ebp], eax
        mov eax, [esp + F + CFrSS]
%ifdef NativeHost
.stack_ready:
%endif
        push ss
        pop ds
PassupIretTrapE:
        mov [ebp + 4], eax
        mov eax, [OffRMStack]
        RRT
        movzx ebp, ax
        dec bp
        sub ebp, VMIStruct_size - 12 - 1 - 10
        ;jc RMStackFault
        shr eax, 16
        push eax
        shl eax, 4
        add eax, ebp
        pop dword [eax + VMI_SS]
        add ebp, VMI_EAX
        mov [eax + VMI_ESP], ebp
        movzx ebp, word [eax + VMI_IP - RMSStruct_size + RMS_ES]
        mov [eax + VMI_ES], ebp
        mov bp, [eax + VMI_IP - RMSStruct_size + RMS_DS]
        mov [eax + VMI_DS], ebp
        mov bp, [eax + VMI_IP - RMSStruct_size + RMS_FS]
        mov [eax + VMI_FS], ebp
        mov bp, [eax + VMI_IP - RMSStruct_size + RMS_GS]
        mov [eax + VMI_GS], ebp
        pop dword [eax + VMI_EAX]
        pop ebp                  ;restore EFlags
        mov [eax + VMI_Flags], bp
        pop ebp
;        add  esp, 8               ;drop return address
;        pop  PMStack              ;retore client stack base
;        RRT
;        pop  PMStack[4]
;        RRT
        VCPICallTrap


        ESEG Text
