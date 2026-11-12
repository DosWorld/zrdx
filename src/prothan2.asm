;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; Second half of prothand.asm (split by size, task 150):
; it is included at the end of prothand.asm.

;PM handler for VM->PM   VCPI switch
        DPROC RMS_PMHandler
        mov eax, Data0Selector
        movzx esi, bp
        mov ds, eax
        mov es, eax
        mov ss, eax
          mov esp, OffKernelStack
        mov eax, [OffPMCR0]
        RRT
        mov cr0, eax
        ; Real DOS may clear IOPL. Ring-1 cleanup uses CLI/STI as
        ; well.
        pushfd
        or byte [esp + 1], 30h
        popfd
        ; ajust RM sp to top of normal RM stack
        add bp, RMSStruct_size
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
        ; restore segment registers from client stack
        mov fs, [esi + PMI_FS]
        mov gs, [esi + PMI_GS]
        mov es, [esi + PMI_ES]
        mov ds, [esi + PMI_DS]
        ; and transfer RON from RM stack
        mov eax, [ebp + RMS_EAX]
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
; RMIE - client stack frame for real mode interrupt emulation
; (DPMI service 30X)

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
        ; restore the protected caller's full flags
        mov [esp], ecx
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
        ; save on RM stack all RM segment registers
        sub word [OffRMStack], 10
        RRT 1
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
        push es
        push esi
        pushfd
%ifdef NativeHost
        cmp dword [OffNativeCurrent], 0
        jne .active_vector
        push Trap3Selector
        lea eax, [eax*4 + OffDefIntTrap3 \
                - FirstPassupSwitchCode*4]
        push eax
        jmp short .vector_ready
.active_vector:
%endif
        push dword [eax*8 + OffClientIDT \
                - ((FirstPassupSwitchCode) * 8) + 4]
        push dword [eax*8 + OffClientIDT \
                - ((FirstPassupSwitchCode) * 8) + 0]
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
        ; start client with NT = 0 and IOPL = 3
        and ah, ~(40h)
        or ah, 30h
        mov [esp + 8], eax
                                       ;replace flags
        ; with flags from RM int stack frame
        mov ax, [ebp + RMSStruct_size + 4]
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

        DPROC PassupIretTrapH
        push ebp
        mov ebp, OffPMStack
        RRT
        pushfd
        cli
        push eax
                                   ; set client PMStack from PL1
                                   ; stack bottom
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
