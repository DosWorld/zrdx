;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; Second half of dpmifunc.asm (split by size, task 150):
; it is included at the end of dpmifunc.asm.

;copy parameters
        _ifnot jcxz
          ; reload client esp to esi
          mov esi, [esp + F + DPMIFrame_SF + CFrESP]
          push edi
          push es
          ; interrupt frame length
          add esi, 12
%ifdef NativeHost
          call NativeRMParameters
%endif
          lea edi, [ebp + edx + VMIStruct_size - 2]
          push ss
          pop es
          cld
          rep movsw
          pop es
          pop edi
          ;add  ebp, edx
          ;$do
          ; mov  ax, ds:[esi+ecx*2-2+12]    ;get parameter from
          ; client stack
          ; mov  ss:[ebp+ecx*2-2+size VMIStruct-2], ax ;put it on
          ; the RM stack
          ;$enddo loop
          ;sub  ebp, edx
        _endif
;set flags value for RM iret, if requered
        or edx, edx          ;interrupt stack frame mode ?
        push es
        pop ds
        mov dx, [edi + DC_Flags]
        _ifnot jz
          ; put flags to RM stack iret frame
          mov [ebp + VMI_EndFlags], dx
          and dh, ~(3)       ;clear TF and IF for initial flags
        _endif
        pop eax               ;saved client eax
        mov [ebp + VMI_Flags], dx    ;set initial flags

        or al, al            ;Fn number == 0  ?
        mov edx, [edi + DC_IP]
        _ifnot jnz
          movzx edx, bl
          ; call CS:IP from current interrupt vector for function
          ; 0 only
          mov edx, [ss:edx*4]
        _endif
        db 0C7h, 45h, VMI_EndIP
        dw RMIERetSwitchCode, DGROUP16

; ss:ebp - RM switch stack, ds:edi - dos call struct, edx - start
; address
        test bh, 1
        jz SwitchToVMWithTransfer
        ; nested calls retain the outer RM policy
        mov byte [ss:OffA20Reset], 1
        RRT 1
SwitchToVMWithTransfer:
        movzx eax, word [edi + DC_GS]
        mov [ebp + VMI_IP], edx
;prepare VCPI stack frame
        movzx ecx, word [edi + DC_FS]
        mov [ebp + VMI_GS], eax
        movzx edx, word [edi + DC_ES]
        mov [ebp + VMI_FS], ecx
        movzx eax, word [edi + DC_DS]
        mov [ebp + VMI_ES], edx
        mov [ebp + VMI_DS], eax

;load all other registers from DPMI table
        mov edx, [edi + DC_EAX]
        mov eax, ebp
        mov [ebp + VMI_EAX], edx

        mov ebx, [edi + DC_EBX]
        mov ecx, [edi + DC_ECX]
        mov edx, [edi + DC_EDX]
        mov esi, [edi + DC_ESI]
        mov ebp, [edi + DC_EBP]
        mov edi, [edi + DC_EDI]
        VCPICallTrap
        ;jmp  SwitcherToVM
RStackOverflow@10:
        pop eax
        mov esi, [esp + DPMIFrame_SF + CFrESP]  ;@@CSR
        mov edx, [esi + 12 - RMIEStruct_size + RMIE_EDX]
        mov ecx, [esi + 12 - RMIEStruct_size + RMIE_ECX]
        jmp DPMIError1
;allocate RM callback
        DPMIFn 3, 3
        mov esi, (nMaxCallbacks * CBTStruct_size) / 3
        _do
          sub esi, 4 ; (size CBTStruct)/3
          ; free callback not found
          jb DPMIError1
          cmp word [esi + esi*2 + OffCallbacksTable + CBT_CS], 0
        _enddo jne
        mov ebp, [esp + DPMIFrame_DS]
        or ebp, ebp
        je DPMIError1
        lea dx, [si + OffFirstSwitchCode + MaxSystemSwitchCode]
        lea esi, [esi + esi*2 + OffCallbacksTable]
        mov [esi + CBT_CS], bp
        mov ebp, [esp + DPMIFrame_ESI]
        mov [esi + CBT_EIP], ebp
        mov [esi + CBT_SPtrOff], edi
        mov [esi + CBT_SPtrSeg], es
        mov cx, DGROUP16
        FnRet
;Free RM callback
        DPMIFn 3, 4
        cmp cx, DGROUP16
        jne DPMIError1
        movzx esi, dx
        lea esi, [esi + esi*2 - ((OffFirstSwitchCode \
                + MaxSystemSwitchCode) * 3)]
        test esi, 3         ;address must be dword aligned
        jne DPMIError1
        cmp esi, nMaxCallbacks * CBTStruct_size
        jae DPMIError1
        cmp word [esi + OffCallbacksTable + CBT_CS], 0
        je DPMIError1       ;unused or already freed callback
        mov word [esi + OffCallbacksTable + CBT_CS], 0
        FnRet


        DPROC PhMap
%define _mapregionstart eax
%define _mapregionstartb al
%define _mapregionend ebp
%define _lastdi edx
%define _firstdi eax
%define _tmappedpage esi
%define _t edx

        DPMIFn 8, 0
        pushad
        push ds
        pop es
        shl ebx, 16
        mov esi, [esp + (8 * 4) + DPMIFrame_ESI]
        shl esi, 16
        mov bx, cx
        mov si, di
        mov _mapregionstart, ebx
        and _mapregionstart, ~(0FFFh)
        test esi, esi
        jz near MapRangeError@12
        cmp ebx, 100000h
        jb near MapRangeError@12
        mov _mapregionend, ebx
        add _mapregionend, esi
        jnc MapRound@12
        test _mapregionend, _mapregionend
        jnz near MapRangeError@12
MapRound@12: ;zero denotes an exclusive physical end of 4 GB
        add _mapregionend, 0FFFh
        xor ebx, _mapregionstart
        and _mapregionend, ~(0FFFh)
        push _mapregionstart
        push _mapregionend
        sub _firstdi, _mapregionend
        mov _lastdi, dword [OffRootMCB + MCB_StartOffset]
        add _firstdi, _lastdi
        jnc near Err@12
        mov ecx, [OffRootMCB + MCB_Prev]
        cmp _firstdi, dword [ecx + MCB_EndOffset]
        jb near Err@12
        push _firstdi
        ;uses @@FirstDI, @@LastDI, ecx, esi, edi, ebp
        shr _firstdi, 22
        shr _lastdi, 22
        cmp _firstdi, _lastdi
        jae MapTables@12
        test byte [OffPageDir + _firstdi*4], 1
        jz MapTables@12
        ; share the normal block's boundary table
        inc _firstdi
MapTables@12:
        ; first newly owned directory for rollback
        push _firstdi
        _do jmp
          lea edi, [OffPageDir + (_firstdi * 4)]
          mov ecx, _lastdi
          sub ecx, _firstdi
          call XAllocPages
          jz Recover@12
          add _firstdi, ecx
        _while
        cmp _lastdi, _firstdi
        _enddo jne
        add esp, 4
        pop _tmappedpage
        pop _mapregionend
        pop _mapregionstart
        add ebx, _tmappedpage
        mov [esp + PA_ECX], bx
        shr ebx, 16
        mov [esp + PA_EBX], bx
        mov dword [OffRootMCB + MCB_StartOffset], _tmappedpage
        mov _mapregionstartb, 67h
        LLabel PatchPoint4
        movzx _t, _mapregionstartb
        ; compare matching PTE attributes, including PCD
        or _mapregionend, _t
        shr _tmappedpage, 12
MapDirectory@12:
        mov _t, _tmappedpage
        shr _t, 10
        cli
        mov edi, [OffPageDir + _t*4]
        mov [OffPage2 + ((OffPageTableWin - KernelBase) \
                >> 10)], edi
        lea edi, [OffnEntriesInTable + _t*4]
        InvalidateTLB
        movzx edx, si
        and dh, 3
MapEntries@12:
        inc dword [edi]
        mov [OffPageTableWin + _t*4], _mapregionstart
        add _mapregionstart, 1000h
        inc _tmappedpage
        inc _t
        cmp _mapregionend, _mapregionstart
        je MapComplete@12
        ; 1024 entries: reload the window once per PDE
        test dh, 4
        jz MapEntries@12
        ; IRQs may reuse the window between directories
        sti
        jmp MapDirectory@12
MapComplete@12:
        sti
        popad
        ret
MapRangeError@12:
        popad
        mov al, 21h
        jmp DPMIError1
Recover@12:
        mov edx, eax
        pop ebx
        call FreeNewMapTables
        pop eax                    ;original mapping start
Err@12:
        pop eax
        pop eax
        popad
        jmp DPMIError1


;ebx - first page
;edx(@@LastDI) - last page
FreeHMPages:
        mov eax, ebx
        shr eax, 12
        shl edx, 10
        ; shared page walker, no physical release/IRQ
        push 2
        call FreePagesN
        ret

; New mapping tables have no installed entries yet; rollback
; frees only
; directories allocated by this call, excluding an existing
; boundary.
FreeNewMapTables:
.loop:  cmp ebx, edx
        jae .done
        push ebx
        push edx
        lea esi, [OffPageDir + ebx*4]
        mov ecx, 1
        call XFreePages
        pop edx
        pop ebx
        inc ebx
        jmp .loop
.done:  ret

        ESEG Text
