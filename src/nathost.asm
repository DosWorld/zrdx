; Native DPMI frontend. Explicit profile; not in the bound-image build.
; Budget and shared-kernel decision: docs/D09WORK.MD.

; Entry snapshot preserves live transferred registers and the RM frame.
NS_BX equ 0
NS_CX equ 4
NS_DX equ 8
NS_DI equ 12
NS_RM equ 16
NS_PM equ 20
NS_Frames equ 28
NS_TSS equ 32
NS_RMS equ 36
NS_Size equ NS_RMS + RMSStruct_size
NC_Parent equ NS_Size
NC_Handle equ NC_Parent + 4
NC_PSP equ NC_Handle + 4
NC_Env equ NC_PSP + 2
NC_Private equ NC_Env + 2
NC_Mode equ NC_Private + 2
NC_CS equ NC_Mode + 2
NC_DS equ NC_CS + 2
NC_SS equ NC_DS + 2
NC_PspSel equ NC_SS + 2
NC_EnvSel equ NC_PspSel + 2
NC_WorkSel equ NC_EnvSel + 2
NC_IP equ NC_WorkSel + 2
NC_SP equ NC_IP + 4
NC_Callbacks equ 96
NC_Debug equ 100
NC_Physical equ 104
NC_Protected equ 108        ;private PTEs must be restored before recycling
NC_RMAlias equ 112
NC_SelBits equ 128
NC_MemBits equ NC_SelBits + 1024
NC_MemBytes equ ((OffClientPages-OffMCBVector)/MCBStruct_size+7)/8
NC_Stack equ (NC_MemBits + NC_MemBytes + 4095) & ~4095
NC_Kernel1 equ NC_Stack - (OffKernelStack1End - OffKernelStack1Start)
%if NC_Kernel1 < NC_MemBits + NC_MemBytes
%error native context alignment padding cannot hold the kernel continuation
%endif
NC_Frames equ NC_Stack + LockedStackSize
NC_Size equ NC_Frames + 4096

        VSegm BSS16
        DFB NativeTail, 128
        DFB NativeFCB, 74
        DFB NativeExecBlock, 14
        EVSeg BSS16

        SEGM Text16
        LWord NativeRootPSP
        dw 0
        LWord NativeRMCS
        dw 0
        LWord MemBlock0Size
        dw 0
        LByte NativeCPU
        db 0
        LByte NativePending
        db 0
        LByte NativeExitCode
        db 0
        LWord NativePendingPSP
        dw 0
        LDWord NativeOld2F
        dd 0
        DPROC NativeDiscovery
        cmp ax, 1687h
        jne .chain
        xor ax, ax
        mov bx, 1
        mov cl, [cs:ROffNativeCPU]
        mov dx, 005Ah
        mov si, (600 + 15) / 16
        push cs
        pop es
        mov di, NativeRMEntry
        iret
.chain: jmp far [cs:ROffNativeOld2F]
NativeRMEntry:
        pushfd
        pushad
        mov ah, 51h
        int 21h
        mov [cs:ROffNativePendingPSP], bx
        popad
        popfd
        mov byte [cs:ROffNativePending], 1
        push word RawSwitchCode
        ret

; Root command tail holds an 8.3 DOS program path and its arguments.
        DPROC NativeRootExec
        mov es, [cs:ROffNativeRootPSP]
        mov bx, [cs:ROffMemBlock0Size]
        mov ah, 4Ah
        int 21h
        jc .bad
        mov ds, [cs:ROffNativeRootPSP]
        mov si, 81h
.space: lodsb
        cmp al, ' '
        je .space
        cmp al, 9
        je .space
        cmp al, 13
        je .bad
        dec si
        mov dx, si
.name:  lodsb
        cmp al, ' '
        je .tail
        cmp al, 9
        je .tail
        cmp al, 13
        jne .name
.tail:  mov byte [si-1], 0
        movzx cx, byte [80h]
        add cx, 81h
        sub cx, si
        jnc .length
        xor cx, cx
.length:
        push cs
        pop es
        mov [cs:ROffNativeTail], cl
        mov di, ROffNativeTail + 1
        cld
        rep movsb
        mov al, 13
        stosb
        push ds
        push dx
        push cs
        pop ds
        xor eax, eax
        mov di, ROffNativeFCB
        mov cx, 74 / 2
        rep stosw
        mov si, ROffNativeTail + 1
        mov di, ROffNativeFCB
        mov ax, 2901h
        int 21h
        mov di, ROffNativeFCB + 37
        mov ax, 2901h
        int 21h
        mov di, ROffNativeExecBlock
        xor ax, ax
        stosw
        mov ax, ROffNativeTail
        stosw
        mov ax, cs
        stosw
        mov ax, ROffNativeFCB
        stosw
        mov ax, cs
        stosw
        mov ax, ROffNativeFCB + 37
        stosw
        mov ax, cs
        stosw
        pop dx
        pop ds
        mov bx, ROffNativeExecBlock
        mov ax, 4B00h
        int 21h
        jc .bad
        mov ah, 4Dh
        int 21h
        jmp short .done
.bad:   mov al, 1
.done:  mov [cs:ROffNativeExitCode], al
        mov byte [cs:ROffNativePending], 2
        push word RawSwitchCode
        ret
        DPROC NativeRootExit
        mov al, [cs:ROffNativeExitCode]
        mov ah, 4Ch
        int 21h
        ESEG Text16

        SEGM Text
; Both protected stacks share one existing kernel PTE. A layout change
; must not silently leave part of either stack user accessible.
%if (OffKernelStack1Start & ~0FFFh) != ((OffKernelStack - 1) & ~0FFFh)
%error Native kernel stacks must remain within their protected page
%endif
NativeHostStart:
        cli
        mov edi, OffIDT
        push 16
        pop ecx
.low:   and byte [edi + 5], ~60h
        add edi, 8
        loop .low
        mov dword [OffIDT + 2*8], (Code0Selector << 16) | (OffNativeCPU2 & 0FFFFh)
        mov dword [OffIDT + 2*8 + 4], (KernelBase & 0FFFF0000h) | 8E00h
        mov eax, (Code0Selector << 16) | (OffNativeCPU16 & 0FFFFh)
        push 16
        pop ecx
.high:  mov [edi], eax
        mov dword [edi + 4], (KernelBase & 0FFFF0000h) | 8E00h
        add edi, 8
        add ax, 4
        loop .high
        mov byte [OffNativeMode], 0FFh
        mov edx, DGROUP16
        shl edx, 4
        mov [OffNativeRealBase], edx
        mov eax, [2Fh * 4]
        mov [edx + ROffNativeOld2F], eax
        mov word [2Fh * 4], ROffNativeDiscovery
        mov word [2Fh * 4 + 2], DGROUP16
        ; Bootstrap calls can still use the original user anchor. Then
        ; reserve one root IRQ page before making kernel stacks private.
        mov dword [OffNativeRootStack], OffUserStackEnd
        xor ebx, ebx
        mov ecx, LockedStackSize
        mov eax, 0501h
        call NativeCall31
        jc NativeRootFail
        shl ebx, 16
        mov bx, cx
        add ebx, LockedStackSize
        mov [OffNativeRootStack], ebx
        mov [OffKernelStack1End - 8], ebx
        and byte [OffPage2 + (((OffKernelStack - 1) & ~0FFFh) - KernelBase) / 1024], ~4
        InvalidateTLB
        mov bl, 2Fh
        mov cx, Code3Selector
        mov edx, OffNativePM2F
        mov eax, 0205h
        call NativeCall31
        mov ebx, ROffNativeRootExec
        jmp NativeRootVM
NativeRootFail:
        mov ebx, [OffNativeRealBase]
        mov byte [ebx + ROffNativeExitCode], 1
NativeRootDone:
        mov ebx, ROffNativeRootExit
NativeRootVM:
        cli
        mov esp, [OffNativeRealBase]
        add esp, ROffExitStackEnd - NExitPages * 4 - TSFrameSize
        xor eax, eax
        call NativeKernelStack
        jmp SimpleSwitchToVM
        DPROC NativeResume
        push ss
        pop ds
        push ss
        pop es
        mov esi, [OffNativeRealBase]
        cmp byte [esi + ROffNativePending], 2
        je NativeRootDone
        mov byte [esi + ROffNativePending], 0
        push edi
        mov edi, OffNativeBoot
        mov [edi + NS_BX], ebx
        mov [edi + NS_CX], ecx
        mov [edi + NS_DX], edx
        pop eax
        mov [edi + NS_DI], eax
        mov esi, [OffNativeRealBase]
        add esi, ROffRMStack
        mov edi, OffNativeBoot + NS_RM
        push 4
        pop ecx
        cld
        rep movsd
        mov eax, [OffTSS + TSS_ESP1]
        mov [OffNativeBoot + NS_TSS], eax
        mov edi, OffNativeBoot + NS_RMS
        mov esi, ebp
        mov ecx, RMSStruct_size / 4
        cld
        rep movsd
        movzx eax, word [OffNativeBoot + NS_RMS + RMS_EAX]
        cmp eax, 1
        ja near NativeEntryFail
        cmp byte [OffNativeMode], 0FFh
        je .bitness
        cmp al, [OffNativeMode]
        jne near NativeEntryFail
.bitness:
        movzx ebx, word [OffNativeBoot + NS_RMS + RMS_ES]
        test ebx, ebx
        jz near NativeEntryFail
        add ebx, (600 + 15) / 16
        cmp ebx, 10000h
        ja near NativeEntryFail
        xor ebx, ebx
        mov ecx, NC_Size
        mov eax, 0501h
        call NativeCall31
        jc near NativeEntryFail
        shl ebx, 16
        mov bx, cx
        shl esi, 16
        mov si, di
        mov ebp, ebx
        mov edx, esi
        mov edi, ebx
        xor eax, eax
        mov ecx, NC_Size / 4
        rep stosd
        mov [ebp + NC_Handle], edx
        mov eax, [OffRootMCB + MCB_StartOffset]
        mov [ebp + NC_Physical], eax
        mov esi, [OffNativeCurrent]
        mov [ebp + NC_Parent], esi
        mov [OffNativeCurrent], ebp
        mov esi, OffNativeBoot
        mov edi, ebp
        mov ecx, NS_Size / 4
        rep movsd
        lea eax, [ebp + NC_Frames]
        mov edx, [OffNativeRealBase]
        mov [edx + ROffNativeFrameNext], eax
        mov ax, [ebp + NS_RMS + RMS_EAX]
        mov [ebp + NC_Mode], ax
        call NativeSetLockedStack
        cmp word [ebp + NC_Mode], 0
        jne .alias_ready
        xor eax, eax
        mov ecx, 0FFFFh
        mov edx, 0F2h
        call NativeMakeSel
        jc NativeInitAbort
        mov [ebp + NC_RMAlias], ax
.alias_ready:
        mov ax, [ebp + NS_RMS + RMS_ES]
        mov [ebp + NC_Private], ax
        mov esi, [OffNativeRealBase]
        mov ax, [esi + ROffNativePendingPSP]
        mov [ebp + NC_PSP], ax
        movzx esi, ax
        shl esi, 4
        mov ax, [esi + 2Ch]
        mov [ebp + NC_Env], ax
        movzx eax, ax
        mov ecx, 0FFFFh
        mov edx, 0F2h
        call NativeMakeSel
        jc near NativeInitAbort
        mov [ebp + NC_EnvSel], ax
        movzx eax, word [ebp + NC_PSP]
        mov ecx, 0FFh
        call NativeMakeSel
        jc near NativeInitAbort
        mov [ebp + NC_PspSel], ax
        mov eax, [ebp + NS_RM]
        movzx esi, ax
        shr eax, 16
        shl eax, 4
        add esi, eax
        movzx eax, word [esi + 2]
        movzx ecx, word [esi]
        mov [ebp + NC_IP], ecx
        mov ecx, 0FFFFh
        mov edx, 0FAh
        call NativeMakeSel
        jc near NativeInitAbort
        mov [ebp + NC_CS], ax
        movzx eax, word [ebp + NS_RMS + RMS_DS]
        movzx edx, word [ebp + NC_Mode]
        shl edx, 14
        or edx, 0F2h
        call NativeMakeSel
        jc near NativeInitAbort
        mov [ebp + NC_DS], ax
        mov [ebp + NC_SS], ax
        mov eax, [ebp + NS_RM]
        shr eax, 16
        cmp ax, [ebp + NS_RMS + RMS_DS]
        je .stack
        call NativeMakeSel
        jc near NativeInitAbort
        mov [ebp + NC_SS], ax
.stack: movzx edi, word [ebp + NS_RM]
        add di, 4
        sub di, 12
        mov [ebp + NC_SP], edi
        mov eax, [ebp + NS_RM]
        shr eax, 16
        shl eax, 4
        add edi, eax
        mov eax, [ebp + NC_IP]
        stosd
        movzx eax, word [ebp + NC_CS]
        stosd
        movzx eax, word [ebp + NS_RMS + RMS_Flags]
        or ah, 30h
        and ah, ~40h
        and al, ~1
        stosd
        call NativeCreateThunks
        jc NativeInitAbort
        cmp word [ebp + NC_Mode], 0
        jne .native_vectors
        call NativeInstall16
        jc NativeInitAbort
.native_vectors:
        movzx eax, word [ebp + NC_PSP]
        shl eax, 4
        mov dx, [ebp + NC_EnvSel]
        mov [eax + 2Ch], dx
        movzx eax, word [ebp + NC_Private]
        shl eax, 16
        mov ax, 600
        mov esi, [OffNativeRealBase]
        mov [esi + ROffRMStack], eax
        call NativeProtectContext
        mov ax, [ebp + NC_Mode]
        mov [OffNativeMode], al
        cli
        movzx eax, word [ebp + NC_SS]
        push eax
        mov eax, [ebp + NC_SP]
        add eax, 12
        push eax
        movzx eax, word [ebp + NS_RMS + RMS_Flags]
        or ah, 30h
        and ah, ~40h
        and al, ~1
        push eax
        movzx eax, word [ebp + NC_CS]
        push eax
        push dword [ebp + NC_IP]
        mov es, [ebp + NC_PspSel]
        mov ds, [ebp + NC_DS]
        xor eax, eax
        mov fs, ax
        mov gs, ax
        mov edi, [ebp + NS_DI]
        mov ecx, [ebp + NS_CX]
        mov edx, [ebp + NS_DX]
        mov esi, [ebp + NS_RMS + RMS_ESI]
        mov eax, [ebp + NS_RMS + RMS_EAX]
        mov ebx, [ebp + NS_BX]
        mov ebp, [ebp + NS_RMS + RMS_EBP]
        iretd

; Invoke the same dispatcher from ring 1. The synthetic gate's old SS
; identifies an internal call independently of native client bitness.
NativeCall31:
        push dword [OffTSS + TSS_ESP1]
        pushfd                     ;caller flags remain private
        cli
        push dword [esp]
        and byte [esp + 1], ~2      ;return through the unwinder with IF=0
        push cs
        push OffNativeCallReturn
        push Data3Selector         ;separate public IRQ anchor
        push dword 0
        push ss
        push esp
        add dword [esp], 12         ;private synthetic IRET, not the IRQ stack
        push cs
        push dword 0
        push eax
        mov eax, [OffNativeCurrent]
        test eax, eax
        jz .bootstrap
        add eax, NC_Stack + LockedStackSize
        jmp short .anchor
.bootstrap:
        mov eax, [OffNativeRootStack]
.anchor:
        mov [esp + 4 + CFrame_size], eax
        lea eax, [esp + 4 + CFrame_size + 8]
        mov [OffTSS + TSS_ESP1], eax
        pop eax
        jmp DPMIIntEntry
        DPROC NativeCallReturn
        ;Same-ring IRET leaves two stack-switch words unconsumed. Skip
        ;the saves, fake gate, IRQ anchor and private synthetic IRET.
        lea esp, [esp + 8 + DPMIFrame_SF - 4 + CFrame_size + 8 + 12]
        rcr dword [esp], 1
        rol dword [esp], 1           ;merge service CF into original flags
        push dword [esp + 4]
        pop dword [OffTSS + TSS_ESP1]
        popfd
        lea esp, [esp + 4]
        ret

; Initial selector setup uses the public descriptor operations and the
; existing contiguous allocator; only the native ownership bit is new.
NativeMakeSel:
        pushad
        push 1
        call AllocSelectors
        jc .failed
        xchg eax, [esp + PA_EAX] ;publish AX, recover the original real segment
        mov edi, [OffNativeCurrent]
        bts [edi + NC_SelBits], esi
        ; Trusted entry-only inputs: real segment, limit FFh/FFFFh and
        ; F2h/FAh rights with optional B. No public validation is bypassed.
        shl eax, 4
        mov [ebp + 2], eax
        mov ecx, [esp + PA_ECX]
        mov [ebp], cx
        mov edx, [esp + PA_EDX]
        mov [ebp + 5], dl
        mov [ebp + 6], dh
        popad
        clc
        ret
.failed:
        popad
        stc
        ret

NativeInitAbort:
        call NativeFreeInitial
NativeEntryFail:
        mov esp, [OffNativeRealBase]
        add esp, ROffExitStackEnd - NExitPages * 4 - TSFrameSize
        xor eax, eax
        call NativeKernelStack
        mov edi, [OffNativeRealBase]
        add edi, ROffRMStack
        mov esi, OffNativeBoot + NS_RM
        push 4
        pop ecx
        cld
        rep movsd
        mov eax, [OffNativeBoot + NS_TSS]
        mov [OffTSS + TSS_ESP1], eax
        mov eax, [OffNativeBoot + NS_RM]
        movzx edi, ax
        shr eax, 16
        mov ecx, eax
        shl eax, 4
        add eax, edi
        mov edx, [eax]           ;original FAR return, before overwriting
        add di, 4 - VMIShortStruct_size
        mov eax, ecx
        shl eax, 4
        add edi, eax
        mov [edi + VMI_SS], ecx
        mov eax, [OffNativeBoot + NS_RM]
        movzx eax, ax
        sub ax, 6
        mov [edi + VMI_ESP], eax
        mov [edi + VMI_IP], edx
        mov ax, [OffNativeBoot + NS_RMS + RMS_Flags]
        or ax, 1
        mov [edi + VMI_Flags], ax
        mov eax, [OffNativeBoot + NS_RMS + RMS_EAX]
        mov [edi + VMI_EAX], eax
        movzx eax, word [OffNativeBoot + NS_RMS + RMS_DS]
        mov [edi + VMI_DS], eax
        movzx eax, word [OffNativeBoot + NS_RMS + RMS_ES]
        mov [edi + VMI_ES], eax
        movzx eax, word [OffNativeBoot + NS_RMS + RMS_FS]
        mov [edi + VMI_FS], eax
        movzx eax, word [OffNativeBoot + NS_RMS + RMS_GS]
        mov [edi + VMI_GS], eax
        mov eax, edi
        mov ebp, OffNativeBoot
        mov ebx, [ebp + NS_BX]
        mov ecx, [ebp + NS_CX]
        mov edx, [ebp + NS_DX]
        mov edi, [ebp + NS_DI]
        mov esi, [ebp + NS_RMS + RMS_ESI]
        mov ebp, [ebp + NS_RMS + RMS_EBP]
        jmp SwitcherToVM

NativeFreeInitial:
        mov ebp, [OffNativeCurrent]
        call NativeReleaseResources
        call NativeReleaseMappings
        xor esi, esi
.loop:  btr [ebp + NC_SelBits], esi
        jnc .next
        btr [OffLDTFree], esi
        btr [OffLDTDOS], esi
        btr [OffLDTBlock], esi
        mov byte [OffLDT + esi*8 + 5], 0
.next:  inc esi
        cmp esi, 2000h
        jb .loop
        mov eax, [ebp + NS_Frames]
        mov edx, [OffNativeRealBase]
        mov [edx + ROffNativeFrameNext], eax
        cmp byte [ebp + NC_Protected], 0
        je .public_pages
        call NativeProtectContext
.public_pages:
        mov edx, [ebp + NC_Handle]
        mov eax, [ebp + NC_Parent]
        mov [OffNativeCurrent], eax
        call NativeSetLockedStack
        mov esi, edx
        shr esi, 16
        mov edi, edx
        mov eax, 0502h
        call NativeCall31
        ret

NativeClientExit:
        cli
        push Data1Selector
        pop ds
        push ds
        pop es
        mov ebp, [OffNativeRealBase]
        mov [ebp + ROffNativeExitCode], al
        mov ax, Data1Selector
        mov ds, ax
        mov es, ax
        xor eax, eax
        mov fs, ax
        mov gs, ax
        mov ebp, [OffNativeCurrent]
        movzx eax, word [ebp + NC_PSP]
        shl eax, 4
        mov dx, [ebp + NC_Env]
        mov [eax + 2Ch], dx
        ;Cleanup may service an IRQ through a private anchor. Restore the
        ;parent bridge only after the final private free has returned.
        push dword [ebp + NS_PM + 4]
        push dword [ebp + NS_PM]
        push dword [ebp + NS_TSS]
        call NativeFreeInitial
        pop dword [OffTSS + TSS_ESP1]
        mov esi, [OffNativeRealBase]
        pop dword [esi + ROffPMStack]
        pop dword [esi + ROffPMStack + 4]
        jmp NativeRootDone

        DPROC NativePM2F
        cmp ax, 1686h
        jne .chain
        xor ax, ax
        iretd
.chain: db JmpFarCode
        dd OffDefIntTrap3 + 2Fh * 4
        dw Trap3Selector

; The public ABI is selected by entry, not by CS.D. Internal calls from
; ring 1 keep their full pointers and bypass client adaptation.
NativeBefore31:
        mov esi, [esp + DPMIFrame_SF + CFrCS]
        and esi, 3
        cmp esi, 3
        jne .done
        mov esi, [OffNativeCurrent]
        test esi, esi
        jz .done
        lea ebp, [esp + DPMIFrame_SF]
        call NativePrepareFrame
        cmp ax, 1
        je .stack
        cmp ax, 0101h
        jne .stack_ok
.stack: push ecx
        mov cx, bx
        cmp ax, 1
        je .selector
        mov cx, dx
.selector:
        mov esi, [esp + 4 + DPMIFrame_SF + CFrESP]
        mov ebp, [esp + 4 + DPMIFrame_SF + CFrSS]
        cmp bp, Data1Selector
        jne .actual_ss
        movzx ebp, word [ss:esi - NF_Iret + NF_SS]
.actual_ss:
        xor bp, cx
        test bp, ~3
        pop ecx
        jnz .stack_ok
        cmp ax, 1
        je .badstack
        mov ax, 9
        stc
        ret
.badstack:
        mov ax, 8022h
        stc
        ret
.stack_ok:
        cmp ax, 0502h
        je .memory
        cmp ax, 0503h
        jne .writable
.memory:
        push ecx
        mov ecx, [esp + 4 + DPMIFrame_OrigSI]
        shl ecx, 16
        mov cx, di
        sub ecx, OffMCBVector
        test ecx, 15
        jnz .badmem
        cmp ecx, OffClientPages - OffMCBVector
        jae .badmem
        shr ecx, 4
        mov esi, NC_MemBits
        call NativeFindOwner
        test ebp, ebp
        jz .badmem
        pop ecx
.writable:
        cmp ax, 1
        je .protect
        cmp ax, 7
        jb .mode
        cmp ax, 9
        jbe .protect
        cmp ax, 000Ch
        jne .mode
.protect:
        push ecx
        mov esi, [OffNativeCurrent]
        movzx ebp, bx
        and ebp, ~3
        mov cx, [OffNativeThunkSel]
        and cx, ~3
        cmp bp, cx
        je .protected
.parent:
        mov cx, [esi + NC_PspSel]
        and cx, ~3
        cmp bp, cx
        je .protected
        mov cx, [esi + NC_EnvSel]
        and cx, ~3
        cmp bp, cx
        je .protected
        mov cx, [esi + NC_RMAlias]
        and cx, ~3
        cmp bp, cx
        je .protected
        mov esi, [esi + NC_Parent]
        test esi, esi
        jnz .parent
        pop ecx
.mode:  mov esi, [OffNativeCurrent]
        cmp word [esi + NC_Mode], 0
        jne .done
        cmp ax, 0303h
        jne .input
        movzx esi, word [esp + DPMIFrame_ESI]
        mov [esp + DPMIFrame_ESI], esi
.input: cmp ax, 0203h
        je .vector_in
        cmp ax, 0205h
        jne .buffer_in
.vector_in:
        movzx edx, dx
.buffer_in:
        cmp ax, 000Bh
        je .pointer
        cmp ax, 000Ch
        je .pointer
        cmp ax, 0500h
        je .pointer
        cmp ax, 0303h
        ja .done
        cmp ax, 0300h
        jb .done
.pointer:
        movzx edi, di
.done:  clc
        ret
.badmem:
        pop ecx
        mov ax, 8023h
        stc
        ret
.protected:
        pop ecx
        mov ax, 8022h
        stc
        ret

; Shared 0.9 tables permit an ancestor's resource to be used or freed.
; ECX is a bitmap index, ESI its context offset; EBP returns the owner.
NativeFindOwner:
        mov ebp, [OffNativeCurrent]
.next:  test ebp, ebp
        jz .done
        bt [ebp + esi], ecx
        jc .done
        mov ebp, [ebp + NC_Parent]
        jmp .next
.done:  ret

NativeAfter31:
        push ss
        pop ds                   ;090xh handlers leave DS on the client stack
        pushfd
        pushad
        call NativeAccount31
        popad
        mov esi, [esp + 4 + DPMIFrame_SF + CFrCS]
        and esi, 3
        cmp esi, 3
        jne .done
        mov esi, [OffNativeCurrent]
        test esi, esi
        jz .done
        cmp word [esi + NC_Mode], 0
        jne .done
        mov bp, [esp + 4 + DPMIFrame_Func]
        cmp bp, 0202h
        jb .not_vector
        cmp bp, 0205h
        ja .not_vector
        mov esi, [esp + 4 + DPMIFrame_OrigDX]
        test bp, 1
        jnz .vector_restore
        mov si, dx
.vector_restore:
        mov edx, esi
.not_vector:
        cmp bp, 0303h
        jne .input
        mov esi, [esp + 4 + DPMIFrame_OrigSI]
        mov [esp + 4 + DPMIFrame_ESI], esi
.input: cmp bp, 000Bh
        je .restore
        cmp bp, 000Ch
        je .restore
        cmp bp, 0500h
        je .restore
        cmp bp, 0303h
        ja .output
        cmp bp, 0300h
        jb .done
.restore:
        mov edi, [esp + 4 + DPMIFrame_OrigDI]
        jmp short .done
.output:
        cmp bp, 0306h
        ja .done
        test byte [esp], 1
        jnz .done
        mov esi, [esp + 4 + DPMIFrame_OrigDI]
        mov si, di
        mov edi, esi
.done:  popfd
        ret

; Called with PUSHAD and saved flags beneath its return. Account only
; successful public calls; internal setup/cleanup must not acquire owners.
NativeAccount31:
NAF equ 40
        test byte [esp + 36], 1
        jnz .done
        mov eax, [esp + NAF + DPMIFrame_SF + CFrCS]
        and eax, 3
        cmp eax, 3
        jne .done
        mov ebp, [OffNativeCurrent]
        test ebp, ebp
        jz .done
        movzx eax, word [esp + NAF + DPMIFrame_Func]
        cmp ax, 0000h
        je .array
        cmp ax, 0001h
        je .free_sel
        cmp ax, 0002h
        je .alias
        cmp ax, 000Ah
        je .one
        cmp ax, 000Dh
        je .specific
        cmp ax, 0501h
        je .alloc_mem
        cmp ax, 0502h
        je .free_mem
        cmp ax, 0503h
        je .resize_mem
        cmp ax, 0303h
        je .callback
        cmp ax, 0304h
        je .uncallback
        cmp ax, 0B00h
        je .debug
        cmp ax, 0B01h
        je .undebug
.done:  ret
.array: movzx edx, word [esp + 4 + PA_ECX]
        jmp short .sel_result
.one:   mov edx, 1
.sel_result:
        movzx ecx, word [esp + 4 + PA_EAX]
        shr ecx, 3
.setsel:
        bts [ebp + NC_SelBits], ecx
        inc ecx
        dec edx
        jnz .setsel
        ret
.specific:
        movzx ecx, word [esp + 4 + PA_EBX]
        shr ecx, 3
        mov edx, 1
        jmp .setsel
.alias: movzx ecx, word [esp + 4 + PA_EAX]
        shr ecx, 3
        mov esi, NC_SelBits
        call NativeFindOwner
        test ebp, ebp
        jnz .done
        mov ebp, [OffNativeCurrent]
        mov edx, 1
        jmp .setsel
.free_sel:
        movzx ecx, word [esp + 4 + PA_EBX]
        shr ecx, 3
        mov esi, NC_SelBits
        jmp .clear
.alloc_mem:
        call .mem_result
        bts [ebp + NC_MemBits], ecx
        ret
.free_mem:
        call .mem_original
        mov esi, NC_MemBits
        jmp .clear
.resize_mem:
        call .mem_original
        mov esi, NC_MemBits
        call NativeFindOwner
        btr [ebp + NC_MemBits], ecx
        call .mem_result
        bts [ebp + NC_MemBits], ecx
        ret
.mem_result:
        mov ecx, [esp + 4 + NAF + DPMIFrame_ESI]
        shl ecx, 16
        mov cx, [esp + 8 + PA_EDI]
        sub ecx, OffMCBVector
        shr ecx, 4
        ret
.mem_original:
        mov ecx, [esp + 4 + NAF + DPMIFrame_OrigSI]
        shl ecx, 16
        mov cx, [esp + 4 + NAF + DPMIFrame_OrigDI]
        sub ecx, OffMCBVector
        shr ecx, 4
        ret
.callback:
        call .cb_index
        bts [ebp + NC_Callbacks], ecx
        ret
.uncallback:
        call .cb_index
        mov esi, NC_Callbacks
        jmp .clear
.cb_index:
        movzx ecx, word [esp + 8 + PA_EDX]
        sub ecx, ROffFirstSwitchCode + MaxSystemSwitchCode
        shr ecx, 2
        ret
.debug: movzx ecx, word [esp + 4 + PA_EBX]
        bts [ebp + NC_Debug], ecx
        ret
.undebug:
        movzx ecx, word [esp + 4 + PA_EBX]
        mov esi, NC_Debug
.clear: call NativeFindOwner
        test ebp, ebp
        jz .done
        btr [ebp + esi], ecx
        ret

NativeReleaseResources:
        xor edx, edx
.cb:    btr [ebp + NC_Callbacks], edx
        jnc .nextcb
        lea eax, [edx*4 + ROffFirstSwitchCode + MaxSystemSwitchCode]
        push edx
        mov dx, ax
        mov cx, DGROUP16
        mov eax, 0304h
        call NativeCall31
        pop edx
.nextcb:
        inc edx
        cmp edx, nMaxCallbacks
        jb .cb
        xor edx, edx
.dr:    btr [ebp + NC_Debug], edx
        jnc .nextdr
        mov bx, dx
        mov eax, 0B01h
        call NativeCall31
.nextdr:
        inc edx
        cmp edx, 4
        jb .dr
        xor edx, edx
.dos:   bt [ebp + NC_SelBits], edx
        jnc .nextdos
        bt [OffLDTBlock], edx
        jnc .nextdos
        test byte [edx*8 + OffLDT + 6], 10h
        jz .nextdos
        push edx
        lea edx, [edx*8 + 7]
        mov eax, 0101h
        call NativeCall31
        pop edx
.nextdos:
        inc edx
        cmp edx, 2000h
        jb .dos
        xor edx, edx
.mem:   btr [ebp + NC_MemBits], edx
        jnc .nextmem
        mov edi, edx
        shl edi, 4
        add edi, OffMCBVector
        mov esi, edi
        shr esi, 16
        mov eax, 0502h
        call NativeCall31
.nextmem:
        inc edx
        cmp edx, NC_MemBytes * 8
        jb .mem
        ret

; Metadata is supervisor-only. The existing one-page locked client stack
; remains user-accessible. Pair the toggle on publication and final free;
; the physical pool retains PTE flags and must receive user pages back.
NativeProtectContext:
        pushad
        mov eax, ebp
        shr eax, 12
        push NC_Size / 4096
        pop ecx
        xor ebx, ebx
.page:  mov edx, eax
        shr edx, 10
        mov edx, [OffPageDir + edx*4]
        mov [OffPage2 + ((OffPageTableWin-KernelBase) >> 10)], edx
        InvalidateTLB
        mov esi, eax
        and esi, 3FFh
        cmp ebx, NC_Stack / 4096
        je .next
        xor byte [OffPageTableWin + esi*4], 4
.next:  inc eax
        inc ebx
        loop .page
        not byte [ebp + NC_Protected]
        InvalidateTLB
        popad
        ret

; The child owns the downward mapping interval below its entry snapshot.
; Preserve the parent's partial directory and free only new table pages.
NativeReleaseMappings:
        pushad
        mov eax, [OffRootMCB + MCB_StartOffset]
        mov edx, [ebp + NC_Physical]
        shr eax, 12
        shr edx, 12
        push 2
        call FreePagesN
        mov eax, [ebp + NC_Physical]
        mov [OffRootMCB + MCB_StartOffset], eax
        popad
        ret
; One existing 4 KB client stack, rebased on entry/return to a parent.
NativeSetLockedStack:
        pushad
        mov ebp, [OffNativeCurrent]
        mov byte [OffGDT + LockedStackSelector - 3 + 5], 0
        test ebp, ebp
        jz .done
        lea eax, [ebp + NC_Stack]
        mov word [OffGDT + LockedStackSelector - 3], LockedStackSize - 1
        mov [OffGDT + LockedStackSelector - 3 + 2], ax
        shr eax, 16
        mov [OffGDT + LockedStackSelector - 3 + 4], al
        mov [OffGDT + LockedStackSelector - 3 + 7], ah
        mov al, [ebp + NC_Mode]
        shl al, 6
        mov [OffGDT + LockedStackSelector - 3 + 6], al
        mov byte [OffGDT + LockedStackSelector - 3 + 5], 0F3h
.done:  popad
        ret

; EBX/EDX describe the interrupted stack. Return DS:EBX on the locked
; page; nested delivery continues below its actual current stack pointer.
NativePickLockedStack:
        push eax
        push edx
        mov eax, [ss:OffNativeCurrent]
        and dl, ~3
        cmp dx, Data1Selector - 1
        jne .source_ready
        push ecx
        mov edx, ebx
        and edx, ~127
        lea ecx, [eax + NC_Frames]
        cmp edx, ecx
        jb .not_shadow
        add ecx, 4096
        cmp edx, ecx
        jae .not_shadow
        cmp dword [ss:edx + NF_Type], 1
        jne .not_shadow
        mov ebx, [ss:edx + NF_SP]
        mov dx, [ss:edx + NF_SS]
        and dl, ~3
        jmp short .source_done
.not_shadow:
        mov edx, Data1Selector - 1
.source_done:
        pop ecx
.source_ready:
        cmp dx, LockedStackSelector - 3
        je .same
        cmp dx, Data3Selector - 3
        jne .fresh
        lea edx, [eax + NC_Stack]
        sub ebx, edx
        cmp ebx, LockedStackSize
        jbe .same
.fresh: mov ebx, LockedStackSize
.same:  cmp byte [ss:OffNativeMode], 0
        jne .wide
        movzx ebx, bx            ;word stacks have unspecified high ESP after IRET
.wide:  mov edx, LockedStackSelector
        mov ds, edx
        pop edx
        pop eax
        ret

; Match the common exception frame: saved EBP, vector, error, IRET.
; EBP distinguishes an IRQ (1) from an exception (0); the real EBP is
; already saved in that frame. Both paths restore all input registers.
NativeLockedEvent:
        pushad
        push ds
        push es
        push ss
        pop ds
        lea esi, [esp + 40 + 8] ;error plus five IRET dwords
        mov ebx, [esi + 16]
        mov edx, [esi + 20]
        call NativePickLockedStack
        push ds
        pop es
        push ss
        pop ds
        cmp byte [OffNativeMode], 0
        je NativeLockedException16
        sub ebx, 32
        jb NativeLockedOverflow
        lea edi, [ebx + 8]
        mov ecx, 6
        cld
        rep movsd
        mov dword [es:ebx], OffRetFromExcTrap3
        mov dword [es:ebx + 4], Trap3Selector
        lea esi, [esp + 40 + 8]
        movzx eax, byte [esi - 4]
        test ebp, ebp
        jz .exception
        mov edx, [eax*8 + OffClientIDT]
        mov eax, [eax*8 + OffClientIDT + 4]
        ; IRQ return is IRET to the existing exception-return gate.
        sub ebx, 4
        jb NativeLockedOverflow
        mov dword [es:ebx], OffNativeEventIretTrap3
        mov dword [es:ebx + 4], Trap3Selector
        mov ecx, [esi + 12]
        mov [es:ebx + 8], ecx
        jmp short NativeLockedDeliver
.exception:
        mov edx, [eax*8 + OffClientExc]
        mov eax, [eax*8 + OffClientExc + 4]
NativeLockedDeliver:
.deliver:
        mov [esi + 4], edx
        mov [esi + 8], eax
        and dword [esi + 12], ~4700h ;handlers enter with IF/TF/DF/NT clear
        mov [esi + 16], ebx
        mov dword [esi + 20], LockedStackSelector
        pop es
        pop ds
        popad
        pop ebp
        add esp, 8
        iretd
NativeLockedException16:
        sub ebx, 28
        jb NativeLockedOverflow
        mov word [es:ebx], OffRetFromExcTrap3
        mov word [es:ebx + 2], Trap3Selector
        lea edi, [ebx + 4]
        mov ecx, 6
.copy:  mov ax, [esi]
        mov [es:edi], ax
        add esi, 4
        add edi, 2
        loop .copy
        lea esi, [esp + 40 + 8]
        mov eax, [esi + 4]
        mov [es:ebx + 16], eax
        mov eax, [esi + 12]
        mov [es:ebx + 20], eax
        mov eax, [esi + 16]
        mov [es:ebx + 24], eax
        movzx eax, byte [esi - 4]
        test ebp, ebp
        jz .exception
        sub ebx, 2
        jb NativeLockedOverflow
        mov word [es:ebx], OffNativeEventIretTrap3
        mov word [es:ebx + 2], Trap3Selector
        mov cx, [esi + 12]
        mov [es:ebx + 4], cx
        mov edx, [eax*8 + OffClientIDT]
        mov eax, [eax*8 + OffClientIDT + 4]
        jmp NativeLockedDeliver
.exception:
        mov edx, [eax*8 + OffClientExc]
        mov eax, [eax*8 + OffClientExc + 4]
        jmp NativeLockedDeliver

; An IRET handler changes the flags used on return; its trampoline must
; transfer them to the canonical context before the common return gate.
        DPROC NativeEventIretTrapH
        push eax
        pushfd
        cli                     ;finish the copied return frame atomically
        pop eax
        cmp byte [ss:OffNativeMode], 0
        je .word
        bt dword [ss:esp + 24], 16 ;the trampoline CALL consumed synthetic RF
        jnc .flags
        bts eax, 16
.flags: mov [ss:esp + 24], eax
        jmp short .done
.word:  mov [ss:esp + 18], ax     ;a word IRET cannot change high EFLAGS
.done:  pop eax
        jmp RetFromExcTrapH

; The return gate copied the 12-byte word frame followed by three high-
; word carriers. Rebuild its 32-bit hardware IRET without changing GPRs.
NativeExceptionReturn16:
        pushad
        lea esi, [esp + 32 + 8]
        mov edi, [ss:esi + 12]
        mov di, [ss:esi + 2]
        mov ecx, [ss:esi + 16]
        mov cx, [ss:esi + 6]
        mov edx, [ss:esi + 20]
        mov dx, [ss:esi + 8]
        movzx eax, word [ss:esi + 4]
        movzx ebp, word [ss:esi + 10]
        mov [ss:esi + 4], edi
        mov [ss:esi + 8], eax
        mov [ss:esi + 12], ecx
        mov [ss:esi + 16], edx
        mov [ss:esi + 20], ebp
        popad
        jmp ExceptionReturnFrame

NativeLockedOverflow:
        ; The frame was not installed; terminate through the existing
        ; fatal-exception path with the original saved context intact.
        pop es
        pop ds
        popad
        jmp NativeFatalClient
; Bridge frames share the existing private 4 KB record area. Each entry
; preserves the prior PM bridge; nested callbacks/passups are LIFO.
NativeLockedBridge:
        push ds
        pushad
        call NativeAllocFrame
        jc .overflow
        mov edi, eax
        mov dword [edi + NF_Type], 2
        cmp byte [OffNativeMode], 0
        jne .alias_saved
        mov eax, [OffNativeCurrent]
        movzx eax, word [eax + NC_RMAlias]
        and eax, ~7
        mov eax, [eax + OffLDT + 2]
        mov [edi + 8], eax
.alias_saved:
        mov esi, [OffNativeRealBase]
        add esi, ROffPMStack
        mov ebx, [esi]
        mov edx, [esi + 4]
        mov [edi], ebx
        mov [edi + 4], edx
        call NativePickLockedStack
        push ds
        pop es
        push ss
        pop ds
        sub ebx, 6
        cmp byte [OffNativeMode], 0
        je .small_frame
        sub ebx, 6
.small_frame:
        cmp ebx, LockedStackSize
        ja .overflow
        mov [esi], ebx
        mov dword [esi + 4], LockedStackSelector
        mov [esp + PA_ESI], ebx
        popad
        pop ds
        ret
.overflow:
        popad
        pop ds
        jmp NativeFatalClient

; Return the previous PM bridge in EAX/EDX without touching ES:EDI (the
; callback register packet). Other non-result registers are preserved.
NativeLockedBridgeLeave:
        push ebx
        push ecx
        mov ebx, [ss:OffNativeRealBase]
        sub dword [ss:ebx + ROffNativeFrameNext], 128
        mov ebx, [ss:ebx + ROffNativeFrameNext]
        cmp byte [ss:OffNativeMode], 0
        jne .alias_restored
        mov ecx, [ss:OffNativeCurrent]
        movzx ecx, word [ss:ecx + NC_RMAlias]
        and ecx, ~7
        mov eax, [ss:ebx + 8]
        mov [ss:ecx + OffLDT + 2], eax
.alias_restored:
        mov eax, [ss:ebx]
        mov edx, [ss:ebx + 4]
        pop ecx
        pop ebx
        ret
; Reuse callback alias rebasing when restoring opaque mode state.
NativeRestoreAlias:
        push ds
        pushad
        call NativeCallbackSegments16
        popad
        pop ds
        ret

; Preserve a suspended parent's ring-1 IRQ continuation across DOS EXEC.
; The existing context alignment padding holds the same 512-byte stack.
; AL=1 saves before resetting ESP; AL=0 restores from a separate RM stack.
NativeKernelStack:
        pushad
        mov esi, [OffNativeCurrent]
        test esi, esi
        jz .done
        add esi, NC_Kernel1
        mov edi, OffKernelStack1Start
        test al, al
        jz .copy
        xchg esi, edi
.copy:  mov ecx, (OffKernelStack1End - OffKernelStack1Start) / 4
        cld
        rep movsd
.done:  popad
        ret

; EAX returns one 128-byte private frame; all other GPRs survive.
; Shared by bridge and public-call frames, included in 0305h state.
NativeAllocFrame:
        push ebx
        push edx
        mov ebx, [ss:OffNativeRealBase]
        mov eax, [ss:ebx + ROffNativeFrameNext]
        mov edx, [ss:OffNativeCurrent]
        add edx, NC_Frames + 4096
        cmp eax, edx
        jae .full
        add dword [ss:ebx + ROffNativeFrameNext], 128
        clc
        jmp short .done
.full:  stc
.done:  pop edx
        pop ebx
        ret
NF_SP equ 0
NF_SS equ 4
NF_Bytes equ 8
NF_Type equ 12
NF_Iret equ 80

; EBP points at a public call-gate frame. 16-bit software vectors have
; a word IRET frame; private calls and 32-bit vectors retain their frames.
NativePrepareFrame:
        pushfd
        pushad
        push ds
        push es
        cmp byte [ss:OffNativeMode], 0
        jne .done
        mov ax, [ebp + CFrCS]
        cmp ax, [ss:OffNativeThunkSel]
        jne .done
        push ss
        pop ds
        call NativeAllocFrame
        jc .overflow
        mov edi, eax
        les esi, [ebp + CFrESP]
        mov [edi + NF_SP], esi
        mov [edi + NF_SS], es
        mov dword [edi + NF_Bytes], 6
        mov dword [edi + NF_Type], 1
        movzx eax, word [es:esi]
        mov [edi + NF_Iret], eax
        movzx eax, word [es:esi + 2]
        mov [edi + NF_Iret + 4], eax
        pushfd
        pop eax
        mov ax, [es:esi + 4]
        mov [edi + NF_Iret + 8], eax
        add edi, NF_Iret
        mov [ebp + CFrESP], edi
        mov dword [ebp + CFrSS], Data1Selector
.done:  pop es
        pop ds
        popad
        popfd
        ret
.overflow:
        pop es
        pop ds
        popad
        popfd
        jmp NativeFatalClient

; A protected IRET frame is above this helper's return address. Replace
; a canonical user stack with its original SS:SP and release its record.
NativeFinishIret:
        pushad
        lea ebp, [esp + 36]
        test byte [ebp + IFrCS], 2
        jz .done
        cmp word [ebp + IFrSS], Data1Selector
        jne .done
        mov edi, [ebp + IFrESP]
        sub edi, NF_Iret + 12
        mov eax, [ss:OffNativeCurrent]
        add eax, NC_Frames
        cmp edi, eax
        jb .done
        add eax, 4096
        cmp edi, eax
        jae .done
        cmp dword [ss:edi + NF_Type], 1
        jne .done
        mov eax, [ss:edi + NF_SP]
        add eax, [ss:edi + NF_Bytes]
        mov [ebp + IFrESP], eax
        movzx eax, word [ss:edi + NF_SS]
        mov [ebp + IFrSS], eax
        mov eax, [ss:OffNativeRealBase]
        mov [ss:eax + ROffNativeFrameNext], edi
.done:  popad
        ret

; ESI is canonical IRET+12 for the parameter source of 030xh.
NativeRMParameters:
        push eax
        mov eax, ds
        cmp ax, Data1Selector
        jne .done
        lea eax, [esi - NF_Iret - 12]
        cmp dword [ss:eax + NF_Type], 1
        jne .done
        mov ds, [ss:eax + NF_SS]
        mov esi, [ss:eax + NF_SP]
        add esi, [ss:eax + NF_Bytes]
.done:  pop eax
        ret

; Decode the ignored offset in a 16-bit thunk before common dispatch.
; AH=4Ch termination retains its original frame; 1686h returns directly.
NativeDefaultFrame:
        push ebp
        lea ebp, [esp + 8]
        push eax
        cmp dword [ss:OffNativeCurrent], 0
        je .ordinary
        mov ax, [ebp + CFrCS]
        cmp ax, [ss:OffNativeThunkSel]
        jne .ordinary
        mov eax, [ebp + CFrEIP]
        dec eax                 ;overlapped CALL32 returns at vector*4+8
        mov [ebp + CFrEIP], eax
        pop eax
        cmp ah, 4Ch
        jne .prepare
        cmp dword [ebp + CFrEIP], 21h * 4 + 7
        je .done
.prepare:
        call NativePrepareFrame
        cmp ax, 1686h
        jne .done
        cmp dword [ebp + CFrEIP], 2Fh * 4 + 7
        jne .done
        xor ax, ax
        pop ebp
        stc
        ret
.ordinary:
        pop eax
.done:  pop ebp
        clc
        ret

NativeDefaultReturn:
        push ds
        push esi
        push ebp
        lds esi, [esp + 12 + CFrESP]
        add esi, 12
        push ds
        push esi
        push dword [esi - 4]
        push dword [esi - 8]
        push dword [esi - 12]
        mov ebp, [esp + 20]
        mov esi, [esp + 24]
        mov ds, [esp + 28]
        call NativeFinishIret
        iretd

NativeCreateThunks:
        pushad
        cmp word [OffNativeThunkSel], 0
        jne .done
        push 1
        call AllocSelectors
        jc .failed
        mov [OffNativeThunkSel], ax
        mov eax, OffNativeInt16
        mov [ebp + 2], eax
        mov byte [ebp + 5], 0FAh
        mov byte [ebp + 6], 0
        mov word [ebp], NativeInt16End - NativeInt16 - 1
        mov edi, OffClientExc + 16*8
        push 16
        pop ecx
        mov ax, [OffNativeThunkSel]
.extra: mov dword [edi], NativeFatalStub - NativeInt16
        mov [edi + 4], ax
        add edi, 8
        loop .extra
.done:  popad
        clc
        ret
.failed:
        popad
        stc
        ret

NativeInstall16:
        pushad
        cmp byte [OffNativeMode], 0FFh
        jne .done
        mov byte [OffGDT + FirstGateSelector + 8 + 4], 7
        xor ebx, ebx
        xor edx, edx
.loop:  mov [ebx*8 + OffClientIDT], edx
        mov ax, [OffNativeThunkSel]
        mov [ebx*8 + OffClientIDT + 4], ax
        test byte [ebx*8 + OffIDT + 2], 2
        jz .kernel
        mov [ebx*8 + OffIDT], dx
        mov [ebx*8 + OffIDT + 2], ax
        mov word [ebx*8 + OffIDT + 6], 0
        mov byte [ebx*8 + OffIDT + 5], 0E0h + SS_GATE_TRAP2
.kernel:
        add edx, 4
        inc ebx
        cmp ebx, 256
        jb .loop
.done:  popad
        clc
        ret

NativeCallbackSegments16:
        cmp byte [ss:OffNativeMode], 0
        jne .done
        push eax
        push ecx
        push edx
        mov eax, [ss:OffNativeCurrent]
        movzx ecx, word [ss:eax + NC_RMAlias]
        mov eax, [ss:OffNativeRealBase]
        mov esi, [ss:eax + ROffRMStack]
        mov edx, esi
        shr edx, 16
        shl edx, 4
        mov eax, ecx
        and eax, ~7
        mov [ss:eax + OffLDT + 2], edx
        mov byte [ss:eax + OffLDT + 5], 0F3h
        mov ds, ecx
        movzx esi, si
        pop edx
        pop ecx
        pop eax
.done:  ret
; DOS blocks own an immutable contiguous descriptor array. Head AVL is
; host metadata; LDTBlock prevents clients from modifying any array member.
NativeDOSAlloc:
        pushad
        test bx, bx
        jz .no_selectors
        mov ah, 48h
        call Dos1Call
        jc NativeDOSError
        mov [esp + PA_EAX], ax
        movzx edi, ax
        movzx ecx, bx
        dec ecx
        shr ecx, 12
        inc ecx
        push ecx
        call AllocSelectors
        jc .release_dos
        mov [esp + PA_EDX], ax
        mov eax, edi
        call NativeDOSBuild
        popad
        ret
.release_dos:
        mov esi, edi
        mov ah, 49h
        call Dos1Call
        jc NativeDOSError
.no_selectors:
        call NativeDOSMaximum
NativeDOSError:
        mov [esp + PA_EAX], ax
        mov [esp + PA_EBX], bx
NativeDOSErrorSaved:
        popad
        jmp DPMIError1N
NativeDOSInvalid:
        mov ax, 9
        jmp NativeDOSError

NativeDOSFree:
        pushad
        call NativeDOSCheck
        jc NativeDOSInvalid
        lea ecx, [esp + 32 + DPMIFrame_SF]
        call NativeDOSActiveStack
        jc NativeDOSInvalid
        push esi
        mov esi, eax
        mov ah, 49h
        call Dos1Call
        pop esi
        jc NativeDOSError
        lea ecx, [esp + 32 + DPMIFrame_DS]
        call NativeDOSRelease
        popad
        ret

; Return RM segment EAX, head index ESI, count EDI and size ECX.
NativeDOSCheck:
        movzx esi, dx
        test si, 4
        jz .bad
        shr esi, 3
        bt [OffLDTFree], esi
        jnc .bad
        bt [OffLDTBlock], esi
        jnc .bad
        lea ebp, [esi*8 + OffLDT]
        test byte [ebp + 6], 10h
        jz .bad
        mov eax, [ebp + 2]
        and eax, 0FFFFFFh
        shr eax, 4
        mov ecx, eax
        shl ecx, 4
        movzx ecx, word [ecx - 13] ;DOS MCB stores the complete block size
        lea edi, [ecx - 1]
        shr edi, 12
        inc edi
        clc
        ret
.bad:   stc
        ret

; ECX addresses CFr, ESI/EDI the interval. Private exit cleanup may
; release the exited client's stack; public calls must keep SS valid.
NativeDOSActiveStack:
        pushad
        test byte [ecx + CFrCS], 2
        jz .safe
        movzx edx, word [ecx + CFrSS]
        cmp dx, Data1Selector
        jne .actual
        mov eax, [ecx + CFrESP]
        movzx edx, word [eax - NF_Iret + NF_SS]
.actual:
        test dl, 4
        jz .safe
        shr edx, 3
        cmp edx, esi
        jb .safe
        add esi, edi
        cmp edx, esi
        jmp short .done
.safe:  clc
.done:  popad
        ret

; Build the full-size head and successive 64 KB/final partial aliases.
NativeDOSBuild:
        pushad
        mov ecx, esi
        mov esi, NC_SelBits
        call NativeFindOwner
        test ebp, ebp
        jnz .owned
        mov ebp, [OffNativeCurrent]
.owned: mov edx, ebp
        mov esi, [esp + PA_ESI]
        movzx ecx, word [esp + PA_EBX]
        mov eax, [esp + PA_EAX]
        lea ebp, [esi*8 + OffLDT]
        mov ebx, ecx
        call SetupDOSSelector   ;32-bit host: full head for either client ABI
        or byte [ebp + 6], 10h
.mark:  bts [OffLDTBlock], esi
        bts [edx + NC_SelBits], esi
        sub ecx, 1000h
        jbe .done
        inc esi
        add ebp, 8
        add ax, 1000h
        mov ebx, ecx
        cmp ebx, 1000h
        jbe .limit
        mov ebx, 1000h
.limit: call SetupDOSSelector
        jmp .mark
.done:  popad
        ret

; Release ESI/EDI interval; ECX optionally addresses saved DS/ES/FS/GS.
; The common cache helper is also used by ordinary function 0001h.
NativeDOSRelease:
        pushad
        mov edx, esi
        lea edi, [esi + edi]
.loop:  cmp edx, edi
        jae .cache
        mov ecx, edx
        mov esi, NC_SelBits
        call NativeFindOwner
        test ebp, ebp
        jz .unowned
        btr [ebp + NC_SelBits], edx
.unowned:
        btr [OffLDTFree], edx
        btr [OffLDTBlock], edx
        mov byte [edx*8 + OffLDT + 5], 0
        inc edx
        jmp .loop
.cache: mov ebp, edi
        mov edi, [esp + PA_ECX]
        test edi, edi
        jz .done
        mov esi, [esp + PA_ESI]
        call ClearSelectorCacheRange
.done:  popad
        ret

; Maximum DOS block is bounded by a contiguous selector run as well.
NativeDOSMaximum:
        mov bx, 0FFFFh
        mov ah, 48h
        call Dos1Call             ;a conventional block cannot reach 1 MB
        xor edx, edx
        xor ecx, ecx
        mov esi, 16
.scan:  bt [OffLDTFree], esi
        jc .allocated
        inc ecx
        cmp ecx, edx
        jbe .next
        mov edx, ecx
        cmp edx, 16
        jae .maximum
        jmp short .next
.allocated:
        xor ecx, ecx
.next:  inc esi
        cmp esi, 2000h
        jb .scan
        shl edx, 12
        cmp bx, dx
        jbe .maximum
        mov bx, dx
.maximum:
        mov ax, 8
        ret

; Reserve exactly the adjacent range, sharing the allocator's commit/init.
NativeAllocSelectorsAt:
        movzx ebp, word [esp + 4]
.check: cmp esi, 2000h
        jae .bad
        bt [OffLDTFree], esi
        jc .bad
        inc esi
        dec ebp
        jnz .check
        jmp SelectorsFound@10
.bad:   stc
        ret 4

NativeDOSResize:
        pushad
        call NativeDOSCheck
        jc NativeDOSInvalid
        test bx, bx
        jz .limited
        movzx ebp, bx
        dec ebp
        shr ebp, 12
        inc ebp
        cmp ebp, edi
        jae .reserve
        pushad
        add esi, ebp
        sub edi, ebp
        lea ecx, [esp + 64 + DPMIFrame_SF]
        call NativeDOSActiveStack
        popad
        jc NativeDOSInvalid
.reserve:
        xor edx, edx
        cmp ebp, edi
        jbe .reserved
        mov edx, ebp
        sub edx, edi
        push eax
        push ecx
        push esi
        push edi
        add esi, edi
        push edx
        call NativeAllocSelectorsAt
        pop edi
        pop esi
        pop ecx
        pop eax
        jc .limited
.reserved:
        push edx                 ;new selectors reserved, not yet published
        pushad
        mov esi, eax
        mov ah, 4Ah
        call Dos1Call
        jnc .resized
        mov [esp + 36 + PA_EAX], ax
        mov [esp + 36 + PA_EBX], bx
        popad
        push esi
        mov esi, eax
        mov bx, cx               ;DOS may partially grow even on failure
        mov ah, 4Ah
        call Dos1Call
        pop esi
        add esi, edi
        pop edi
        xor ecx, ecx
        call NativeDOSRelease
        jmp NativeDOSErrorSaved
.resized:
        popad
        pop edx
        cmp ebp, edi
        jae .build
        pushad
        add esi, ebp
        sub edi, ebp
        lea ecx, [esp + 64 + DPMIFrame_DS]
        call NativeDOSRelease
        popad
.build: call NativeDOSBuild
        popad
        ret
.limited:
        call NativeDOSResizeMaximum
        jmp NativeDOSError

; Probe the selector-limited growth ceiling through DOS, then restore the
; original size. The visible block and descriptors are unchanged on error.
NativeDOSResizeMaximum:
        pushad
        lea ebp, [esi + edi]
.scan:  cmp edi, 16
        jae .probe
        cmp ebp, 2000h
        jae .probe
        bt [OffLDTFree], ebp
        jc .probe
        inc ebp
        inc edi
        jmp .scan
.probe: mov ebx, edi
        shl ebx, 12
        cmp ebx, 0FFFFh
        jbe .size
        mov ebx, 0FFFFh
.size:  mov esi, eax
        mov ah, 4Ah
        call Dos1Call
        push ebx
        mov esi, [esp + 4 + PA_EAX]
        mov bx, [esp + 4 + PA_ECX]
        mov ah, 4Ah
        call Dos1Call
        pop ebx
        mov [esp + PA_EBX], bx
        popad
        mov ax, 8
        ret
; Raw frame: saved EBP, vector, error, EIP/CS/flags/ESP/SS. Default
; nonfatal exceptions reflect through the same real-mode interrupt path.
NativeFatalClient:
        cli
        mov ax, cs
        test al, 3
        jnz .ring1
        push Data1Selector
        push OffKernelStack1End
        push Code1Selector
        push OffNativeFatalExit
        retf
.ring1:
        mov esp, OffKernelStack1End
        DPROC NativeFatalExit
        mov al, 0FFh
        jmp NativeClientExit

NativeDefaultExceptionRaw:
        pushad
        push ds
        push ss
        pop ds
        mov ecx, [esp + 40]
        cmp ecx, 8
        jae .fatal_default
        cmp cl, 6
        jne .interrupt
.fatal_default:
        ; 0.9 compatibility corrections: a hooked PM interrupt receives
        ; an otherwise unhandled CPU exception before the default policy.
        lea edx, [ecx*4 + OffDefIntTrap3]
        mov eax, Trap3Selector
        cmp byte [OffNativeMode], 0
        jne .compare
        lea edx, [ecx*4]
        mov ax, [OffNativeThunkSel]
.compare:
        cmp [ecx*8 + OffClientIDT + 4], ax
        jne .interrupt
        cmp [ecx*8 + OffClientIDT], edx
        je NativeFatalClient
.interrupt:
        pop ds
        popad
        mov ebp, 1               ;the PM interrupt hook returns with IRET
        jmp NativeLockedEvent    ;CPU defaults still require the locked stack

; A word client chained to its saved default handler. The seven copied
; dwords contain the documented word frame plus high-value carriers.
NativeDefaultWordException:
        pushad
        lea esi, [esp + 32]
        mov edi, [ss:esi]
        movzx edi, byte [ss:edi + OffFirstTrap3 - 4]
        and edi, 0Fh
        mov eax, [ss:esi + 24]
        mov ax, [ss:esi + 14]
        mov edx, [ss:esi + 28]
        mov dx, [ss:esi + 18]
        mov ecx, [ss:esi + 32]
        mov cx, [ss:esi + 20]
        movzx ebp, word [ss:esi + 22]
        movzx ebx, word [ss:esi + 16]
        mov [ss:esi + 28], ebp
        mov [ss:esi + 24], ecx
        mov [ss:esi + 20], edx
        mov [ss:esi + 16], ebx
        mov [ss:esi + 12], eax
        mov [ss:esi + 4], edi
        and dword [ss:esi + 8], 0
        mov ebp, [esp + PA_EBP]
        mov [ss:esi], ebp
        popad
        jmp NativeDefaultExceptionRaw

; Leave ring 0 on the normal kernel-1 stack before any cleanup can admit
; IRQs. An unhandled fatal child must not leave its resources in its parent.
; DPL-0 CPU gates make software INT distinguishable through #GP's IDT
; error code. INT3/INTO remain CPU traps; CD nn dispatches ClientIDT.
; Common CPU error frame, excluding the helper return and saved registers.
NSG_Vector equ 0
NSG_Error equ 4
NSG_IP equ 8 + IFrEIP
NSG_CS equ 8 + IFrCS
NSG_Flags equ 8 + IFrFlags
NSG_SP equ 8 + IFrESP
NSG_SS equ 8 + IFrSS
NSG_Saved equ 8 * 4 + 2 * 4 ;PUSHAD, DS, ES
%if NSG_SS != 24 || NSG_Saved != 40
%error Native software-interrupt frame layout changed
%endif
NativeSoftwareGP:
        pushad
        push ds
        push es
        push ss
        pop ds
        lea esi, [esp + NSG_Saved + 4]
        mov al, [esi + NSG_CS]
        and al, [esi + NSG_Error]
        test al, 2
        jz NativeSoftwareUnhandled
        mov ecx, [esi + NSG_Error]
        shr ecx, 3
        cmp ecx, 32
        jae NativeSoftwareUnhandled
        mov edi, [esi + NSG_IP]
        lsl eax, word [esi + NSG_CS]
        jnz NativeSoftwareUnhandled
        mov edx, eax
        mov es, [esi + NSG_CS]
        xor ebx, ebx
        mov bl, 15              ;architectural instruction-length limit
.prefix:
        dec ebx
        js NativeSoftwareUnhandled
        cmp edi, edx
        ja NativeSoftwareUnhandled
        mov al, [es:edi]
        inc edi
        jz NativeSoftwareUnhandled ;do not wrap a 32-bit fetch offset
        cmp al, 64h
        jb .segment
        cmp al, 67h
        jbe .prefix
        cmp al, 0F2h
        jb .opcode
        cmp al, 0F3h
        jbe .prefix
.segment:
        mov ah, al
        and ah, 0E7h           ;26/2E/36/3E segment overrides
        cmp ah, 26h
        je .prefix
.opcode:
        cmp al, 0CCh
        jne .into
        cmp ecx, 3
        je NativeSoftwareCPUTrap
.into:  cmp al, 0CEh
        jne .int
        cmp ecx, 4
        je NativeSoftwareCPUTrap
.int:   cmp al, 0CDh
        jne NativeSoftwareUnhandled
        test ebx, ebx
        jz NativeSoftwareUnhandled
        cmp edi, edx
        ja NativeSoftwareUnhandled
        cmp [es:edi], cl
        jne NativeSoftwareUnhandled
        inc edi
        mov ebx, edi
        call NativeIntStack
        jc NativeSoftwareFault
        mov [esi + NSG_IP], ebx
        and byte [esi + NSG_Flags+2], ~1 ;discard #GP's synthetic RF
        jmp short NativeDispatchInt
NativeSoftwareUnhandled:
        pop es
        pop ds
        popad
        ret
NativeDispatchInt:
        cmp byte [OffNativeMode], 0
        jne .frame32
        mov ax, [esi + NSG_IP]
        mov [es:edi], ax
        mov ax, [esi + NSG_CS]
        mov [es:edi + 2], ax
        mov ax, [esi + NSG_Flags]
        mov [es:edi + 4], ax
        jmp short .dispatch
.frame32:
        mov eax, [esi + NSG_IP]
        mov [es:edi], eax
        mov eax, [esi + NSG_CS]
        mov [es:edi + 4], eax
        mov eax, [esi + NSG_Flags]
        mov [es:edi + 8], eax
.dispatch:
        mov [esi + NSG_SP], edi
        mov eax, [ecx*8 + OffClientIDT]
        mov [esi + NSG_IP], eax
        movzx eax, word [ecx*8 + OffClientIDT + 4]
        mov [esi + NSG_CS], eax
        and byte [esi + NSG_Flags+1], ~41h
        cmp ecx, 8
        jae .return
        and byte [esi + NSG_Flags+1], ~2
.return:
        pop es
        pop ds
        popad
        add esp, 12              ;helper return, vector and #GP error
        iretd
NativeSoftwareCPUTrap:
        and byte [esi + NSG_Flags+2], ~1
        mov [esi + NSG_IP], edi
        mov [esi + NSG_Vector], ecx
        and dword [esi + NSG_Error], 0
NativeSoftwareFault:
        pop es
        pop ds
        popad
        add esp, 4
        jmp ExceptionWithCodeH@84

; Preflight the complete frame at client privilege before any CPL0 store.
; Return ES:EDI ready for stores; on failure rewrite the original CPU
; frame for #SS/#PF without advancing IP. Preserve the mapping window.
NativeIntStack:
        pushad
        push dword [OffPage2 + ((OffPageTableWin-KernelBase) >> 10)]
        mov edx, [esi + NSG_SS]
        lar eax, edx
        jnz .stack
        mov ebx, eax
        and bh, 0FAh            ;present DPL3 writable data, either direction
        cmp bh, 0F2h
        jne .stack
        mov es, edx
        bt eax, 22
        sbb edx, edx
        or edx, 0FFFFh
        mov edi, [esi + NSG_SP]
        and edi, edx
        mov ecx, 6
        cmp byte [OffNativeMode], 0
        je .width
        add ecx, ecx
.width: sub edi, ecx
        and edi, edx
        lea ebp, [ecx - 1]
        add ebp, edi
        jc .stack
        cmp ebp, edx
        ja .stack
        lsl edx, word [esi + NSG_SS]
        jnz .stack
        test ah, 4              ;expand-down data
        jz .up
        cmp edi, edx
        jbe .stack
        jmp short .base
.up:    cmp ebp, edx
        jbe .base
.stack: xor edx, edx
        push byte 12
        pop eax
        jmp .fault
.base:  mov [esp + 4 + PA_EDI], edi
        movzx ebx, word [esi + NSG_SS]
        mov edx, OffGDT
        test bl, 4
        jz .table
        mov edx, OffLDT
.table: and ebx, ~7
        add edx, ebx
        mov ebx, [edx + 2]
        rol ebx, 8
        mov bl, [edx + 7]
        ror ebx, 8
        add edi, ebx
        add ebp, ebx
        and ebp, ~0FFFh
.page:  mov eax, edi
        shr eax, 22
        mov edx, [OffPageDir + eax*4]
        mov al, dl
        and al, 7
        cmp al, 7
        jne .page_fault
        mov [OffPage2 + ((OffPageTableWin-KernelBase) >> 10)], edx
        mov eax, cr3
        mov cr3, eax
        mov eax, edi
        shr eax, 12
        and eax, 3FFh
        mov edx, [OffPageTableWin + eax*4]
        mov al, dl
        and al, 7
        cmp al, 7
        jne .page_fault
        mov eax, edi
        and eax, ~0FFFh
        cmp eax, ebp
        je .done                ;CMP leaves CF clear
        or edi, 0FFFh
        inc edi
        jmp .page
.page_fault:
        and edx, 1              ;P plus user/write access bits
        or edx, 6
        mov cr2, edi
        push byte 14
        pop eax
.fault: mov [esi + NSG_Vector], eax
        mov [esi + NSG_Error], edx
        stc
.done:  pop dword [OffPage2 + ((OffPageTableWin-KernelBase) >> 10)]
        mov eax, cr3
        mov cr3, eax
        popad
        ret

        DPROC NativeCPU2
        push byte 2
        jmp ExceptionWOCodeHL
        DPROC NativeCPU16
; Short branches share the two full-distance entries; all 16 IDT slots
; retain fixed four-byte strides without duplicating near jumps.
%assign _cpuexc 16
%rep 16
        push byte _cpuexc
%if _cpuexc = 17
        jmp short NativeCPUWithCode
%else
        jmp short NativeCPUWithoutCode
%endif
%assign _cpuexc _cpuexc + 1
%endrep
NativeCPUWithCode:
        jmp ExceptionWithCodeH@84
NativeCPUWithoutCode:
        jmp ExceptionWOCodeHL
        DPROC NativeInt16
        bits 16
; Like the original Trap3 table, neighboring instructions overlap their
; ignored offsets. CALL32's selector comes from the following four-byte
; slot. No instruction ever returns here except the separate state stub.
%assign _nativeint 0
%rep 257
        db 66h, CallFarCode
%if _nativeint = 32h
        dw DPMIEntryGateSelector
%else
        dw FirstGateSelector + 3
%endif
%assign _nativeint _nativeint + 1
%endrep
NativeState16:
        db 66h, CallFarCode
        dw 0
NativeFatalStub:
        db 66h, CallFarCode
        dw FirstGateSelector + (NTraps3c - 1) * 8 + 3
        db RetfCode, 0            ;state call returns at +8; fatal offset ignores it
        dw FirstGateSelector + 8 + 3
NativeInt16End:
        bits 32
        ESEG Text
