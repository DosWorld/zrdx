; Native DPMI frontend. Explicit profile; not in the bound-image
; build.
; Budget and shared-kernel decision: docs/D09WORK.MD.

; Entry snapshot preserves live transferred registers and the RM
; frame.
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
; private PTEs must be restored before recycling
NC_Protected equ 108
NC_RMAlias equ 112
NC_InDos equ 116
NC_SelBits equ 128
NC_MemBits equ NC_SelBits + 1024
NC_MemBytes equ ((OffClientPages-OffMCBVector) \
        / MCBStruct_size + 7) / 8
NC_Stack equ (NC_MemBits + NC_MemBytes + 4095) & ~4095
NC_Kernel1 equ NC_Stack - (OffKernelStack1End \
        - OffKernelStack1Start)
%if NC_Kernel1 < NC_MemBits + NC_MemBytes
%error native context padding too small
%endif
NC_Frames equ NC_Stack + LockedStackSize
NC_Vectors equ NC_Frames + 4096 ;parent's PM vectors
NC_Size equ NC_Vectors + 8192

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

; Root command tail holds an 8.3 DOS program path and its
; arguments.
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
%ifdef SWAP
        call SwapRDelete
%endif
        mov al, [cs:ROffNativeExitCode]
        mov ah, 4Ch
        int 21h
        ESEG Text16

        SEGM Text
; Both protected stacks share one existing kernel PTE. A layout
; change
; must not silently leave part of either stack user accessible.
%if (OffKernelStack1Start & ~0FFFh) != ((OffKernelStack - 1) \
        & ~0FFFh)
%error native kernel stacks cross a page
%endif
NativeHostStart:
        cli
        mov edi, OffIDT
        push 16
        pop ecx
.low:   and byte [edi + 5], ~60h
        add edi, 8
        loop .low
        mov dword [OffIDT + 2*8], (Code0Selector << 16) \
                | (OffNativeCPU2 & 0FFFFh)
        mov dword [OffIDT + 2*8 + 4], (KernelBase & 0FFFF0000h) \
                | 8E00h
        mov eax, (Code0Selector << 16) | (OffNativeCPU16 \
                & 0FFFFh)
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
        ; Bootstrap calls can still use the original user anchor.
        ; Then
        ; reserve one root IRQ page before making kernel stacks
        ; private.
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
        and byte [OffPage2 + (((OffKernelStack - 1) & ~0FFFh) \
                - KernelBase) / 1024], ~4
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
%ifdef SWAP
        call NativeUnpage
%endif
        mov eax, [OffRootMCB + MCB_StartOffset]
        mov [ebp + NC_Physical], eax
        mov esi, [OffNativeCurrent]
        mov [ebp + NC_Parent], esi
        mov [OffNativeCurrent], ebp
%ifdef SWAP
        ;the client runs inside its own EXEC: its InDOS baseline
        ;(SwapSlots + 1, see SwapDisk) is the current value
        mov edx, [OffNativeRealBase]
        mov al, [edx + ROffSwapSlots + 1]
        mov [ebp + NC_InDos], al
        mov esi, [edx + ROffSwapInDos]
        test esi, esi
        jz .nobase
        mov al, [esi]
        mov [edx + ROffSwapSlots + 1], al
.nobase:
%endif
        mov esi, OffNativeBoot
        mov edi, ebp
        mov ecx, NS_Size / 4
        rep movsd
        clc
        call NativeVectors
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

; Invoke the same dispatcher from ring 1. The synthetic gate's
; old SS
; identifies an internal call independently of native client
; bitness.
NativeCall31:
        push dword [OffTSS + TSS_ESP1]
        pushfd                     ;caller flags remain private
        cli
        push dword [esp]
        ; return through the unwinder with IF=0
        and byte [esp + 1], ~2
        push cs
        push OffNativeCallReturn
        push Data3Selector         ;separate public IRQ anchor
        push dword 0
        push ss
        push esp
        ; private synthetic IRET, not the IRQ stack
        add dword [esp], 12
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
        ; Same-ring IRET leaves two stack-switch words
        ; unconsumed. Skip
        ; the saves, fake gate, IRQ anchor and private synthetic
        ; IRET.
        lea esp, [esp + 8 + DPMIFrame_SF - 4 + CFrame_size + 8 \
                + 12]
        rcr dword [esp], 1
        ; merge service CF into original flags
        rol dword [esp], 1
        push dword [esp + 4]
        pop dword [OffTSS + TSS_ESP1]
        popfd
        lea esp, [esp + 4]
        ret

; Initial selector setup uses the public descriptor operations
; and the
; existing contiguous allocator; only the native ownership bit is
; new.
NativeMakeSel:
        pushad
        push 1
        call AllocSelectors
        jc .failed
        ; publish AX, recover the original real segment
        xchg eax, [esp + PA_EAX]
        mov edi, [OffNativeCurrent]
        bts [edi + NC_SelBits], esi
        ; Trusted entry-only inputs: real segment, limit
        ; FFh/FFFFh and
        ; F2h/FAh rights with optional B. No public validation is
        ; bypassed.
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
        ; original FAR return, before overwriting
        mov edx, [eax]
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
        test eax, eax
        jz .novectors
        stc
        call NativeVectors
.novectors:
        mov [OffNativeCurrent], eax
%ifdef SWAP
        mov ecx, [OffNativeRealBase]
        mov al, [ebp + NC_InDos]
        mov [ecx + ROffSwapSlots + 1], al
%endif
        call NativeSetLockedStack
        mov esi, edx
        shr esi, 16
        mov edi, edx
        mov eax, 0502h
        call NativeCall31
        ret

; The PM vectors (client tables and IDT gates) are per client.
; In: CF=0 save them in the context, CF=1 restore from it.
NativeVectors:
        pushad
        cld
        sbb eax, eax
        lea edx, [ebp + NC_Vectors]
        mov esi, OffClientIDT
        mov ecx, 512
        call .move
        mov esi, OffClientExc
        mov ecx, 64
        call .move
        mov esi, OffIDT
        mov ecx, 512
        call .move
        popad
        ret
.move:  mov edi, edx
        lea edx, [edx + ecx*4]
        test eax, eax
        jz .copy
        xchg esi, edi
.copy:  rep movsd
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
        ; Cleanup may service an IRQ through a private anchor.
        ; Restore the
        ; parent bridge only after the final private free has
        ; returned.
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

; The public ABI is selected by entry, not by CS.D. Internal
; calls from
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

; Shared 0.9 tables permit an ancestor's resource to be used or
; freed.
; ECX is a bitmap index, ESI its context offset; EBP returns the
; owner.
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
        ; 090xh handlers leave DS on the client stack
        pop ds
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

; Called with PUSHAD and saved flags beneath its return. Account
; only
; successful public calls; internal setup/cleanup must not
; acquire owners.
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
        lea eax, [edx*4 + ROffFirstSwitchCode \
                + MaxSystemSwitchCode]
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

%ifdef SWAP
; The context pages are kernel data touched at PL0: they are
; never evicted (a PL0 fault cannot be served). Called as soon
; as the block is allocated, before anything else can evict it.
NativeUnpage:
        pushad
        mov eax, ebp
        shr eax, 12
        push NC_Size / 4096
        pop ecx
.page:  mov edx, eax
        shr edx, 10
        mov edx, [OffPageDir + edx*4]
        mov [OffPage2 + ((OffPageTableWin-KernelBase) >> 10)], \
                edx
        InvalidateTLB
        mov esi, eax
        and esi, 3FFh
        and byte [OffPageTableWin + esi*4 + 1], ~(SwapPageable \
                >> 8)
        inc eax
        loop .page
        InvalidateTLB
        popad
        ret
%endif

; Metadata is supervisor-only. The existing one-page locked
; client stack
; remains user-accessible. Pair the toggle on publication and
; final free;
; the physical pool retains PTE flags and must receive user pages
; back.
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
        mov [OffPage2 + ((OffPageTableWin-KernelBase) >> 10)], \
                edx
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

; The child owns the downward mapping interval below its entry
; snapshot.
; Preserve the parent's partial directory and free only new table
; pages.
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
; One existing 4 KB client stack, rebased on entry/return to a
; parent.
NativeSetLockedStack:
        pushad
        mov ebp, [OffNativeCurrent]
        mov byte [OffGDT + LockedStackSelector - 3 + 5], 0
        test ebp, ebp
        jz .done
        lea eax, [ebp + NC_Stack]
        mov word [OffGDT + LockedStackSelector - 3], \
                LockedStackSize - 1
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

%include "nathost2.asm"
