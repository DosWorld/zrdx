;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; Second half of nathost.asm (split by size, task 150):
; it is included at the end of nathost.asm.

; EBX/EDX describe the interrupted stack. Return DS:EBX on the
; locked
; page; nested delivery continues below its actual current stack
; pointer.
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
        ; word stacks have unspecified high ESP after IRET
        movzx ebx, bx
.wide:  mov edx, LockedStackSelector
        mov ds, edx
        pop edx
        pop eax
        ret

; Match the common exception frame: saved EBP, vector, error,
; IRET.
; EBP distinguishes an IRQ (1) from an exception (0); the real
; EBP is
; already saved in that frame. Both paths restore all input
; registers.
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
        ; IRQ return is IRET to the existing exception-return
        ; gate.
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
        ; handlers enter with IF/TF/DF/NT clear
        and dword [esi + 12], ~4700h
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

; An IRET handler changes the flags used on return; its
; trampoline must
; transfer them to the canonical context before the common return
; gate.
        DPROC NativeEventIretTrapH
        push eax
        pushfd
        ; finish the copied return frame atomically
        cli
        pop eax
        cmp byte [ss:OffNativeMode], 0
        je .word
        ; the trampoline CALL consumed synthetic RF
        bt dword [ss:esp + 24], 16
        jnc .flags
        bts eax, 16
.flags: mov [ss:esp + 24], eax
        jmp short .done
; a word IRET cannot change high EFLAGS
.word:  mov [ss:esp + 18], ax
.done:  pop eax
        jmp RetFromExcTrapH

; The return gate copied the 12-byte word frame followed by three
; high-
; word carriers. Rebuild its 32-bit hardware IRET without
; changing GPRs.
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
        ; The frame was not installed; terminate through the
        ; existing
        ; fatal-exception path with the original saved context
        ; intact.
        pop es
        pop ds
        popad
        jmp NativeFatalClient
; Bridge frames share the existing private 4 KB record area. Each
; entry
; preserves the prior PM bridge; nested callbacks/passups are
; LIFO.
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

; Return the previous PM bridge in EAX/EDX without touching
; ES:EDI (the
; callback register packet). Other non-result registers are
; preserved.
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

; Preserve a suspended parent's ring-1 IRQ continuation across
; DOS EXEC.
; The existing context alignment padding holds the same 512-byte
; stack.
; AL=1 saves before resetting ESP; AL=0 restores from a separate
; RM stack.
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
; Shared by bridge and public-call frames, included in 0305h
; state.
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

; EBP points at a public call-gate frame. 16-bit software vectors
; have
; a word IRET frame; private calls and 32-bit vectors retain
; their frames.
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

; A protected IRET frame is above this helper's return address.
; Replace
; a canonical user stack with its original SS:SP and release its
; record.
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

; Decode the ignored offset in a 16-bit thunk before common
; dispatch.
; AH=4Ch termination retains its original frame; 1686h returns
; directly.
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
        ; overlapped CALL32 returns at vector*4+8
        dec eax
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
; DOS blocks own an immutable contiguous descriptor array. Head
; AVL is
; host metadata; LDTBlock prevents clients from modifying any
; array member.
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
        ; DOS MCB stores the complete block size
        movzx ecx, word [ecx - 13]
        lea edi, [ecx - 1]
        shr edi, 12
        inc edi
        clc
        ret
.bad:   stc
        ret

; ECX addresses CFr, ESI/EDI the interval. Private exit cleanup
; may
; release the exited client's stack; public calls must keep SS
; valid.
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

; Build the full-size head and successive 64 KB/final partial
; aliases.
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
        ; 32-bit host: full head for either client ABI
        call SetupDOSSelector
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

; Release ESI/EDI interval; ECX optionally addresses saved
; DS/ES/FS/GS.
; The common cache helper is also used by ordinary function
; 0001h.
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

; Maximum DOS block is bounded by a contiguous selector run as
; well.
NativeDOSMaximum:
        mov bx, 0FFFFh
        mov ah, 48h
        ; a conventional block cannot reach 1 MB
        call Dos1Call
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

; Reserve exactly the adjacent range, sharing the allocator's
; commit/init.
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
        ; new selectors reserved, not yet published
        push edx
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
        ; DOS may partially grow even on failure
        mov bx, cx
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

; Probe the selector-limited growth ceiling through DOS, then
; restore the
; original size. The visible block and descriptors are unchanged
; on error.
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
; Raw frame: saved EBP, vector, error, EIP/CS/flags/ESP/SS.
; Default
; nonfatal exceptions reflect through the same real-mode
; interrupt path.
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
        ; 0.9 compatibility corrections: a hooked PM interrupt
        ; receives
        ; an otherwise unhandled CPU exception before the default
        ; policy.
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
        ; the PM interrupt hook returns with IRET
        mov ebp, 1
        ; CPU defaults still require the locked stack
        jmp NativeLockedEvent

; A word client chained to its saved default handler. The seven
; copied
; dwords contain the documented word frame plus high-value
; carriers.
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

; Leave ring 0 on the normal kernel-1 stack before any cleanup
; can admit
; IRQs. An unhandled fatal child must not leave its resources in
; its parent.
; DPL-0 CPU gates make software INT distinguishable through #GP's
; IDT
; error code. INT3/INTO remain CPU traps; CD nn dispatches
; ClientIDT.
; Common CPU error frame, excluding the helper return and saved
; registers.
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
        ; architectural instruction-length limit
        mov bl, 15
.prefix:
        dec ebx
        js NativeSoftwareUnhandled
        cmp edi, edx
        ja NativeSoftwareUnhandled
        mov al, [es:edi]
        inc edi
        ; do not wrap a 32-bit fetch offset
        jz NativeSoftwareUnhandled
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
        ; discard #GP's synthetic RF
        and byte [esi + NSG_Flags+2], ~1
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
        ; helper return, vector and #GP error
        add esp, 12
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

; Preflight the complete frame at client privilege before any
; CPL0 store.
; Return ES:EDI ready for stores; on failure rewrite the original
; CPU
; frame for #SS/#PF without advancing IP. Preserve the mapping
; window.
NativeIntStack:
        pushad
        push dword [OffPage2 + ((OffPageTableWin-KernelBase) \
                >> 10)]
        mov edx, [esi + NSG_SS]
        lar eax, edx
        jnz .stack
        mov ebx, eax
        ; present DPL3 writable data, either direction
        and bh, 0FAh
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
        mov [OffPage2 + ((OffPageTableWin-KernelBase) >> 10)], \
                edx
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
.done:  pop dword [OffPage2 + ((OffPageTableWin-KernelBase) \
        >> 10)]
        mov eax, cr3
        mov cr3, eax
        popad
        ret

        DPROC NativeCPU2
        push byte 2
        jmp ExceptionWOCodeHL
        DPROC NativeCPU16
; Short branches share the two full-distance entries; all 16 IDT
; slots
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
; Like the original Trap3 table, neighboring instructions overlap
; their
; ignored offsets. CALL32's selector comes from the following
; four-byte
; slot. No instruction ever returns here except the separate
; state stub.
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
        ; state call returns at +8; fatal offset ignores it
        db RetfCode, 0
        dw FirstGateSelector + 8 + 3
NativeInt16End:
        bits 32
        ESEG Text
