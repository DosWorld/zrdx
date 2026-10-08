;             This file is part of the ZRDX 0.50OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

        SEGM Text16
;assume cs:dgroup16, ds:nothing, es:nothing, ss:nothing
;assume cs:Text16, ds:nothing, es:nothing, ss:nothing
        LLabel MouseRHandler
        dw 20
        db 'ZRDX0.50'
InitFlags: dw 2
        LWord TransferBufferPSize
        dw 0400h ;400h
        LDWord MaxXMSAllocate
        dd 0ffffffffh
        LWord MemReserve
        dw 0F000h ;0F000h
        dw 0
        LByte MouseBusyFlag
        db 1 ;mouse busy flag
        LLabel MouseRHandlerEntry
        shr byte [cs:ROffMouseBusyFlag], 1
        _ifnot jnc
        db JmpFarCode
        LDWord MouseCallbackPlace
        dw 0, 0
        mov byte [cs:ROffMouseBusyFlag], 1
        _endif
        retf
        LLabel MouseRHandlerEnd
MouseRHandlerPSize equ (ROffMouseRHandlerEnd - ROffMouseRHandler + 15) / 16
MouseRHandlerSize equ MouseRHandlerPSize * 16
        LWord OldInt22
      dw 0, 0
        LDWord XMC             ;address of XMS entry point
        dd 0
        LWord XMHandle         ;handle of XMS memory block
        dw 0 ;NULL if no XMS memory allocated
        LDWord FreeXMSCount
        dd 0
        LDWord FirstFreeXMS
        dd 0
        LDWord TotalXMSPages
        dd 0
        LWord DefIDT           ;default IDT for real mode
        dw 3FFh, 0, 0
        LDWord PMCR0
        dd 0
        LDWord RMCR0
        dd 0
;protected mode IDT
        LWord IDTRSize
        dw 100h * 8 - 1 ;IDT always full
        LWord IDTRBase
        dd OffIDT ;fixed address
;protected mode GDT
        LWord GDTRSize
        dw OffGDTEnd - OffGDT - 1
        LDWord GDTRBase
        dd OffGDT ;fixed address
;pointer to current real mode stack for DPMI client
        LDWord RMStack
        dw OffRStackEnd
        dw DGROUP16
;pointer to current protected mode stack for DPMI client
        LDWord PMStack
        dd 0, 0
        LLabel SwitchTable
        LDWord SwitchTableCR3
        dd 0 ;Must be set during init
SwitchTableGDT: dd OffGDTRSize
;FirstRelocR = $ - 4      ;First relocation address in module
;LastRelocR  = $
        RRT      ;place it to real mode relocation table
SwitchTableIDT: dd OffIDTRSize
        RRT      ;too
SwitchTableLDTR: dw LDTSelector ;
SwitchTableTR: dw TSSSelector
        LDWord SwitchTableEIP
        dd OffPMInit ;fixed offset of first PM entry, shell be
                               ;changed by real mode handlers
SwitchTableCS: dw Code0Selector
        LByte HasExitMessage
        db 0
int15handler:
        cmp ah, 88h
        _ifnot je
        db JmpFarCode
        LLabel OldInt15
        dd 0
        _endif
        LLabel PatchPoint_int15
        mov ax, 0
        push bp
        mov bp, sp
        and byte [bp + 6], ~(1)  ;clear carry
        pop bp
        LLabel int67Handler
        iret

        DPROC AllocXMSBlock
        push cs
        pop ds
        ;allocate and lock lagest available block
        mov ah, 88h
        call XMSICall@1           ;get free mem v3+
        or bl, bl
        _ifnot je
          mov ah, 8
          call XMSICall@1         ;get free mem v2-
          movzx eax, ax
        _endif
        mov ebp, [ROffMaxXMSAllocate]
        cmp ebp, eax
        _ifnot jb
          xchg ebp, eax
        _endif
        mov edx, ebp
        or edx, edx
        _ifnot je
          mov ah, 89h               ;alloc block v3+
          call XMSICall@1
          _ifnot jne
            mov ah, 9h             ;alloc block v2-
            call XMSICall@1
            jz NoXMS@1
          _endif
          mov [ROffXMHandle], dx   ;save handle
          mov ah, 0Ch
          call XMSICall@1           ;lock block
          _ifnot jnz
            mov ah, 0Ah
            mov dx, [ROffXMHandle]
            call XMSICall@1         ;free not succefully locked block
NoXMS@1:
            xor ebp, ebp
          _endif
          shl edx, 16
          mov dx, bx               ;prepare 32 bit pointer in edx
        _endif
        mov bx, ROffFreeXMSCount
        call TranslateMemLimits
        iret

;ebp - mem size in B, edx - mem base in B
;
TranslateMemLimits:
        shl ebp, 10
        add ebp, edx
        rcr ebp, 1                 ;in case of 4G upper bound:-)
        shr ebp, 11
        add edx, 0FFFh
        shr edx, 12
        sub ebp, edx
        _ifnot jae
          xor ebp, ebp
        _endif
        mov [bx], ebp
        mov [bx + ROffTotalXMSPages - ROffFreeXMSCount], ebp
        mov [bx + ROffFirstFreeXMS - ROffFreeXMSCount], edx
        ret
XMSICall@1:
        call far [ROffXMC]
        or ax, ax
        ret


;warning - this code executed after program termination
;in a free dos memory block on small and unstable stack
TerminateRHandler:
        pushf
        cli
        push ax
        push bx
        mov ax, ss
        mov bx, sp
        push cs
        pop ss
        mov sp, ROffExitStackEnd
TSFrameSize equ 8 * 4 + 4 * 2
        push ds
        push es
        push fs
        push gs
        pushad
        push TerminateSwitchCode + 3 ;push switch code for RMS handler
        jmp RMS_RMHandlerL        ;go to PM terminate handler
;continued after DPMI host are cleaned
TerminateRHandler2:

        _do     ;free VCPI pages with DPMI host internal code&data
          pop edx
          test dh, VCPIPageBit
          _ifnot jz
            and dx, 0F000h
            mov ax, 0DE05h
            int 67h
          _endif
        _enddo loop
ExitXMS:
        mov ah, 0Dh              ;unlock
        mov dx, [ROffXMHandle]
        or dx, dx
        _ifnot jz
          call far [ROffXMC]               ;unlock and free XMS if handle is not null
          mov ah, 0Ah            ;free
          call far [ROffXMC]
        _endif
PatchPoint1:
        ;mov  ah, 05H            ;local disable A20 always
        jmp short Exit2         ;may be patched with "mov ah, 05h" (B4 05)
                                 ;if A20 local enabled with XMS
        call far [ROffXMC]
Exit2:
        cmp byte [ROffHasExitMessage], 0
        _ifnot je
%ifndef Release
          mov ax, 3
          int 21h
%endif
        mov dx, ROffRStackStart + 4
        mov ah, 9
        int 21h        ;display exit message
        _endif
        mov sp, ROffExitStackEnd - TSFrameSize
        popad
        pop gs
        pop fs
        pop es
        pop ds
        mov ss, ax     ;restore dos stack
        mov sp, bx
        pop bx
        pop ax
        popf
        jmp far [cs:ROffOldInt22]  ;chain to next int 22 handler

        LLabel Int214C
        mov ax, 4CFFh
        int 21h
UnexpectedPMError:
        sti
        jmp $
;ESEG Text16
;Segm Text
;assume cs:Text
;ENDP

        DPROC RMSaveState
        push si
        push di
        push ds
        push es
        pushf
        mov si, ROffRMStack
        or al, al
        cld
        push cs
        _ifnot jne
          pop ds
        _else jmp
          xchg si, di
          push es
          pop ds
          pop es
        _endif
%rep 5
        movsw
%endrep
        popf
        pop es
        pop ds
        pop di
        pop si
        retf

;emulator of switch to VM86, really switch to RM with same stack frame
;ESeg Text
;Segm Text16
;assume cs:Text16
        DPROC RawSwitchToRM1
        LLabel L0234
        push eax
        mov ax, VCPISelector + 16
        mov ds, ax
        mov byte [ss:dword OffGDT + TSSSelector + 5], 80h + SS_FREE_TSS3 ;mark current TSS as FREE
        mov es, ax
        mov gs, ax
        mov [ROffRStackStart], ebx
        mov fs, ax
        pop ebx
        mov ss, ax
        mov esp, ROffRStackStart + 40h
        ror ebx, 4               ;convert linear to segment
        mov eax, [ROffRMCR0]
        and eax, ~(80000000h)   ;disable paging in protected mode
        mov cr0, eax
        xor eax, eax
        mov cr3, eax             ;clear cr3 in protected mode
        mov eax, cr0
        and al, ~(1)
        lidt [ROffDefIDT]
        mov cr0, eax             ;switch to real mode
        db JmpFarCode
        dw ROffL0235
        dw DGROUP16
%assign F 0
        LLabel L0235
        mov ds, bx
        shr ebx, 28
        lss esp, [bx + F + VMI_ESP]
        mov es, [bx + F + VMI_ES]
        mov fs, [bx + F + VMI_FS]
        mov gs, [bx + F + VMI_GS]
        mov ds, [bx + F + VMI_DS]
        mov ebx, [cs:ROffRStackStart]
;this code always entered after PM to VM VCPI switch to restore eax, flags
;and jump to entry point
pop_eax_iret:
        pop eax
        iret


nPassups equ 20
        LLabel AutoPassupRJmps
        times 5 * nPassups db 0 ;reserve space for DPMI autopassup traps

;RM handler for switch from VM to PM
;must be placed on bottom of
        LLabel RMS_RMHandler
RMS_RMHandlerL:
        push gs       ;save all registers, are not transferred
        push fs       ;save all registers, are not transferred
        push ds       ;save all registers, are not transferred
        push es       ;save all registers, are not transferred
        push eax       ;by VCPI
        push ebp       ;by VCPI
        push esi       ;by VCPI
        pushf
SwitchToPM:
        cli
        mov bp, ss
        shl ebp, 16
        mov bp, sp             ;store RM ss:sp to ebx
PatchPoint:
        mov esi, OffSwitchTable
        RRT
        mov ax, 0DE0Ch
        int 67h

;raw switcher to PM from RM with same format, as VCPI
RawSwitcherToPM:
;-------------------------- load system tables ------------------------------
        push cs
        mov si, ROffSwitchTable + 12
        pop ds
        pushf                    ;set IOPL 3 and NT 0
        mov eax, [si - 12]
        mov cr3, eax
        lgdt [si + ROffGDTRSize - ROffSwitchTable - 12]
        pop ax
        ;or   ah, 30h
        and ah, ~(40h)
        push ax
        popf
        mov eax, cr0
        or eax, 80000001h      ;enable paging and PM
        lidt [si + ROffIDTRSize - ROffSwitchTable - 12]
        mov cr0, eax
        db JmpFarCode
        dw ROffRawSwitchPMEntry0
        dw VCPISelector + 8
        LLabel RawSwitchPMEntry0
        lldt [si]
        ltr [si + 2]
        jmp dword far [si + 4]
        ESEG Text16
