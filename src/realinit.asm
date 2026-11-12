;             This file is part of the ZRDX OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

;real mode init routines
        SEGM IData16
        LByte IntroMsg
        db 'Zurenava DOS extender, version 0.53OSE. ', \
        'Copyright(C) 1998-1999, Sergey Belyakov', 13, 10, \
        'Copyright(C) 2026, Viacheslav Komenda', 13, 10, '$'
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
%ifdef NativeHost
        db 'native host needs direct VCPI/XMS/RAW ownership$'
%else
        db 'already in V86 mode without VCPI or DPMI$'
%endif
%ifdef NATIVEPE
        LByte NoHostEM
        db 'no DPMI host for the PE run$'
        LByte PEWrapEM
        db "command line too long for the PE run$"
        LByte RoleClient
        db 0
%endif
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
%ifndef NativeHost
%define CLIENTROLE
%elifdef NATIVEPE
%define CLIENTROLE
%endif
%macro ClientBody 2
        mov ax, 1687h
        int 2Fh
        or ax, ax
        _ifnot jz
%1:
          ret
        _endif
        shr bx, 1
        jnc %1
        ;RMCodeSize = 0 ;SegSizeEText16
        mov bp, cs
        ;mov  ss, bp
        lea ax, [bp + si + ((ROffLoaderEnd + 200h + 15) / 16) \
                + MouseRHandlerPSize]
        lea bx, [bp + MouseRHandlerPSize]
        lea bp, [bx + si]
        add [ROff %+ %2 + PSP], si
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
        ; !!!!!!!!!!!!!!
        mov si, ROffLoaderEnd - 4 + PSPSize
        mov cx, (ROffLoaderEnd - ROffPermanentPart + 3) / 4
        mov es, bp
        std
        rep movsd
        mov ds, bp
        mov [ROffPatchPoint8 - 2], bp
        mov ax, 1
        mov es, bx
        mov si, ROffSwitchModeEM
        ; workaround for MS NT5(windows 2000) beta 3
        cld
        retf
%endmacro
        SEGM IText16
;assume cs:DGROUP16, ds:DGROUP16, ss:DGROUP16
RInit:
InitDPMI:
PSPSize equ 100h
TryDPMI@167:
%ifdef NATIVEPE
        cmp byte [cs:ROffRoleClient], 0
        jne TryClient@171
%endif
%ifdef NativeHost
        mov ax, 1687h
        int 2Fh
        test ax, ax
        jnz .absent
        mov si, ROffAlreadyVMEM
        jmp RInitError
.absent:
        ret
        ; InitRXV fills the switch stubs before the remaining
        ; init calls.
%if (($-$$)-CurSegBaseR) < (ROffFirstSwitchCode \
        + MaxSystemSwitchCode + nMaxCallbacks*4)
        times (ROffFirstSwitchCode + MaxSystemSwitchCode \
                + nMaxCallbacks*4) - (($-$$)-CurSegBaseR) db 90h
%endif
%else
        ClientBody NoDPMI@167, MemBlock0Size
%endif
%ifdef NATIVEPE
TryClient@171:
        call TryClientX@171
        mov si, ROffNoHostEM
        jmp RInitError
TryClientX@171:
        ClientBody NoClientRet@171, MemBlock0SizeL
%endif
LS equ 0

%ifdef PEMAX
;ds = environment. CF set if it holds ZRDXROLE= (task 170).
RoleScan@170:
        push bx
        xor bx, bx
.n:     cmp byte [bx], 0
        je .no
        cmp word [bx], 525Ah
        jne .skip
        cmp word [bx + 2], 5844h
        jne .skip
        cmp word [bx + 4], 4F52h
        jne .skip
        cmp word [bx + 6], 454Ch
        jne .skip
        cmp byte [bx + 8], '='
        je .yes
.skip:  inc bx
        js .no
        cmp byte [bx - 1], 0
        jne .skip
        jmp .n
.yes:   pop bx
        stc
        ret
.no:    pop bx
        clc
        ret

;Open the file named by the first token of the command tail
;(es = PSP). Out: ax = handle; di and es are kept.
RoleOpen@170:
        push ds
        push es
        pop ds
        mov si, 81h
.sp:    lodsb
        cmp al, ' '
        je .sp
        dec si
        cmp word [si], '/#'
        jne .tok
        cmp byte [si + 2], 'R'
        jne .tok
        cmp byte [si + 3], ' '
        jne .tok
        add si, 3
        jmp .sp
.tok:   cmp byte [si], 0Dh
        je .fail
        mov dx, si
        mov bp, si
.en:    inc bp
        cmp byte [ds:bp], 21h
        jae .en
        mov al, [ds:bp]
        push ax
        mov byte [ds:bp], 0
        push dx
        mov bx, 20h
        xor cx, cx
        mov dx, 1
        mov ax, 716Ch
        stc
        int 21h
        pop dx
        jnc .ok
        mov si, dx
        mov ax, 3D20h
        int 21h
        jc .bad
.ok:    mov bx, ax
        pop ax
        mov [ds:bp], al
        mov ax, bx
        pop ds
        ret
.bad:   pop ax
        mov [ds:bp], al
.fail:  mov si, ROffFileAccessEM
        jmp RInitError

; ds = environment, es = PSP. CF set if the run is a client role:
; the
;environment holds ZRDXROLE= or the command tail starts with the
;marker token "/#R" (task 172).
RoleCheck@172:
        call RoleScan@170
        jc .yes
        push si
        mov si, 81h
.sp:    es lodsb
        cmp al, ' '
        je .sp
        cmp al, '/'
        jne .no
        es lodsw
        cmp ax, '#R'
        jne .no
        es lodsb
        cmp al, ' '
        je .yes0
        cmp al, 0Dh
        je .yes0
.no:    pop si
        clc
        ret
.yes0:  pop si
.yes:   stc
        ret

%ifdef NATIVEPE
;Host role, ds = es = PSP. If the first token of the command tail
; names a PE file, the tail becomes "<own path> /#R <old tail>":
; the
;host then runs itself as the client role (task 172).
PEWrap@172:
        mov si, 81h
.sp:    lodsb
        cmp al, ' '
        je .sp
        cmp al, 9
        je .sp
        cmp al, 0Dh
        je near .none
        dec si
        mov dx, si
        mov bp, si
.en:    inc bp
        cmp byte [ds:bp], 21h
        jae .en
        mov al, [ds:bp]
        push ax
        mov byte [ds:bp], 0
        mov ax, 3D00h
        int 21h
        jc .res0
        mov bx, ax
        sub sp, 64
        mov di, sp
        push ds
        push ss
        pop ds
        mov dx, di
        mov cx, 64
        mov ah, 3Fh
        int 21h
        jc .rd
        cmp ax, 64
        jb .rd
        cmp word [di], 'MZ'
        jne .rd
        mov dx, [di + 3Ch]
        mov cx, [di + 3Eh]
        mov ax, 4200h
        int 21h
        jc .rd
        mov dx, di
        mov cx, 4
        mov ah, 3Fh
        int 21h
        jc .rd
        cmp ax, 4
        jb .rd
        cmp word [di], 'PE'
        jne .rd
        cmp word [di + 2], 0
        jne .rd
        mov si, 1
        jmp short .cl
.rd:    xor si, si
.cl:    mov ah, 3Eh
        int 21h
        pop ds
        add sp, 64
        jmp short .res
.res0:  xor si, si
.res:   pop ax
        mov [ds:bp], al
        or si, si
        jnz .pe
.none:  ret
.pe:    mov es, [2Ch]
        xor di, di
        xor ax, ax
        cld
.ev:    cmp byte [es:di], 0
        je .evd
        mov cx, 7FFFh
        sub cx, di
        repne scasb
        jne near .fail
        jmp .ev
.evd:   add di, 3
        mov si, di
.pl:    cmp byte [es:di], 0
        je .pld
        inc di
        cmp di, 7FFFh
        jb .pl
        jmp .fail
.pld:   mov cx, di
        sub cx, si
        mov bx, cx
        add bx, 5
        xor ax, ax
        mov al, [80h]
        mov dx, ax
        add dx, bx
        cmp dx, 126
        ja .fail
        push es
        push ds
        pop es
        push si
        push cx
        mov si, 81h
        add si, ax
        mov di, si
        add di, bx
        mov cx, ax
        inc cx
        std
        rep movsb
        cld
        mov [80h], dl
        pop cx
        pop si
        pop ds
        mov di, 81h
        mov al, ' '
        stosb
        rep movsb
        mov al, ' '
        stosb
        mov ax, '/#'
        stosw
        mov al, 'R'
        stosb
        push es
        pop ds
        ret
.fail:  mov si, ROffPEWrapEM
        jmp RInitError
%endif
%endif

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
          add dx, (RMBSSEnd + 1FFFh + PSPSize) / 16
        mov dl, 0              ;page aligment
        mov cl, 0
        cmp cx, dx
        _ifnot ja
          mov cx, dx
        _endif
        mov ax, ds
        ; get para size of dpmi real mode segment
        sub ax, cx
                                ;and vcpi page0
        neg ax
        mov [ROffMemBlock0Size + PSP], ax
        sub cx, dx
        mov [ROffNExtraRPages + PSP], ch
        mov di, dx
        dec dh
        mov es, dx
%ifndef NativeHost
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
        ; set bitmask for allocated descriptors
        mov byte [ROffLDTFree + (16 / 8) + PSP], 11110b
%endif
        ; skip first page and
        mov di, WinSize + 1000h + (OffLastInit - KernelBase) - 4
        mov si, OffProtectedStart + (OffLastInit - KernelBase) \
                + PSP - 4
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
        ; mov Page0Ref[LSX], eax
        mov [(OffPage2 + ((OffPage0 - KernelBase) >> 10)) \
                + LSX], eax
        mov eax, [si + ((OffPage2 + LSX) / 1024) - 4]
        ; set reference to page 2 in the page directory
        call MovePageRef1@167

        mov di, OffPage2 + LSX
        add si, WinSize / 1024
        mov cl, (OffLastInit - KernelBase + 0FFFh) / 1000h
        _do
        ; set references to kernel in the page 2 table
        call MovePageRef@167
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

%ifdef CLIENTROLE
        LLabel DefaultFlatDescriptor
        ; Flat data descriptor
        Descr 0, 0FFFFFh, 0CF93h
        LLabel ILoaderEntry0
        jc near RInitError
        LLabel ILoaderEntry
        ; workaround for windows 3000:)
        movzx esp, sp
        ; save psp to extender
        mov [ROffSavedPSP + LS], es
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
        ; allocate selector for flat
        int 31h
        jc near PInitErrorSel
        ; save flat selector to extender
        mov [ROffFlatSelector + LS], ax
        xchg bx, ax
        mov ax, 0Ch
        int 31h                     ;set flat descriptor
        jc near PInitErrorSel
        mov fs, bx
        mov dx, ExtenderSize / 4
        mov cx, ExtenderFullSize
        mov bp, ExtenderStart + LS
        ; allocate and move extender
        call AllocAndMove@167
        mov [ROffExtenderSel + LS], bx
%ifdef PEMAX
        mov [ROffExtenderSelB + LS], bx
        mov [ROffExtenderSelC + LS], bx
        mov [ROffExtenderSelD + LS], bx
        mov [ROffExtenderSelE + LS], bx
        mov [ROffExtenderSelV + LS], bx
%endif
        ; save code selector for extender
        mov bp, bx
        mov ds, ax
        ; save code selector on the stack
        push bx
        ; save data selector on the stack
        push ax
        ;es, ds - selector of the extender
;assume es:EGroup
        mov [dword OffSelReference0 - 4], ax
        mov [dword OffSelReference1 - 2], ax
%ifdef PEMAX
        mov [dword OffPESelD - 4], ax
        push ax
        mov ax, [dword OffFlatSelector]
        mov [dword OffPESelE - 4], ax
        pop ax
%endif
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
        cmp si, OffFirstExtHandler + NHookedInterrupts \
                * ExtHandlerStep
        _enddo jb
        ; set default selector
        mov [dword OffClientInt0Vector + 4], cx
                                                 ; for client
                                                 ; int0 handler
; ----------------- set interrupt vector for coprocessor
; ---------------------
        mov ax, 400h
        int 31h
        mov bl, dl
        add bl, 5
        mov [dword OffOldVectors + 6], bl
        mov ax, 204h
        int 31h
        mov [dword OffOldVectors], edx
        mov [dword OffOldVectors + 4], cx
        mov dx, int75handler
        mov cx, bp
        mov ax, 205h
        int 31h
        jc PInitErrorSetVect
; ----------------------allocate a callback for
; mouse-------------------------
        ; setup selector for default mouse hook proc
        mov [dword OffMouseHookProc + 4], bp
        mov esi, MouseCallbackHandler
        ; high part of edi already zero
        mov di, OffMouseCallbackStruct
        ; code selector of the extender
        mov ds, bp
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
; ---------------------------- init debugger
; --------------------------------
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
; ----------------------------- setup loader
; ---------------------------------
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
        ; put transfer segment here
        mov word [di + DC_DS], 0
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

%endif
        .8086
;RInitMemError:
; ---------------------------------- startup
; ----------------------------------
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
        ; check if all bits 12-15 in flags are set
        sub ah, bh
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
        ; set PCD in int31(800h)
        mov byte [ROffPatchPoint4 + PSP - 1], 77h
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
; ----------------------- real mode init error handler
; ------------------------
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

%ifdef NATIVEPE
        call RoleCheck@172
        _ifnot jnc
          mov byte [cs:ROffRoleClient], 1
          jmp ClientFront@171
        _endif
%endif
%ifdef NativeHost
        mov ax, es
        mov [cs:ROffNativeRootPSP], ax
        push cs
        pop ax
        mov [cs:ROffNativeRMCS], ax
        mov al, [cs:ROffCPUType]
        mov [cs:ROffNativeCPU], al
        push es
        pop ds
%ifdef NATIVEPE
        call PEWrap@172
%endif
%endif
%ifdef CLIENTROLE
%ifdef NATIVEPE
        jmp FrontDone@171
ClientFront@171:
%endif
        inc di
        inc di
        mov dx, di
        _do
        inc di
        js RInitError
        cmp [di - 1], ah
        _enddo jne
%ifdef PEMAX
        call RoleCheck@172
        jnc NoRole@170
        call RoleOpen@170
        jmp short RoleOpened@170
NoRole@170:
%endif
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
                                 ; the file may be opened by
                                 ; other instance
                                 ;with same open mode only
          int 21h
          jc RInitError
        _endif
%ifdef PEMAX
RoleOpened@170:
%endif
        push es
        pop ds
%ifndef NativeHost
        add [ROffEnvSize + PSP], di
%endif
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
        cmp dword [si], 'ZRDX'
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

%ifdef NATIVEPE
FrontDone@171:
%endif
%endif
; ------------ Setup linear disp for all references to 16Stub
; -----------------
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
%ifdef CLIENTROLE
        ; patch transfer buffer size
        mov ax, [ROffTransferBufferPSize + PSP]
        mov [ROffPatchPointTStSz - 2 + PSP], ax  ;in loader code
%endif
        ; DPMI/VCPI check sequence?
        shr byte [InitFlags + PSP], 1
        _ifnot jnc
          call TryVCPI@167
          call TryDPMI@167
        _else jmp
          call TryDPMI@167
          call TryVCPI@167
        _endif
%ifdef SWAP
;only the host needs the file (no VCPI, no DPMI provider)
        push ds
        pop es
%ifdef NativeHost
;optional "/S<hex MB>" before the program name sets the swap
;size; it is blanked out of the command tail (task 133)
        mov si, 81h
SwSk@167:
        lodsb
        cmp al, ' '
        je SwSk@167
        cmp al, '/'
        jne SwArgs@167
        lea di, [si - 1]
        lodsb
        and al, 0DFh
        cmp al, 'S'
        jne SwArgs@167
        xor bx, bx
SwHx@167:
        lodsb
        sub al, '0'
        cmp al, 9
        jbe SwDg@167
        and al, 0DFh
        sub al, 7
        cmp al, 10
        jb SwBlank@167
SwDg@167:
        cmp al, 15
        ja SwBlank@167
        shl bl, 4
        or bl, al
        jmp SwHx@167
SwBlank@167:
        mov [InitFlags + PSP + 1], bl
        mov cx, si
        sub cx, di
        dec cx
        mov al, ' '
        rep stosb
SwArgs@167:
%endif
;swap: the size in megabytes is the high byte of InitFlags
;(0 = off); the file itself is made at the first page out
;(SwapCreate). Keep the host PSP and the InDOS flag address.
        mov al, [InitFlags + PSP + 1]
        mov [ROffSwapSlots + PSP], al
        test al, al
        jz SwNone@167
        mov ah, 51h
        int 21h
        mov [ROffSwapPSP + PSP], bx
        mov ax, [2Ch]              ;the real environment, before
        ; the PM selector replaces it
        mov [ROffSwapNameSeg + PSP], ax
        push es
        mov ah, 34h
        int 21h                  ;InDOS flag address
        mov ax, es
        pop es
        movzx eax, ax
        shl eax, 4
        movzx ebx, bx
        add eax, ebx
        mov [ROffSwapInDos + PSP], eax
SwNone@167:
        push ds
        pop es
%endif
TryXMS@167:
;look for PE bit in cr0 - must be cleared for RAW/XMS!
        smsw ax
        shr ax, 1
        mov si, ROffAlreadyVMEM
        jc RInitError
;patch 2 points for working under RAW/XMS
        mov word [VCPIRICall + PSP], JmpShortCode + 1 * 100h
        mov word [PatchPoint + PSP], JmpShortCode \
                + (RawSwitcherToPM - PatchPoint - 2) * 100h
        call FindXMSServer
        jnz near TryRAW@167
        mov ah, 5
        call far [ROffXMC + PSP]         ;local enable A20
        or ax, ax
        ; extended memory not available if impossible to enable
        ; A20
        _ifnot jnz
          ; disable XMS block allocation
          dec byte [ROffXMSBlockNotAllocated + PSP]
          jmp InitRX
        _endif
        ; host owns one local enable
        mov byte [ROffA20State + PSP], 5
;change gate descriptor for PM->RM mode switch in XMS/RAW
InitRX:
        mov dword [ROffVCPICallDesc + PSP], ROffL0234 \
                + ((VCPISelector + 8) << 16)
        mov byte [ROffVCPICallDesc + PSP + 6], 0
        jmp InitRXV
FindXMSServer:
;check for XMS server
        mov ax, 4300h
        int 2Fh
        cmp al, 80h                ;is XMS available ?
        _ifnot jne
          mov ax, 4310h
          ; get XMS entry - always succefful
          int 2Fh
          mov [ROffXMC + PSP], bx
          mov [ROffXMC + PSP + 2], es
          ; enable XMS block allocation
          inc byte [ROffXMSBlockNotAllocated + PSP]
          xor bx, bx        ;clear ZF
        _endif
        ret
TryVCPI@167:
; @@VTest:                    ;in V86 mode int 67h shall be
; supported anywhere
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
; ---------------------- init DPMI host under
; VCPI----------------------------
        inc byte [ROffVCPIMemAvailable + PSP]
        call FindXMSServer
        mov ax, 0DE03h
        int 67h
        mov [ROffTotalVCPIPages + PSP], edx
; ------------------- init DPMI host under VCPI/XMS/RAW
; -----------------------
InitRXV:
        push ds
        pop es
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
        ; this code may by replaced with "mov eax,"
        jmp short PatchPoint7End
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

; ------------------------- set gate descriptor in GDT
; -----------------------
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
        mov word [ROffFirstTrap3 + PSP + (4 * 31h) + 5], \
                DPMIEntryGateSelector
        mov byte [dword ROffFirstTrap3 + OffPMSaveStateTrap3 \
                + 7 + PSP], RetfCode
%ifdef VENDORON
%ifndef VENDORPE
        mov byte [dword ROffFirstTrap3 + OffVendorTrap3 + 7 \
                + PSP], RetfCode
%endif
%endif

; -------------------------- init some TSS fields
; ------------------------
        mov dword [ROffTSS + PSP + TSS_ESP0], OffKernelStack
        mov dword [ROffTSS + PSP + TSS_ESP1], OffKernelStack1End
        mov word [ROffTSS + PSP + TSS_SS0], Data0Selector
        mov word [ROffTSS + PSP + TSS_SS1], Data1Selector
        mov word [ROffTSS + PSP + TSS_IOBASE], 0FFFFh

%include "realini2.asm"
