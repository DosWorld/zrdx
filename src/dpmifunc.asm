;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

%define zcat4(a,b,c,d) a %+ b %+ c %+ d

%macro zlabx 1
%1:
%endmacro

%macro DPMIFn 2
        %assign _dflo %2
        zlabx zcat4(DPMIFn_,%1,_,_dflo)
%endmacro

%macro DPMIRow 2-*
        LDWord zcat2(Table2,%1)
%rotate 1
%rep %0 - 1
%ifidn %1, InvalidFunction
        dw OffInvalidFunction - KernelBase
%else
        dw (%1 - $$) - CurSegBase - KernelBase
%endif
%rotate 1
%endrep
%endmacro

%if SegSizeText > 10000h
%error DPMI word dispatch offsets require Text within 64 KB
%endif

        SEGM Text
;assume cs:dgroup, ss:dgroup, es:nothing, ds:dgroup
;assume cs:Text, ss:nothing, es:nothing, ds:nothing
;DPMIFrame   struc
DPMIFrame_Ret equ 0
DPMIFrame_EBP equ 4
DPMIFrame_ESI equ 8
DPMIFrame_DS equ 12
DPMIFrame_ES equ 16
DPMIFrame_FS equ 20
DPMIFrame_GS equ 24
%ifdef NativeHost
DPMIFrame_Func equ 28
DPMIFrame_OrigSI equ 32
DPMIFrame_OrigDI equ 36
DPMIFrame_OrigDX equ 40
DPMIExtra equ 16
%else
DPMIExtra equ 0
%endif
DPMIFrame_SF equ 28 + DPMIExtra
DPMIFrame_size equ 44 + DPMIExtra
        DPROC DPMIIntEntry
        cli
        cld                     ;client DF is restored by its saved IRET frame
%ifdef NativeHost
        push edx
        push edi
        push esi
        push eax
%endif
        push gs
        push fs
        push es
        push ds
        push esi
        push ebp
%assign F 6 * 4 + DPMIExtra
        mov ebp, ss
        mov ds, ebp
        ClientFrameSP F + CFrESP, F + CFrSS
%ifdef NativeHost
        call NativeBefore31
        jc DPMIErrorN
%endif
        cmp ah, 0Bh
        movzx esi, al
        movzx ebp, ah
        ja FnNumError@10
        cmp al, [ebp + OffDPMITable2N]
        ; The first level indexes a compact word table; handlers are
        ; offsets from KernelBase within the resident 64 KB Text window.
        movzx ebp, byte [ebp + OffDPMITable2]
        jae FnNumError@10
        movzx ebp, word [ebp + esi*2 + OffTable20]
        add ebp, KernelBase
        call ebp
        ;clc
        or ebp, ebp           ;faster then direct clc
DPMIIret:
%ifdef NativeHost
        call NativeAfter31
%endif
        lds esi, [esp + F + CFrESP] ;load client ss:esp
        lea esi, [esi + 12]            ;instread of add to save CF
        push ds                      ;client ss
        push esi                     ;client new esp

        mov ebp, [esi - 4]         ;load eflags from client iret frame
        rcr ebp, 1                  ;copy CF to BIT0 of ebp
        rol ebp, 1
        push ebp                     ;eflags
        push dword [esi - 8]    ;cs
        push dword [esi - 12]   ;eip
        mov es, [esp + (5 * 4) + (3 * 4)]
        mov fs, [esp + (5 * 4) + (4 * 4)]
        ;mov  ebp, ss:[esp+5*4+5*4]
        mov gs, [esp + (5 * 4) + (5 * 4)]
        ;mov  gs, ebp
        mov ebp, [esp + (5 * 4)]
        lds esi, [esp + (5 * 4) + 4]
%ifdef NativeHost
        call NativeFinishIret
%endif
        iretd

DPMIError3N:
        pop ebp
DPMIError2N:
        pop ebp
DPMIError1N:
        pop ebp
        jmp DPMIErrorN
DPMIError2InvSel:
        pop ebp
DPMIError1InvSel:
        mov al, 22h       ;invalid selector
        jmp DPMIError1
DPMIError2InvVal:
        pop ebp
DPMIError1InvVal:
        mov al, 21h       ;invalid value
        jmp DPMIError1
DPMIError4:
        pop ebp
DPMIError3:
        pop ebp
DPMIError2:
        pop ebp      ;pop 2 dword from stack and ret error flag
DPMIError1: ;pop 1 dword ...
        pop ebp          ;drop return address
        jmp DPMIError
        LLabel InvalidFunction
UnsupportedCall:
        mov al, 1
        jmp DPMIError1       ;table dispatch pushed a return address
FnNumError@10:
        mov al, 1
DPMIError:
        mov ah, 80h
DPMIErrorN:
        stc
        jmp DPMIIret
%macro FnRet 0
        ret
%endmacro

        DPMIFn 0, 0
        push ecx
        call AllocSelectors
        jc DPMIError1
        FnRet

        DPMIFn 0, 1
        mov si, bx
        xor si, [esp + DPMIFrame_SF + CFrSS]
        test si, ~3
        jz DPMIError1InvSel       ;RPL does not distinguish the active stack
        call CheckSelector
FreeDescriptor1:
        btr dword [OffLDTFree], esi ;clear alloc bit in flag vector
        btr dword [OffLDTBlock], esi
        mov byte [ebp + 5], 0      ;clear descriptor to prevent next use
        push edi
        lea edi, [esp + 4 + DPMIFrame_DS]
        lea ebp, [esi + 1]
        call ClearSelectorCacheRange
        pop edi
        FnRet

; ESI..EBP is an LDT-index interval; EDI addresses saved DS/ES/FS/GS.
; Share cache invalidation between ordinary and DOS-array descriptor frees.
ClearSelectorCacheRange:
        pushad
        push 4
        pop ecx
.loop:  movzx edx, word [edi]
        test dl, 4
        jz .next
        shr edx, 3
        cmp edx, esi
        jb .next
        cmp edx, ebp
        jae .next
        and dword [edi], 0
.next:  add edi, 4
        loop .loop
        popad
        ret

        DPMIFn 0, 2
        movzx ebp, word [OffLDTLimit]
        shr ebp, 3
        _do
          bt dword [OffLDTDOS], ebp
          _ifnot jnc
            mov esi, [ebp*8 + OffLDT + 2]
            shr esi, 4
            cmp bx, si
            je RetThis@10
          _endif
        _while
          dec ebp
        _enddo jnz
        push 1
        call AllocSelectors
        jc DPMIError1
        bts dword [OffLDTDOS], esi
        movzx esi, bx
        shl esi, 4
        mov word [ebp + 0], 0FFFFh
        mov [ebp + 2], esi
        mov byte [ebp + 5], 0F3h    ;Data R/W PL3  mode32
        ;mov  byte ptr [ebp].7, 0
        FnRet
RetThis@10:
        lea ebp, [ebp*8 + 7]
        mov ax, bp
        FnRet

        DPMIFn 0, 3
        mov ax, 8
        FnRet

        DPMIFn 0, 6
        call CheckSelectorRead  ;(bx)
        mov ch, [ebp + 7]
        mov cl, [ebp + 4]
        mov dx, [ebp + 2]
        FnRet

        DPMIFn 0, 7
        call CheckSelector  ;(bx)
        mov [ebp + 7], ch
        mov [ebp + 4], cl
        mov [ebp + 2], dx
        FnRet

        DPMIFn 0, 8
        call CheckSelector
        mov esi, edx
        push edx
        shl edx, 16
        mov dx, cx
        cmp cx, 0Fh
        _ifnot jbe
          ror edx, 12
          add dx, 10h
          jnc DPMIError2InvVal2
          or dl, 80h
          mov esi, edx
          shr esi, 16
        _endif
        and byte [ebp + 6], 70h
        or [ebp + 6], dl
        pop edx
        mov [ebp], si
        FnRet
CheckSelectorInCX@10:
        lar esi, ecx
        jnz DPMIError2InvVal
        shr esi, 9
        and esi, 0FEh >> 1
        cmp esi, 0FAh >> 1
        jne DPMIError2InvVal
        ret
DPMIError2InvVal2:
        pop edx
        push ebp
DPMIError2InvVal1:
        pop ebp
DPMIError1InvVal1:
        jmp DPMIError1InvVal
;check access right
CheckACC@10:
        xchg ax, si
        xor al, 70h
        test al, 70h
        jnz DPMIError2InvVal  ;this bits must be 1
        test ah, 20h
        jnz DPMIError2InvVal     ;reserved extended-rights bit, even if P=0
        test al, 80h
        _ifnot jz               ;0.9 errata permit arbitrary absent types
          and al, 1110b
          cmp al, 1010b
          _ifnot je
            test al, 1000b
            jnz DPMIError2InvVal
          _endif
        _endif
        xchg ax, si
        ret

        DPMIFn 0, 9
        call CheckSelector  ;(bx)
        mov esi, ecx
        call CheckACC@10
        push ecx
        mov [ebp + 5], cl
        and ch, 11010000b
        and byte [ebp + 6], 101111b
        or [ebp + 6], ch
        pop ecx
        FnRet

        DPMIFn 0, 0Ah
        call CheckSelectorRead  ;(bx)
        push dword [ebp]
        push edx
        mov edx, [ebp + 4]
        and dh, ~(1100b)     ;now data with up extend
        or dh, 10b           ;enable write
        push 1
        call AllocSelectors
        mov esi, edx
        pop edx
        jc DPMIError2
        mov [ebp + 4], esi
        pop dword [ebp]
        FnRet

        DPMIFn 0, 0Bh
        call CheckSelectorRead
        mov esi, [ebp]
        mov [es:edi], esi
        mov esi, [ebp + 4]
        mov [es:edi + 4], esi
        FnRet

        DPMIFn 0, 0Ch
        call CheckSelector
        movzx esi, word [es:edi + 5]
        call CheckACC@10
        mov esi, [es:edi]
        mov [ebp], esi
        mov esi, [es:edi + 4]
        mov [ebp + 4], esi
        FnRet
        DPMIFn 0, 0Dh
        movzx esi, bx
        test esi, 4
        jz DPMIError1InvSel     ;must select the LDT; RPL is ignored
        shr esi, 3
        cmp esi, 16
        jae DPMIError1InvSel    ;only indices 0-15 are reserved
        bts dword [OffLDTFree], esi
        jc DPMIError1InvSel    ;not free selector
InitOneSelector:
        and dword [OffLDT + esi*8], 0
        mov dword [OffLDT + esi*8 + 4], 40F200h
        ret

%ifdef NativeHost
        DPMIFn 1, 0
        jmp NativeDOSAlloc
        DPMIFn 1, 1
        jmp NativeDOSFree
        DPMIFn 1, 2
        jmp NativeDOSResize
%else
        DPMIFn 1, 0
        test bx, bx
        jnz DOSAllocSize@10
        dec bx                   ;query the maximum instead of a zero block
DOSAllocSize@10:
        push 1
        call AllocSelectors
        jc DOSNoSelector@10
        push eax       ;save selector
        mov ah, 48h   ;Get Block
        ;mov  ax, 0
        ;mov  ss, eax
        call Dos1Call
        _ifnot jnc
          btr dword [OffLDTFree], esi           ;clear alloc bit
          and dword [ebp + 4], 0    ;clear descriptor
          jmp DPMIError2N
        _endif
        bts dword [OffLDTBlock], esi
        pop esi                     ;add esp, 4
        mov dx, si
        ;jmp  SetupDOSSelector
%endif
;ebp - descriptor, ax - RM segment, bx - size in paragraphs
        DPROC SetupDOSSelector
        push eax
        push ebx
        movzx eax, ax
        shl eax, 4                 ;convert segment to LA
        mov [ebp + 2], eax           ;write base
        shl ebx, 4                 ;convert size to bytes
        dec ebx                    ;if bx==0, DOS MUST return error
        mov [ebp], bx
        shr ebx, 16
        and ebx, 0Fh
        ;or  bl, 40h                 ;32-bit selector
        mov [ebp + 6], bx
        mov byte [ebp + 5], 0F3h  ;data R/W PL3
        pop ebx
        pop eax
        ret


%ifndef NativeHost
        DPMIFn 1, 2
        call CheckSelectorInDX
        test bx, bx
        jnz DOSResizeSize@10
        dec bx                   ;the failure path probes and rolls back
DOSResizeSize@10:
        push eax
        call GetSegment@10
        push ecx
        mov ecx, esi
        shl ecx, 4
        movzx ecx, word [ecx - 13] ;DOS MCB's original paragraph count
        push ecx
        mov ah, 4Ah
        call Dos1Call
        jnc DOSResized@10
        cmp ax, 8
        jne DOSResizeFailed@10
        push eax
        push ebx
        mov bx, [esp + 8]         ;DOS can partially grow on error 8
        mov ah, 4Ah
        call Dos1Call
        pop ebx
        pop eax
DOSResizeFailed@10:
        add esp, 4
        pop ecx
        jmp DPMIError2N
DOSResized@10:
        add esp, 4
        pop ecx
        mov ax, si                ;use the block segment, not AX=0102h
        call SetupDOSSelector
        pop eax
        ret
DOSNoSelector@10:
        xor bx, bx
        mov ax, 8
        jmp DPMIError1N

        DPMIFn 1, 1
        mov si, dx
        xor si, [esp + DPMIFrame_SF + CFrSS]
        test si, ~3
        jnz DOSFreeStackOK@10
        mov ax, 9                 ;DOS invalid memory block, keep SS valid
        jmp DPMIError1N
DOSFreeStackOK@10:
        call CheckSelectorInDX
        push esi
        call GetSegment@10
        mov ebp, eax
        mov ah, 49h              ;Free block
        call Dos1Call
        jc DPMIError2N          ;@@Err101
        pop esi
        xchg eax, ebp
        lea ebp, [esi*8 + OffLDT]
        jmp FreeDescriptor1
; Only verified immutable DOS heads reach this conversion.
GetSegment@10:
        mov esi, [ebp + 2]
        shl esi, 8
        shr esi, 12
        ret

%endif
; call int 21h from PL1
; RM<->PM transfers:
; before call: si -> es, ax, bx
; after return: ax, bx
        DPROC Dos1Call
        ;push ebp
        ;mov  ebp, ds:[21h*4]        ;int 21
        push dword [(21h * 4)]
        call DosPCall
        ;pop  ebp
        ret
;call RM proc from PL1
;before call: si -> es, eax, ebx will be transferred to RM
;            ebp - rm proc far address
;after return: eax, ebx will be transferred from RM
;modify ebp
DosPCall:
        push esi
        push edi
        push ebp
        push fs
        push gs
%assign F 6 * 4 ;"pushad" + Dos1Call ret
        mov ebp, OffRMStack
        RRT
        mov edi, [OffTSS + TSS_ESP1]
        push edi                 ;save Kernel1Stack
%assign F F + 4
        push dword [edi - 8]      ;client ESP    ;setup client stack
        pop dword [ebp + OffPMStack - OffRMStack]
        push dword [edi - 4]      ;elient SS
        pop dword [ebp + OffPMStack - OffRMStack + 4]
        mov edi, [ebp]
        push edi                 ;save old real stack
%assign F F + 4
        movzx ebp, di
        shr edi, 16
        dec bp
        sub ebp, VMIStruct_size - 1
        ;jb   @@RStackOverflow
        push edi
        shl edi, 4
        add edi, ebp
        pop dword [edi + VMI_SS]
        add ebp, VMI_EAX
        mov [edi + VMI_ESP], ebp
        mov [edi + VMI_ES], esi     ;es
%ifdef VMM
          mov [edi + VMI_DS], esi     ;es
%endif
        mov [edi + VMI_EAX], eax
        mov eax, [esp + F]
        mov [edi + VMI_IP], eax
        db 0C7h, 47h, VMI_EndIP
        dw Dos1CallRetSwitchCode, DGROUP16
        ;clc           ;not needed: add ebp, VMI_EAX always clear cf
        pushfd
        pop eax
        mov [edi + VMI_Flags], ax
        mov [edi + VMI_EndFlags], ax
        mov [OffTSS + TSS_ESP1], esp
        mov eax, edi
        jmp SwitcherToVM

        LLabel Dos1CallRet1
        push ss
        pop ds
        push ss
        pop es
        pop dword [OffRMStack]
        RRT
        pop dword [OffTSS + TSS_ESP1]
        mov eax, [ebp + RMS_EAX]
        bt word [ebp + RMS_Flags], 0  ;copy carry flag from DOS to CF
        pop gs
        pop fs
        pop ebp
        pop edi
        pop esi
        ret 4


;simple jmp to fixed point at PL1
        DPROC Dos1CallRet
        push Data1Selector
        push dword [OffTSS + TSS_ESP1]
        push Code1Selector
        push OffDos1CallRet1
        retf                   ;retf to pl1

;bx - selector
;output: ebp -> pointer to descriptor or error handler call if selector incorrect
;destoy: esi
%ifndef NativeHost
CheckSelectorInDX:
        movzx esi, dx
        test si, 4
        jz DOSInvalid@10
        shr esi, 3
        bt dword [OffLDTBlock], esi
        jnc DOSInvalid@10
        lea ebp, [esi*8 + OffLDT]
        ret
DOSInvalid@10:
        mov ax, 9
        jmp DPMIError2N
%endif

        DPROC CheckSelector
        movzx esi, bx
CheckSelector1:
        test esi, 4
        jz DPMIError2InvSel       ;only LDT entries are writable by clients
        mov ebp, esi
        shr ebp, 3
        bt dword [OffLDTDOS], ebp
        jc DPMIError2InvSel
        bt dword [OffLDTBlock], ebp
        jc DPMIError2InvSel
        jmp short ReadLDT@10

        DPROC CheckSelectorRead
        movzx esi, bx
CheckSelectorRead1:
        ;xor  esi, 7
        test esi, 4
        jnz ReadLDT@10
        mov ebp, esi
        and ebp, ~7
        cmp ebp, OffGDTEnd - OffGDT
        jae DPMIError2InvSel
        add ebp, OffGDT
        movzx esi, byte [ebp + 5]
        and esi, 70h
        cmp esi, 70h              ;only client-accessible code/data, not gates
        jne DPMIError2InvSel
        ret
ReadLDT@10:
        shr esi, 3
        bt dword [OffLDTFree], esi
        jnc DPMIError2InvSel
        lea ebp, [esi*8 + OffLDT]
        ret


;stack - number of selectors to allocate
;return: ax - first allocated selector, ebp - pointer to descriptor
;esi - index of descriptor
        DPROC AllocSelectors
%define _t ebp
%define _tw bp
%define _t1 esi
        cmp word [esp + 4], 1   ;Error if less then 1 selector requred
        mov al, 21h
        jb near Err@10
        mov al, 11h
        mov _t1, 16 - 1
        _do
NextScan@10:
        inc _t1
        cmp _t1, 2000h
        jae near Err@10
        bt dword [OffLDTFree], _t1
        _enddo jc
        movzx _t, word [esp + 4]   ;Count
        _do jmp
        cmp _t1, 2000h
        jae near Err@10
        bt dword [OffLDTFree], _t1
        jc NextScan@10
        _while
        inc _t1
        dec _t
        _enddo jnz
;interval found
SelectorsFound@10:
        lea _t, [(_t1 * 8) - 1] ;inclusive end; 8192 entries end at FFFFh
        sub _tw, word [OffLDTLimit]
        _ifnot jbe
          push _t
          push _t
          movzx _t, word [OffLDTLimit]
          add _t, OffLDT
          push _t
          push 0
          call AllocPages
          _ifnot jnc
            add esp, 4
            jmp Err@10
          _endif
          pop _t
          add word [OffLDTLimit], _tw
          mov _tw, LDTSelector
          db CallFarCode
          dd 0
          dw LoadLDTGateSelector
        _endif
        movzx _t, word [esp + 4]
        _do
          dec _t1
          bts dword [OffLDTFree], _t1
          call InitOneSelector
          dec _t
        _enddo jnz
        lea ebp, [(_t1 * 8) + 7]
        mov ax, bp
        lea ebp, [ds:ebp + OffLDT - 7]   ;ebp - pointer to first descriptor
        clc
ret@10:
        ret 4
Err@10:
        stc
        jmp ret@10


        DPMIFn 2, 0
        movzx ebp, bl
        mov cx, [ebp*4 + 2]
        mov dx, [ebp*4 + 0]
        FnRet
;set real mode vector
        DPMIFn 2, 1
        movzx ebp, bl
        mov [ebp*4 + 2], cx
        mov [ebp*4], dx
        FnRet
;get exception handler
        DPMIFn 2, 2
        movzx ebp, bl
        cmp bl, 32
        jae DPMIError1InvVal
        lea ebp, [ds:ebp*8 + OffClientExc]
        mov edx, [ebp]
        mov cx, [ebp + 4]
        FnRet
;set exception handler
        DPMIFn 2, 3
        cmp bl, 32
        jae DPMIError1InvVal
        call CheckSelectorInCX@10
        movzx ebp, bl
        lea ebp, [ds:ebp*8 + OffClientExc]
        mov [ebp], edx
        mov [ebp + 4], cx
        FnRet
;get PM interrupt vector
        DPMIFn 2, 4
        movzx ebp, bl
        shl ebp, 3
        mov edx, [ebp + OffClientIDT]
        mov cx, [ebp + OffClientIDT + 4]
        FnRet
;set PM interrupt vector
;Get RM mapped vector for this number
;if it in AutoPassup:
;  check handler address for default, if client:
;    set CallMethodFlag
;    replace RM passup code to call PM trap with apropriate switch code
;  if default:
;    clear CallMethodFlag
;    replace RM passup code to far jmp to prevision RM handler
        DPMIFn 2, 5
        call CheckSelectorInCX@10
        movzx ebp, bl
        bt dword [OffPassupIntMap], ebp
        _ifnot jnc
        movzx esi, byte [ebp*4 + OffFirstTrap3 + OffDefIntTrap3 + 3] ;load linked RM int number
        bt dword [OffPassupIntMap], esi      ;
        _ifnot jnc
          push eax
          push edi
          xor eax, eax
          mov edi, esi             ;calculate relative index of passup
          _do
            bt dword [OffPassupIntMap], edi
            adc eax, 0
            dec edi
          _enddo jns
          lea edi, [eax + eax*4 + OffAutoPassupRJmps - 5]
          RRT
%ifdef NativeHost
          cmp cx, [OffNativeThunkSel]
          jne .old_defaults
          cmp edx, 256 * 4
          jb SetDefaultVect@10
.old_defaults:
%endif
          cmp cx, Trap3Selector
          jne SetClientVect@10
          cmp edx, 400h            ;edx points to Trap3 call gate ?
          jb SetDefaultVect@10
SetClientVect@10:
          mov byte [edi], PushWCode
          mov byte [edi + 3], JmpShortCode
          lea eax, [eax + eax*4 + OffAutoPassupRJmps - OffRMS_RMHandler]
          neg eax
          mov [edi + 4], al
          lea eax, [ds:ebp + FirstPassupSwitchCode + OffFirstSwitchCode + 3]
          mov [edi + 1], ax
          bts dword [OffRIntFlags], esi  ;set flag for routing to prevision RM vector
          _ifnot jmp
SetDefaultVect@10:
          mov byte [edi], JmpFarCode
          mov eax, [esi*4 + OffSavedRealVectors]
          mov [edi + 1], eax
          btr dword [OffRIntFlags], esi  ;clear flag for routing to current RM vector
          _endif
          pop edi
          pop eax
        _endif
        _endif
        shl ebp, 3
        mov [ebp + OffClientIDT], edx
        mov [ebp + OffClientIDT + 4], cx
%ifndef NativeHost
        cmp ebp, 7 * 8   ;legacy fast FPU software interface
        je SetInt7@10
%endif
        test byte [ebp + OffIDT + 2], 10b  ;PL0/1 handler installed ?
        _ifnot jz                          ;else set in IDT
SetInt7@10:
%ifdef NativeHost
          cmp byte [OffNativeMode], 0
          jne .wide_gate
          and byte [ebp + OffIDT + 5], ~8
.wide_gate:
%endif
          mov [ebp + OffIDT + 2], cx
          mov [ebp + OffIDT], dx
          shld esi, edx, 16               ;place high 16 bits of edx to si
          mov [ebp + OffIDT + 6], si
        _endif
        FnRet

        DPMIFn 9, 0
        DPMIFn 9, 1
        DPMIFn 9, 2
        lds esi, [esp + DPMIFrame_SF + CFrESP]
        mov ebp, [esi + 8]       ;return the previous IF in all three forms
        cmp al, 2
        je VIFRead@10
        shl al, 1               ;disable=0, enable=IF bit in the high byte
        and byte [esi + 9], ~2
        or [esi + 9], al
VIFRead@10:
        shr ebp, 10
        setc al
        FnRet

        DPMIFn B, 0
        DPMIFn B, 1
        DPMIFn B, 2
        DPMIFn B, 3
        VCPITrap              ;debug operations share the existing ring-0 gate
        jc DPMIError1
        FnRet

        DPMIFn A, 0
        jmp UnsupportedCall ;no vendor extensions are implemented

        DPMIFn 6, 4
        xor bx, bx
        mov cx, 1000h
%ifndef VMM
  DPMIFn 6, 0
%endif
        DPMIFn 6, 1
        DPMIFn 6, 2
        DPMIFn 6, 3
        DPMIFn 7, 2
        DPMIFn 7, 3
        FnRet

        DPMIFn 5, 0
        pushad
        xor eax, eax
        dec eax
        mov ecx, 30h / 4
        cld
        rep stosd
        xor edx, edx
%ifdef VMM
        mov [es:edi - 30h + 20h], edx ;swap file size
%endif
        mov ax, 0DE03h      ;get VCPI free pages
        cmp [OffVCPIMemAvailable], dl
        _ifnot je
          VCPITrap
        _endif
        add edx, [OffnFreePages] ;add free pages in my pool
        add edx, [OffFreeXMSCount]
        RRT
%ifdef VMM
        mov eax, [OffTotalVCPIPages]
        add eax, [OffTotalXMSPages]
        RRT
%ifndef Release
          ;mov eax, 3300
          ;mov edx, 3300
          ;add  eax, 10000
          ;add  edx, 10000
%endif
        mov [es:edi - 30h + 18h], eax
        mov [es:edi - 30h + 0Ch], eax
        mov [es:edi - 30h + 4], edx ;maximum unlocked page allocation
        mov [es:edi - 30h + 8], edx ;maximum locked page allocation(same)
        mov [es:edi - 30h + 10h], edx
        mov [es:edi - 30h + 14h], edx
        mov [es:edi - 30h + 1Ch], edx
%endif
%ifdef VMM
        mov eax, edx
        shr eax, 10
        inc eax
        inc eax
        sub edx, eax
        _ifnot ja
          xor edx, edx
        _endif
        shl edx, 12         ;convert pages to bytes
        mov [es:edi - 30h], edx  ;maximum free block
%else
        ; Metadata occupies the permanently mapped kernel directory.
        ; Only a new MCB page consumes another physical page.
        cmp dword [OffFMemRover], 0
        jne .budget
        mov eax, [OffMCBVectorEnd]
        cmp eax, 800000h
        jae .empty
        test eax, 0FFFh
        jnz .budget
        dec edx
.budget:
        xor ebx, ebx
        test edx, edx
        jle .store
        mov ebp, edx
        mov esi, OffRootMCB
.gap:   mov eax, [esi + MCB_EndOffset]
        mov esi, [esi + MCB_Next]
        mov ecx, [esi + MCB_StartOffset]
        sub ecx, eax
        shr ecx, 12
        shr eax, 12
        push ebx
        mov ebx, ebp
        push eax
.table: jecxz .count
        mov edx, eax
        shr edx, 10
        cmp dword [OffnEntriesInTable + edx*4], 0
        jne .present
        dec ebx
        jz .count
.present:
        mov edx, eax
        or dx, 3FFh
        inc edx
        sub edx, eax
        cmp edx, ecx
        jbe .gap_limit
        mov edx, ecx
.gap_limit:
        cmp edx, ebx
        jbe .physical_limit
        mov edx, ebx
.physical_limit:
        add eax, edx
        sub ecx, edx
        sub ebx, edx
        jnz .table
.count: pop edx
        sub eax, edx
        pop ebx
        cmp eax, ebx
        jbe .next
        mov ebx, eax
.next:  cmp esi, OffRootMCB
        jne .gap
        shl ebx, 12
        jmp .store
.empty: xor ebx, ebx
.store: mov [es:edi - 30h], ebx
%endif
        popad
        FnRet


;get version
        DPMIFn 4, 0
        mov ax, 005Ah          ;minor 0.90, decimal 90 (DPMI 1.0 Appendix C)
        mov bx, 1              ;32-bit clients supported
        cmp word [OffVCPICallDesc + 2], VCPISelector + 8
        jne .v86                 ;VCPI remains active even when out of free pages
        or bl, 2               ;host uses real mode outside VCPI
.v86:   mov cl, [OffCPUType]
        mov dx, 870h
        FnRet

        DPMIFn 3, 6
        mov cx, RawSwitchCode
        mov edi, OffPMRawSwitchTrap3
L045@10:
        mov word [esp + DPMIFrame_ESI], Trap3Selector
        mov bx, DGROUP16
        FnRet
        DPMIFn 3, 5
%ifdef NativeHost
        mov ax, 16
%else
        mov ax, 12
%endif
        mov cx, ROffRMSaveState
        mov edi, OffPMSaveStateTrap3
%ifdef NativeHost
        cmp byte [OffNativeMode], 0
        jne L045@10
        mov edi, NativeState16 - NativeInt16
        mov bx, DGROUP16
        mov si, [OffNativeThunkSel]
        mov [esp + DPMIFrame_ESI], si
        FnRet
%endif
        jmp L045@10
        DPMIFn 3, 0
        DPMIFn 3, 1
        DPMIFn 3, 2
%define _csr esi
        lds _csr, [esp + DPMIFrame_SF + CFrESP]  ;client stack
        mov ebp, OffRMStack
        RRT
        sub _csr, RMIEStruct_size - 12
        mov [ebp + OffPMStack - OffRMStack], _csr
        mov [ebp + OffPMStack - OffRMStack + 4], ds
;        @@CS equ ds:[@@CSR]

;Save all PM registers on client stack
        mov [esi + RMIE_EDX], edx
        mov [esi + RMIE_EAX], eax
        mov edx, [esp + DPMIFrame_EBP] ;ebp, saved on current stack
        mov [esi + RMIE_EBX], ebx
        mov [esi + RMIE_EBP], edx
        mov [esi + RMIE_ECX], ecx
        mov edx, [esp + DPMIFrame_ESI] ;esi, saved on current stack
        push eax
%assign F 4
        mov [esi + RMIE_ESI], edx
        mov [esi + RMIE_EDI], edi            ;@@CS[26] - pointer to client DC_Struct
%ifdef NativeHost
        mov edx, [esp + F + DPMIFrame_OrigDI]
        mov [esi + RMIE_OriginalDI], edx ;keep high bits separate from a 16-bit pointer
%endif
        mov edx, [esp + F + DPMIFrame_DS]  ;@@CS[20] - saved client DS:ESI
        movzx ecx, cx
        mov [esi + RMIE_DS], dx
        mov ebp, [ebp]              ;RMStack->ebp
        mov [esi + RMIE_ES], es
        mov [esi + RMIE_FS], fs
        mov [esi + RMIE_GS], gs
        mov [esi + RMIE_RealStack], ebp
        mov dl, [ss:OffA20Reset]
        RRT
        mov [esi + RMIE_CS + 2], dl ;unused high CS byte: nested RM A20 policy
        shr al, 1
        sbb edx, edx
;calculate iret frame additional length(only for iret forms)

;free: eax, edx, esi, ebp
        inc edx       ;convert to 1/0
        mov esi, [es:edi + DC_SP]
        shl edx, 1
                      ;edx == 2 if al==(0|2)
;calculate RM stack frame lenght
        or esi, esi
        lea eax, [ecx*2 + edx + VMIStruct_size - 2 - 1] ;full stack frame length-1  -> ax
;replace kernel stack on client's if it is nonzero
        _ifnot jz
          mov ebp, esi
        _endif
;calculate RM stack linear address
        movzx esi, bp
        dec si
        sub esi, eax
        jb near RStackOverflow@10
        shr ebp, 16
        mov eax, ebp
        shl ebp, 4
        add ebp, esi
        mov [ebp + VMI_SS], eax   ;RM SS
        add esi, VMI_EAX
        mov [ebp + VMI_ESP], esi  ;RM ESP

;copy parameters
        _ifnot jcxz
          mov esi, [esp + F + DPMIFrame_SF + CFrESP]   ;reload client esp to esi
          push edi
          push es
          add esi, 12                            ;interrupt frame length
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
          ;  mov  ax, ds:[esi+ecx*2-2+12]    ;get parameter from client stack
          ;  mov  ss:[ebp+ecx*2-2+size VMIStruct-2], ax ;put it on the RM stack
          ;$enddo loop
          ;sub  ebp, edx
        _endif
;set flags value for RM iret, if requered
        or edx, edx          ;interrupt stack frame mode ?
        push es
        pop ds
        mov dx, [edi + DC_Flags]
        _ifnot jz
          mov [ebp + VMI_EndFlags], dx ;put flags to RM stack iret frame
          and dh, ~(3)       ;clear TF and IF for initial flags
        _endif
        pop eax               ;saved client eax
        mov [ebp + VMI_Flags], dx    ;set initial flags

        or al, al            ;Fn number == 0  ?
        mov edx, [edi + DC_IP]
        _ifnot jnz
          movzx edx, bl
          mov edx, [ss:edx*4]  ;call CS:IP from current interrupt vector for function 0 only
        _endif
        db 0C7h, 45h, VMI_EndIP
        dw RMIERetSwitchCode, DGROUP16

;ss:ebp - RM switch stack, ds:edi - dos call struct, edx - start address
        test bh, 1
        jz SwitchToVMWithTransfer
        mov byte [ss:OffA20Reset], 1 ;nested calls retain the outer RM policy
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
          jb DPMIError1                    ;free callback not found
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
        lea esi, [esi + esi*2 - ((OffFirstSwitchCode + MaxSystemSwitchCode) * 3)]
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
        inc _firstdi                ;share the normal block's boundary table
MapTables@12:
        push _firstdi               ;first newly owned directory for rollback
        _do jmp
%ifndef VMM
          lea edi, [OffPageDir + (_firstdi * 4)]
          mov ecx, _lastdi
          sub ecx, _firstdi
          call XAllocPages
          jz Recover@12
          add _firstdi, ecx
%else
          push eax
          call alloc_page
          xchg edi, eax
          pop eax
          jc Recover@12
          mov [OffPageDir + (_firstdi * 4)], edi
          inc _firstdi
%endif
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
        or _mapregionend, _t ;compare matching PTE attributes, including PCD
        shr _tmappedpage, 12
MapDirectory@12:
        mov _t, _tmappedpage
        shr _t, 10
        cli
        mov edi, [OffPageDir + _t*4]
        mov [OffPage2 + ((OffPageTableWin - KernelBase) >> 10)], edi
        lea edi, [OffnEntriesInTable + _t*4]
        InvalidateTLB
        movzx edx, si
        and dh, 3
MapEntries@12:
%ifndef VMM
        inc dword [edi]
%endif
        mov [OffPageTableWin + _t*4], _mapregionstart
        add _mapregionstart, 1000h
        inc _tmappedpage
        inc _t
        cmp _mapregionend, _mapregionstart
        je MapComplete@12
        test dh, 4               ;1024 entries: reload the window once per PDE
        jz MapEntries@12
        sti                      ;IRQs may reuse the window between directories
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
%ifdef VMM
        shr ebx, 22
.loop:  cmp ebx, edx
        jae .done
        push dword [OffPageDir + ebx*4]
        call free_page
        inc ebx
        jmp .loop
.done:  ret
%else
        mov eax, ebx
        shr eax, 12
        shl edx, 10
        push 2                     ;shared page walker, no physical release/IRQ
        call FreePagesN
        ret
%endif

; New mapping tables have no installed entries yet; rollback frees only
; directories allocated by this call, excluding an existing boundary.
FreeNewMapTables:
.loop:  cmp ebx, edx
        jae .done
%ifndef VMM
        push ebx
        push edx
        lea esi, [OffPageDir + ebx*4]
        mov ecx, 1
        call XFreePages
        pop edx
        pop ebx
%else
        push dword [OffPageDir + ebx*4]
        call free_page
%endif
        inc ebx
        jmp .loop
.done:  ret

        ESEG Text
