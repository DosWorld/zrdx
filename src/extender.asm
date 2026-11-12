;             This file is part of the ZRDX OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda


;NOWARN Res
;DataPtr EQU ss:[ebp]
;WARN Res

NHookedInterrupts equ 4
%assign p_ds 200h
%assign p_es 0
p_ax equ DC_EAX
p_cx equ DC_ECX
%assign p_dx DC_EDX
%assign p_bx DC_EBX
%assign p_bp DC_EBP
%assign p_si DC_ESI
%assign p_di DC_EDI
%assign p_dsdx p_ds + p_dx
%assign p_dsbx p_ds + p_bx
%assign p_esbx p_es + p_bx
%assign p_esdi p_es + p_di
%assign p_esdx p_es + p_dx
%assign p_dssi p_ds + p_si
p_esbp equ p_es + p_bp

        SEGM EData
        LDWord ExcReturnAddr
       dd ExcHandler2Entry
        LDWord ClientInt0Vector
       dd OffDefaultInt0Handler
       dd 0 ;must be filled with cs
;StackLo DD OffxStack
;SvStack DD OffxStackEnd
        LDWord FlatSelector
       dd 0
        LDWord SavedPSP
       dd 0
        LDWord TransferStack
        ; current pointer to top of the transfer stack
        dd -(DTASize)
        LWord TransferSelector        ;selector of transfer stack
        dw 0, 0
TaskDTA: dd OffDefaultDTA ;pointer to current client DTA
        dw 0, 0
        LDWord TransferDTA
        dd -(DTASize) ;pointer to DTA in RM area, reported to DOS
        LDWord MouseHookProc
        dd DefMouseHookProc, 0 ;offset and segment   DD ?
; address returned by mouse on exchange vectors
HookFromMouse32: dd 0
%ifndef Release
OutPos: dd 0 ; 0B8000h+160*10  ;DEBUG only
%endif
        ; RM segment of the transfer stack
        LWord TransferSegment
        dw 0
        LByte OrigVideoMode
        db 3
        ESEG EData
;assume ds:EGroup, ss:nothing, cs:ETextG
        SEGM EText
TTT:
        ;push eax es
        ;mov es, FlatSelector
        ;mov eax, OutPos
        ;or  eax, eax
        ;$ifnot jz
        ;mov word ptr es:[eax], 1F00h+'C'
        ;add OutPos, 2
        ;$endif
        ;pop es eax
        ;retn

;EIntDescr struc
EID_FirstJmp equ 0
EID_OldIntVect equ 4
EID_OldIntVectHi equ 8
;reserved for alignment
EID_IntNum equ 11
EIntDescr_size equ 12
        SEGM EData
EID0:
EID33: dd Int33
        dd 0
        dw 0
        db 0
        db 33h
EID10: dd Int10
        dd 0
        dw 0
        db 0
        db 10h
EID21: dd Int21
        dd 0
        dw 0
        db 0
        db 21h
EID2F: dd Int2F
        dd 0
        dw 0
        db 0
        db 2Fh
        ESEG EData
        LLabel FirstExtHandler
$FirstExtHandler equ $
        pushfd
        push EID33
        jmp short IntEntry
ExtHandlerStep equ $ - $FirstExtHandler

        pushfd
        push EID10
        jmp short IntEntry

        pushfd
        push EID2F
        jmp short IntEntry

int21entry:
        pushfd
        push EID21
IntEntry:
        sub esp, DC_EID - DC_EAX - 4
        pushad
        mov ebp, esp
%ifdef D4G
        mov edi, LGROUP
%else
        mov edi, 0
        LLabel SelReference0
%endif
        mov [ebp + DC_DS0], ds
        mov ds, edi
        mov esi, [ebp + DC_EID]
        jmp [esi + EID_FirstJmp]
;new entry
Int33:
        or ah, ah
        jnz near ToOldInt0
        mov edi, int33Table
        jmp CallMethod
Int10:
        mov edi, int1010Table
%ifndef Release
        cmp ah, 0
        je rrrr1
        cmp ax, 4f02h      ;ignore vesa set videomode
        _ifnot jne
         mov dword [ebp + EAX], 4Fh
         jmp rrrr1
        _endif
%endif
        cmp ah, 10h
        je CallMethod
        mov edi, int104FTable
        cmp ah, 4Fh
        je CallMethod
        cmp ax, 1130h
        jne near ToOldInt0
        db 0B8h
        dw p_esbp, r_ptr
        jmp CallMethod1
Int2F:
%ifdef VENDORON
        cmp ax, 168Ah
        je near Vendor168A
%endif
        cmp ax, 1686h
        jne Int2FClipboard
        mov word [ebp + DC_EAX], 0
        jmp rrrr1
Int2FClipboard:
        cmp ah, 17h
        jne near ToOldInt0
        mov edi, int2F17Table
        jmp CallMethod
Int21:
        cmp eax, 0FF00h
        je GetExtVer?
        mov edi, int2144Table
        cmp ah, 44h
        je CallMethod
        mov edi, int2171Table
        cmp ah, 71h
        je CallMethod
        mov edi, int21Table
CallMethod3:
        mov al, ah
CallMethod:
        cmp al, [edi]
        ja ToOldInt0
        movzx eax, al
        mov al, [edi + eax + 1]
        shl eax, 1
        jz ToOldInt0
        mov eax, [eax + MethodTable0 - 4]
CallMethod1:
        mov edi, [ebp + DC_REFLAGS]     ;saved eflags
        mov ebx, eax
        mov [ebp + DC_Flags], edi
        shr eax, 16
        xor edi, edi
        mov [ebp + DC_FS0], fs
        mov [ebp + DC_ES0], es
        mov [ebp + DC_ES], edi
        mov [ebp + DC_FS], edi
        cld
%ifdef D4G
        add eax, base_fn_addr
%endif
        call eax
        mov al, [ebp + DC_Flags]
        mov es, [ebp + DC_ES0]
        mov fs, [ebp + DC_FS0]
        mov [ebp + DC_REFLAGS1], al
rrrr1:
        mov ds, [ebp + DC_DS0]
        popad
        add esp, DC_REIP1 - DC_EAX - 4
        iretd
base_fn_addr:
GetExtVer?:
        cmp dx, 78h
        jne ToOldInt0
        mov dword [ebp + DC_EAX], 0FFFF3447h
        jmp rrrr1
; ------------------------------ int 4Ch
; -------------------------------------
;free all callback handlers
ExitDPMI:
        pushad
        mov eax, 0900h
        int 31h
        push eax
        mov edi, EID0
.restore:
        movzx ebx, byte [edi + EID_IntNum]
        mov cx, [edi + EID_OldIntVectHi]
        mov edx, [edi + EID_OldIntVect]
        mov eax, 0205h
        int 31h
        add edi, EIntDescr_size
        cmp edi, EID0 + NHookedInterrupts * EIntDescr_size
        jb .restore
        movzx ebx, byte [OffOldVectors + 6]
        mov cx, [OffOldVectors + 4]
        mov edx, [OffOldVectors]
        mov eax, 0205h
        int 31h
        pop eax
        test al, al
        jz .disabled
        mov eax, 0901h
        int 31h
.disabled:
        popad
        mov edx, 0
        LBack MouseCallbackPlace1, 4
        shld ecx, edx, 16
        mov ax, 304h
        int 31h
;must save after call: esi, edx, ecx
ToOldInt1:
        pop eax      ;drop ret address
ToOldInt0:
        movzx edi, word [esi + EID_OldIntVectHi]
        mov esi, [esi + EID_OldIntVect]
        mov ds, [ebp + DC_DS0]
        mov [ebp + DC_REIP], esi
        mov [ebp + DC_RCS], edi
        popad
        add esp, DC_REIP - DC_EAX - 4
        iretd

;read the pointer
GetasciizLen:
        mov al, 0
GetasciiLen:
        movzx ecx, bh           ;DataPtr.DC_SegmentIndex
        mov es, [ebp + ecx + DC_FirstSR0]
        mov cl, bl              ;edi, DataPtr.DC_OffsetIndex
        mov edi, [ebp + ecx + DC_FirstR]
        mov ecx, -(1)
        repne scasb
        not ecx
        ret
GetAsciizLenPush:
        call GetasciizLen
;Entry: ecx - size, ???-index
; Exit:  edx-old transfer stack, fs:esi(ecx) - saved data, eax -
; size
;ecx - zero
PushStr:
        movzx eax, bh      ;, DataPtr.DC_SegmentIndex
        mov edi, [OffTransferSegment]
        and ebx, 7Fh
        mov [ebp + eax + DC_FirstSR], di
        les edi, [OffTransferStack]
        mov fs, [ebp + eax + DC_FirstSR0]
        mov edx, edi
        sub edi, ecx
        mov esi, [ebp + ebx]
        jb near TransferStackOverflow
        ; align new stack pointer on dword
        and edi, ~(3)
        mov eax, ecx
        mov [ebp + ebx + DC_FirstR], edi
        shr ecx, 2
        mov [OffTransferStack], edi
        rep fs movsd
        mov cl, al
        and cl, 3
        rep fs movsb
        sub esi, eax                   ;restore esi
        ret

ResStr:
        movzx edi, bh      ;, DataPtr.DC_SegmentIndex
        mov eax, [OffTransferSegment]
        and ebx, 7Fh
        mov [ebp + edi + DC_FirstSR], ax
        mov edx, [OffTransferStack]
        mov fs, [ebp + edi + DC_FirstSR0]
        sub edx, ecx
        jb near TransferStackOverflow
        and edx, -(4)
        mov esi, edx
        xchg esi, [ebp + ebx + DC_FirstR]
        mov eax, ecx
        mov [OffTransferStack], edx
        ret


wr_cx3: ;movzx ecx, word ptr DataPtr.DC_ECX
        imul ecx, 3
        jmp PushCallPop
wr_17:
        mov ecx, 17
        jmp PushCallPop
wr_bx:
        movzx ecx, word [ebp + DC_EBX]
        jmp PushCallPop
wr_64:
        mov ecx, 64
        jmp PushCallPop
wr_256:
        mov ecx, 256
        jmp PushCallPop
wr_ecx4:
        shl ecx, 2
        jmp PushCallPop
wr_pas:
        mov fs, [ebp + DC_DS0]
        movzx ecx, byte [fs:edx]
        inc ecx
        inc ecx
PushCallPop:
        call PushStr
CallPop:
        call DosCall
        jmp PopStr

r_createunic:
        call GetasciizLen
        add ecx, 13
;PushCallPopZEAX:
        call PushCallPop
ZeroEAX:
        mov [ebp + DC_EAX + 2], cx
        ret
w_asciizc:
        mov al, 0
        call w_ascii
        jmp ZeroEAX

;get directory
wr_64_de:
        mov ecx, 64
        call PushStr
        ;jmp  CallEPop
;ResCallEPop:
;        call ResStr
CallEPop:
        call DosCallErr
PopStr:
        mov ecx, fs
        mov edi, esi
        mov [ebp + ebx + DC_FirstR], esi
        mov es, ecx
        lfs esi, [OffTransferStack]
        mov ecx, eax
        shr ecx, 2
        and al, 3
        rep fs movsd
        mov cl, al
        rep fs movsb
        mov [OffTransferStack], edx
        ret

w_ascii$:
        mov al, '$'
w_ascii:
        call GetasciiLen
PushCallRest:
        call PushStr
CallRest:
        call DosCall
$Rest:
        mov [ebp + ebx + DC_FirstR], esi
Rest1:
        mov [OffTransferStack], edx
        ret

w_asciiz_de:
        call GetAsciizLenPush
DosCallERest:
        call DosCallErr
        jmp $Rest

w_asciiz_r_dta:
        call GetAsciizLenPush
        call DTACall
        jmp $Rest

;assume es:nothing
r_country:
        xor ecx, ecx
        cmp edx, -(1)
        _ifnot je
          mov cl, 34
          call PushStr
        _endif
        call DosCallErr
        _ifnot jnz
        or eax, eax
        _ifnot jz
          call PopStr
          mov [ebp + DC_EBX + 2], cx
        _endif
        ret
        _endif
        or eax, eax
        jne $Rest
        ret
w_asciizc_zc:
        cmp byte [ebp + DC_EAX + 0], 0
        jne ToOldInt1
        call w_asciizc
        jz z_c
        ret
r_ptr_zc:
        call r_ptr
z_c:
        mov [ebp + DC_ECX + 2], cx
        ret
dc_zc:
        call DosCall
        jmp z_c

onalnff_r_ptr_zcd:
        call DosCall
        cmp byte [ebp + DC_EAX], -(1)
        _ifnot je
        call ptr_cvt
        jmp z_cd
r_ptr_zcd:
        call r_ptr
z_cd:
        mov [ebp + DC_ECX + 2], cx
z_d:
        mov [ebp + DC_EDX + 2], cx
        _endif
        ret
lseek_:
        call DosCall
        mov [ebp + DC_EAX + 2], cx
        jz z_d
        ret
zd_de:
        call DosCallErr
        jz z_d
        ret

;if(ax ne -1) movzx eax,ebx,ecx,edx
;else mov eax, -1
onaxnff_zabcd:
        call DosCall
        dec ecx
        cmp [ebp + DC_EAX], cx
        _ifnot je
        inc ecx
        mov [ebp + DC_EAX + 2], cx
        mov [ebp + DC_EBX + 2], cx
        jmp z_cd
        _endif
        mov [ebp + DC_EAX], ecx
        ret

file_attrs:
        push dword [ebp + DC_EAX]
        call w_asciiz_de
        pop eax
        _ifnot jnz
        or al, al
        jz z_c
        _endif
        ret

w_cx_za:
        call PushCallRest
        jmp za_
r_cx_za:
        call ResStr
        call CallPop
        jmp za_
;call dos then movzx eax
dc_za:
        call DosCall
za_:
        mov [ebp + DC_EAX + 2], cx
        ret

; --------------------- back pointers convertors group
; -----------------------

;call dos and convert ptr if al eq 0
onal0_r_ptr:
        call DosCall
        cmp [ebp + DC_EAX], cl
        je ptr_cvt
        ret

;entry: ebx - pointer descriptor
;exit:  ecx == 0
r_ptr:
        call DosCall
ptr_cvt:
        movzx edi, bh           ;DataPtr.DC_SegmentIndex
        mov eax, [OffFlatSelector]
        and ebx, 7Fh
        mov [ebp + edi + DC_FirstSR0], ax
        movzx eax, word [ebp + edi + DC_FirstSR]
        shl eax, 4
        movzx esi, word [ebp + ebx + DC_FirstR]
        add eax, esi
        mov [ebp + ebx + DC_FirstR], eax
        ret

; -------------------- special routine for VBE 4F00h
; -------------------------
        SEGM EData
        LByte DispTable4F00
      db 6, 0Eh, 16H, 1Ah, 1Eh
        ESEG EData
VBE4F00:
        xor ecx, ecx
        mov edi, [ebp + DC_EDI]
        mov ch, 1
        cmp dword [es:edi], '2EBV'
        _ifnot jne
          mov ch, 2
        _endif
        call PushCallPop
        cmp word [ebp + DC_EAX + 0], 4Fh
        _ifnot jne
        mov esi, OffDispTable4F00
        ;mov  ecx, 5
        mov cl, 5       ;size of DispTable
        mov edi, [ebp + DC_EDI]
        xor eax, eax
        _do
        lodsb
        movzx edx, word [es:edi + eax]
        movzx ebx, word [es:edi + eax + 2]
        shl ebx, 4
        add edx, ebx
        mov [es:edi + eax], edx
        _enddo loop
        _endif
        ret
; ----------------- call dos with DTA instread of transfer stack
; -------------
DTACall:
        push esi
        lfs esi, [TaskDTA]
        mov edi, [OffTransferDTA]
        mov es, [OffTransferSelector]
        mov ecx, 43
        rep fs movsb
        lea esi, [edi - 43]
        call DosCallErr
        les edi, [TaskDTA]
        mov fs, [OffTransferSelector]
        mov ecx, 43
        rep fs movsb
        pop esi
        ret
; --------------------------- call dos and clear eah if
; CY--------------------
DosCallErr:
        call DosCall
        _ifnot jz
          mov [ebp + DC_EAX + 2], cx
        _endif
        ret
; ----------------------------- call dos simple
; ------------------------------
;preserve eax, ebx, edx, ebp, esi
DosCall:
        push eax
        push ebx
        mov eax, ss
        mov ebx, [ebp + DC_EID]
        mov edi, ebp
        mov es, eax
        xor ecx, ecx
        mov eax, 300h
        mov [ebp + DC_SP], ecx
        movzx ebx, byte [ebx + EID_IntNum]
        ;sub  esp, 1000h
        int 31h
        ;add  esp, 1000h
        pop ebx
        pop eax
        test byte [ebp + DC_Flags], 1
        ret

; --------------------------------- get psp
; ----------------------------------
r_psp:
        mov eax, [OffSavedPSP]
        mov [ebp + DC_EBX], eax
        ret
        SEGM EData
%macro LfnOpd 4
        db %1, %2, %3
        dw %4
%endmacro
        LLabel LfnSpNone
        db 0
        LLabel LfnSpName
        LfnOpd 1, DC_EDX, 2, 0
        db 0
        LLabel LfnSpCurDir
        LfnOpd 3, DC_ESI, 2, 264
        db 0
        LLabel LfnSpFind1
        LfnOpd 1, DC_EDX, 2, 0
        LfnOpd 2, DC_EDI, 0, 318
        db 0
        LLabel LfnSpFind2
        LfnOpd 2, DC_EDI, 0, 318
        db 0
        LLabel LfnSpRename
        LfnOpd 1, DC_EDX, 2, 0
        LfnOpd 1, DC_EDI, 0, 0
        db 0
        LLabel LfnSpTrue
        LfnOpd 1, DC_ESI, 2, 0
        LfnOpd 3, DC_EDI, 0, 264
        db 0
        LLabel LfnSpOpen
        LfnOpd 1, DC_ESI, 2, 0
        db 0
        LLabel LfnSpVol
        LfnOpd 1, DC_EDX, 2, 0
        LfnOpd 5, DC_EDI, 0, 0
        db 0
        LLabel LfnSpInfo
        LfnOpd 2, DC_EDX, 2, 52
        db 0
        LLabel LfnSpTimeIn
        LfnOpd 4, DC_ESI, 2, 8
        db 0
        LLabel LfnSpTimeOut
        LfnOpd 2, DC_EDI, 0, 8
        db 0
        LLabel LfnSpShort
        LfnOpd 1, DC_ESI, 2, 0
        LfnOpd 3, DC_EDI, 0, 16
        db 0
        LLabel LfnSpFcb
        LfnOpd 1, DC_ESI, 2, 0
        LfnOpd 2, DC_EDI, 0, 11
        db 0
        LLabel LfnSpSubst
        LfnOpd 3, DC_EDX, 2, 264
        db 0
        ESEG EData
LfnOp:
        movzx eax, byte [esi]
        or eax, eax
        jz DosCall
        sub esp, 28
        mov [esp + 20], esi
        movzx ebx, word [esi + 1]
        mov [esp + 16], ebx
        movzx ecx, word [esi + 3]
        mov [esp + 12], eax
        cmp eax, 1
        _ifnot jne
          call GetasciizLen
        _else jmp
          cmp eax, 5
          _ifnot jne
            movzx ecx, word [ebp + DC_ECX]
            cmp ecx, 64
            _ifnot jbe
              mov ecx, 64
              mov [ebp + DC_ECX], cx
            _endif
          _endif
        _endif
        mov [esp + 8], ecx
        movzx eax, bh
        mov edi, [OffTransferSegment]
        mov [ebp + eax + DC_FirstSR], di
        and ebx, 7Fh
        mov esi, [OffTransferStack]
        mov edi, esi
        sub edi, ecx
        jb near TransferStackOverflow
        and edi, ~(3)
        mov [esp + 0], esi
        mov [esp + 24], edi
        mov [OffTransferStack], edi
        mov edx, [ebp + ebx]
        mov [esp + 4], edx
        mov [ebp + ebx], edi
        mov eax, [esp + 12]
        cmp eax, 1
        je lfn_copyin
        cmp eax, 4
        jne lfn_nocopyin
lfn_copyin:
        movzx eax, byte [esp + 17]
        mov fs, [ebp + eax + DC_FirstSR0]
        mov esi, [esp + 4]
        les edi, [OffTransferStack]
        mov ecx, [esp + 8]
        call MoveStr
lfn_nocopyin:
        mov esi, [esp + 20]
        add esi, 5
        call LfnOp
        test byte [ebp + DC_Flags], 1
        jnz lfn_restore
        mov eax, [esp + 12]
        cmp eax, 2
        je lfn_copyback
        cmp eax, 3
        je lfn_copyz
        cmp eax, 5
        jne lfn_restore
lfn_copyz:
        mov fs, [OffTransferStack + 4]
        mov esi, [esp + 24]
        mov ecx, [esp + 8]
        xor edx, edx
        _do
          cmp byte [fs:esi + edx], 0
          _break je
          inc edx
          cmp edx, ecx
        _enddo jb
        lea ecx, [edx + 1]
        cmp ecx, [esp + 8]
        _ifnot jbe
          mov ecx, [esp + 8]
        _endif
        jmp lfn_copy
lfn_copyback:
        mov ecx, [esp + 8]
lfn_copy:
        movzx eax, byte [esp + 17]
        mov es, [ebp + eax + DC_FirstSR0]
        mov edi, [esp + 4]
        mov fs, [OffTransferStack + 4]
        mov esi, [esp + 24]
        call MoveStr
lfn_restore:
        movzx ebx, byte [esp + 16]
        and ebx, 7Fh
        mov eax, [esp + 4]
        mov [ebp + ebx], eax
        mov eax, [esp + 0]
        mov [OffTransferStack], eax
        add esp, 28
        ret
lfn_name:
        mov esi, OffLfnSpName
        jmp LfnOp
lfn_curdir:
        mov esi, OffLfnSpCurDir
        jmp LfnOp
lfn_find1:
        mov esi, OffLfnSpFind1
        jmp LfnOp
lfn_find2:
        mov esi, OffLfnSpFind2
        jmp LfnOp
lfn_rename:
        mov esi, OffLfnSpRename
        jmp LfnOp
lfn_true:
        mov esi, OffLfnSpTrue
        jmp LfnOp
lfn_open:
        mov esi, OffLfnSpOpen
        jmp LfnOp
lfn_vol:
        mov esi, OffLfnSpVol
        jmp LfnOp
lfn_info:
        mov esi, OffLfnSpInfo
        jmp LfnOp
lfn_time:
        mov esi, OffLfnSpNone
        cmp byte [ebp + DC_EBX], 0
        _ifnot jne
          mov esi, OffLfnSpTimeIn
        _endif
        cmp byte [ebp + DC_EBX], 1
        _ifnot jne
          mov esi, OffLfnSpTimeOut
        _endif
        jmp LfnOp
lfn_short:
        mov esi, OffLfnSpShort
        cmp byte [ebp + DC_EDX + 1], 0
        _ifnot jne
          mov esi, OffLfnSpFcb
        _endif
        jmp LfnOp
lfn_subst:
        mov esi, OffLfnSpNone
        cmp byte [ebp + DC_EBX + 1], 0
        _ifnot jne
          mov esi, OffLfnSpName
        _endif
        cmp byte [ebp + DC_EBX + 1], 2
        _ifnot jne
          mov esi, OffLfnSpSubst
        _endif
        jmp LfnOp
clip_plain:
        jmp DosCall
ClipSize:
        movzx ecx, word [ebp + DC_ESI]
        shl ecx, 16
        mov cx, [ebp + DC_ECX]
        ret
ClipAlloc:
        cmp ecx, 0A0000h
        ja ca_err
        push ecx
        add ecx, 15
        shr ecx, 4
        mov ebx, ecx
        mov ax, 100h
        int 31h
        pop ecx
        ret
ca_err:
        stc
        ret
clip_set:
        call ClipSize
        jecxz clip_plain
        call ClipAlloc
        _ifnot jnc
          mov word [ebp + DC_EAX], 0
          ret
        _endif
        push edx
        push dword [ebp + DC_EBX]
        push ecx
        movzx eax, ax
        push eax
        mov es, edx
        mov fs, [ebp + DC_ES0]
        mov esi, [ebp + DC_EBX]
        xor edi, edi
        call MoveStr
        pop eax
        pop ecx
        movzx ebx, word [ebp + DC_EDX]
        cmp ebx, 2
        je clip_set_bm
        cmp ebx, 82h
        jne clip_set_call
clip_set_bm:
        cmp ecx, 12h
        jb clip_set_call
        mov word [es:0Ah], 12h
        mov [es:0Ch], ax
clip_set_call:
        mov [ebp + DC_ES], ax
        mov word [ebp + DC_EBX], 0
        call DosCall
        pop dword [ebp + DC_EBX]
        pop edx
        mov ax, 101h
        int 31h
        ret
clip_get:
        push dword [ebp + DC_EAX]
        push dword [ebp + DC_EDX]
        push dword [ebp + DC_EBX]
        mov word [ebp + DC_EAX], 1704h
        call DosCall
        movzx ecx, word [ebp + DC_EDX]
        shl ecx, 16
        mov cx, [ebp + DC_EAX]
        test ecx, ecx
        jz near clip_get_none
        call ClipAlloc
        jc clip_get_none
        push edx
        push ecx
        movzx eax, ax
        push eax
        mov ebx, [esp + 16]
        mov [ebp + DC_EDX], ebx
        mov [ebp + DC_ES], ax
        mov word [ebp + DC_EBX], 0
        mov word [ebp + DC_EAX], 1705h
        call DosCall
        pop eax
        pop ecx
        pop edx
        cmp word [ebp + DC_EAX], 0
        je clip_get_free
        mov fs, edx
        mov es, [ebp + DC_ES0]
        mov edi, [esp]
        xor esi, esi
        push edx
        push ecx
        call MoveStr
        pop ecx
        pop edx
        movzx eax, word [esp + 4]
        cmp eax, 2
        je clip_get_bm
        cmp eax, 82h
        jne clip_get_free
clip_get_bm:
        cmp ecx, 12h
        jb clip_get_free
        mov ebx, [esp]
        lea eax, [ebx + 12h]
        mov [es:ebx + 0Ah], ax
        mov ax, [ebp + DC_ES0]
        mov [es:ebx + 0Ch], ax
clip_get_free:
        mov ax, 101h
        int 31h
clip_get_end:
        pop dword [ebp + DC_EBX]
        pop dword [ebp + DC_EDX]
        add esp, 4
        ret
clip_get_none:
        mov word [ebp + DC_EAX], 0
        jmp clip_get_end
; ----------------------------- memory procedures
; ----------------------------
reallocmem:
DPMIMemCall:
        xchg eax, ebx             ;load function number to ax
        mov dx, [ebp + DC_ES0]
        mov ebx, [ebp + DC_EBX]
        and byte [ebp + DC_Flags], ~(1)  ;clear carry
        int 31h
        _ifnot jnc
          movzx eax, ax
          or byte [ebp + DC_Flags], 1  ;set carry
          mov [ebp + DC_EAX], eax
          cmp al, 8
          _ifnot jne
            movzx ebx, bx
            mov [ebp + DC_EBX], ebx
          _endif
          stc
        _endif
        ret
allocmem:
        call DPMIMemCall
        _ifnot jc
          movzx edx, dx
          mov [ebp + DC_EAX], edx
        _endif
        ret
freemem:
        call DPMIMemCall
        _ifnot jc
          mov word [ebp + DC_ES0], 0
        _endif
        ret

%include "extende2.asm"
