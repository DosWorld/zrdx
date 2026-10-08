;             This file is part of the ZRDX 0.50OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda


;NOWARN Res
;DataPtr EQU ss:[ebp]
;WARN Res

NHookedInterrupts equ 3
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
        dd -(DTASize) ;current pointer to top of the transfer stack
        LWord TransferSelector        ;selector of transfer stack
        dw 0, 0
TaskDTA: dd OffDefaultDTA ;pointer to current client DTA
        dw 0, 0
        LDWord TransferDTA
        dd -(DTASize) ;pointer to DTA in RM area, reported to DOS
        LDWord MouseHookProc
        dd DefMouseHookProc, 0 ;offset and segment   DD ?
HookFromMouse32: dd 0 ;address returned by mouse on exchange vectors
%ifndef Release
OutPos: dd 0 ; 0B8000h+160*10  ;DEBUG only
%endif
        LWord TransferSegment             ;RM segment of the transfer stack
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
Int21:
        cmp eax, 0FF00h
        je GetExtVer?
        mov edi, int2144Table
        cmp ah, 44h
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
;------------------------------ int 4Ch -------------------------------------
;free all callback handlers
ExitDPMI:
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
;Exit:  edx-old transfer stack, fs:esi(ecx) - saved data, eax - size
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
        and edi, ~(3)                 ;align new stack pointer on dword
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

;--------------------- back pointers convertors group -----------------------

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

;-------------------- special routine for VBE 4F00h -------------------------
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
;----------------- call dos with DTA instread of transfer stack -------------
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
;--------------------------- call dos and clear eah if CY--------------------
DosCallErr:
        call DosCall
        _ifnot jz
          mov [ebp + DC_EAX + 2], cx
        _endif
        ret
;----------------------------- call dos simple ------------------------------
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

;--------------------------------- get psp ----------------------------------
r_psp:
        mov eax, [OffSavedPSP]
        mov [ebp + DC_EBX], eax
        ret
;----------------------------- memory procedures ----------------------------
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

;--------------------------- interrupt vectors get/set ------------------------
w_vect:
        mov al, [ebp + DC_EAX]
        movzx ecx, word [ebp + DC_DS0]
        or al, al
        _ifnot jne
          mov [OffClientInt0Vector + 4], ecx
          mov [OffClientInt0Vector + 0], edx
          ret
        _endif
DPMIVectCall:
        xchg eax, ebx
        and byte [ebp + DC_Flags], ~(1)  ;clear carry
        int 31h
        _ifnot jnc
          or byte [ebp + DC_Flags], 1      ;set carry
        _endif
        ret
r_vect:
        mov al, [ebp + DC_EAX]
        or al, al
        _ifnot jne
          mov ecx, [OffClientInt0Vector + 4]
          mov edx, [OffClientInt0Vector + 0]
        _else jmp
          call DPMIVectCall
        _endif
        mov [ebp + DC_ES0], cx
        mov [ebp + DC_EBX], edx
        ret
;----------------------------- get and set DTA ------------------------------
GetDTA:
        mov eax, [TaskDTA]
        mov [ebp + DC_EBX], eax
        mov eax, [TaskDTA + 4]
        mov [ebp + DC_ES0], ax
        ret
SetDTA:
        mov eax, [OffTransferDTA]
        mov [TaskDTA], edx
        mov [ebp + DC_EDX], eax
        mov eax, [OffTransferSegment]
        mov [ebp + DC_DS], ax
        mov eax, [ebp + DC_DS0]
        mov [TaskDTA + 4], ax
        call DosCall
        mov [ebp + DC_EDX], edx
        ret
;---------------------------------- read and write ---------------------------
rw_file:
;Source pointer - esi, source length - SavedSize
;Length read - edx
;Old buffer pointer - ebp
ReadWriteFile:
_minreserved equ 1000h
_mincluster equ 512
%define _bytesleft edx
        mov eax, [OffTransferSegment]
        mov [ebp + DC_DS], ax
        mov eax, [OffTransferStack]
        push edx                ;save original edx
        push eax                ;save TransferStack
        mov esi, edx
        mov _bytesleft, ecx   ;DataPtr.DC_ECX
        mov edi, eax
        push _bytesleft
        sub eax, _minreserved     ;eax is signed now!
        cmp eax, _mincluster
        _ifnot jg
          mov eax, _mincluster
        _endif
        and eax, -(_mincluster)
        cmp eax, _bytesleft
        _ifnot jb
          mov eax, _bytesleft
        _endif
        sub edi, eax
        jb near TransferStackOverflow
        and edi, ~(3)
        push eax               ;@@buffersize
;        @@buffersize equ dword ptr ss:[ebp-20]
        mov [OffTransferStack], edi
        mov [ebp + DC_EDX], edi
        _do
        mov ecx, [ebp - 20]
        cmp ecx, _bytesleft
        _ifnot jb
          mov ecx, _bytesleft
        _endif
        mov [ebp + DC_ECX], ecx
        mov eax, ecx
        or bl, bl
        _ifnot je
          les edi, [OffTransferStack]
          mov fs, [ebp + DC_DS0]
          call MoveStr
        _endif
        mov [ebp + DC_EAX + 1], bh
        call DosCall
        jnz Err@153
        movzx ecx, word [ebp + DC_EAX]
        or bl, bl
        _ifnot jne
          push ecx
          mov edi, esi
          mov es, [ebp + DC_DS0]
          lfs esi, [OffTransferStack]
          call MoveStr
          mov esi, edi
          pop ecx
        _endif
        sub _bytesleft, ecx
        jbe Exit@153
        cmp eax, ecx
        _enddo jbe
Exit@153:
        pop eax      ;drop buffer size
        pop eax      ;@@read_rq
        mov esi, eax
        sub eax, _bytesleft
ret@153:
        mov [ebp + DC_EAX], eax
        mov [ebp + DC_ECX], esi    ;restore read_rq
        pop dword [OffTransferStack]
        pop dword [ebp + DC_EDX]
        ret
Err@153:
        pop eax        ;drop buffer size
        pop esi        ;read_rq
        mov eax, 4201h
        xchg [ebp + DC_EAX], ax
        sub _bytesleft, esi
        jz ret@153
        mov dword [ebp + DC_EDX], _bytesleft
        shr _bytesleft, 16
        mov dword [ebp + DC_ECX], _bytesleft
        call DosCall
        jmp ret@153
;es:edi - destination, fs:esi - source, ecx - num
MoveStr:
        push ecx
        shr ecx, 2
        rep fs movsd
        pop ecx
        and ecx, 3
        rep fs movsb
        ret


;---------------------------------- exec ------------------------------------
DosExecute:
        cmp byte [ebp + DC_EAX], 0
        jne ToOldInt1
        call GetAsciizLenPush        ;push command name on ds:edx
        push edx                 ;save last transfer stack and original edx
        push esi                 ;save last transfer stack and original edx
        push gs
        add eax, (12h + 20 + 8) & (~(3))  ;shift TransferStack
        sub edx, eax
        jb near TransferStackOverflow@155 ;overflow
        mov [OffTransferStack], edx      ;reserve space for CB
        lea edi, [edx + 12h]
        mov [es:edx + 6], edi         ;offset of first fcb
        mov [es:edx + 10], edi         ;offset of second fcb
        mov al, 0
        mov cl, 20
        rep stosb
        mov gs, [ebp + DC_ES0]
        mov ebx, [ebp + DC_EBX]     ;load pointer to PM parameter block
        lfs esi, [gs:ebx + 6]
        mov cl, [fs:esi]   ;load size of command line
        inc ecx
        inc ecx
        call PushStr@155
        mov [es:edx + 2], ax             ;store to cb it address
        lfs esi, [gs:ebx + 0]  ;load environment pointer
        xor ecx, ecx
        _do
        cmp word [fs:esi + ecx], 0
        _break je
        inc cx
        _loop jns
        ;too long or bad environment
        mov al, 10
        jmp RetErrorCode@155
        _enddo
        inc ecx
        inc ecx
        push ecx                    ;save environment size
        mov ax, 100h
        lea ebx, [ecx + 15]
        shr ebx, 4
        mov ecx, edx
        int 31h
        jc TransferStackOverflow1@155 ;no DOS memory for environment
        ;shrd e
        ;call @@PushStr             ;move environment
        mov [es:ecx], ax           ;save it to CB

        ;shr  eax, 4                 ;convert offset to segment disp
        mov ax, [OffTransferSegment]   ;TransferSegment
        mov [ebp + DC_ES], ax
        mov [es:ecx + 4], ax         ;set segment address of transfer stack
        mov [es:ecx + 8], ax         ;set segment address of transfer stack
        mov [es:ecx + 12], ax        ;set segment address of transfer stack
        mov eax, [ebp + DC_EBX]
        mov [ebp + DC_EBX], ecx
        mov es, edx
        pop ecx
        xor edi, edi
        rep fs movsb
        call DosCallErr
        mov [ebp + DC_EBX], eax
        mov ax, 101h
        int 31h                     ;free environment segment
        _ifnot jnc
          mov ebx, edx              ;if error
          mov ax, 0001h
          int 31h                   ;try to free environment selector
        _endif
Rest@155:
        pop gs                      ;original gs
        pop dword [ebp + DC_EDX]          ;original edx
        pop edx                     ;TransferStack
        jmp Rest1                   ;restore only Transfer stack form edx
TransferStackOverflow1@155:
        pop eax
TransferStackOverflow@155:
        mov al, 8                    ;insufficient memory
RetErrorCode@155:
        and eax, 7Fh
        mov [ebp + DC_EAX], eax      ;write error code
        or word [ebp + DC_Flags], 1      ;stc
        jmp Rest@155

PushStr@155:
        mov edi, [OffTransferStack]
        sub edi, ecx
        jb TransferStackOverflow1@155
        and edi, ~(0Fh)     ;align on paragraph
        mov [OffTransferStack], edi
        mov eax, edi
        rep fs movsb
        ret


;--------------------------- rename file ------------------------------------
;don't needed with short jumps
w_asciiz_dsdx_esdi: ;for rename file
        call GetAsciizLenPush
        push esi
        push edx
        mov bx, p_esdi         ;word ptr DataPtr.DC_OffsetIndex, 0    ;es:edi internal index
        call w_asciiz_de
        pop dword [OffTransferStack]
        pop dword [ebp + DC_EDX]
        ret

;--------------------------- mouse set handler ------------------------------
MouseSetHandler:
        mov ax, 900h
        int 31h                      ;disable virtual interrupts
        push eax                      ;save old virtual interrupts state
        movzx eax, word [ebp + DC_ES0]
        mov ebx, edx
        mov esi, eax
        xchg [OffMouseHookProc + 0], ebx
        xchg [OffMouseHookProc + 4], esi
        mov edi, cs
        cmp di, ax
        mov ecx, [HookFromMouse32]
        _ifnot jne
          cmp edx, DefMouseHookProc
          je DosCall@157
        _endif
        or eax, edx
        db 0B9h
        LWord MouseCallBackX
        dw DGROUP16
        dw ROffMouseRHandlerEntry
        jne DosCall@157
        xor ecx, ecx
DosCall@157:
        mov [ebp + DC_ES], cx
        shr ecx, 16
        mov [ebp + DC_EDX], cx
        mov al, [ebp + DC_EAX + 0]
        call DosCall
        xchg [ebp + DC_EDX], edx
        cmp al, 14h            ;exchange function ?
        _ifnot jne
          mov [ebp + DC_EDX], ebx
          mov [ebp + DC_ES], si
          cmp ebx, DefMouseHookProc
          _ifnot jne
            mov edi, cs
            cmp di, si
            _ifnot jne
              shl edx, 16
              mov dx, [ebp + DC_ES]
              mov [HookFromMouse32], edx
            _endif
          _endif
        _endif
        pop eax
        int 31h                ;restore old virtual interrupts state
        ret

DefMouseHookProc:
        retf
;------------------------------ Mouse callback ------------------------------
;assume es:EGroup
MouseCallbackHandler:
        ;cld
        push es
        ;lodsd                           ;load return address
        pop ds
        ;mov  ax, TransferSegment
        ;add  ax, TransferBufferSize/16
        ;shl  eax, 16
        ;mov  ax, OffMouseCallbackPlace+4-OffMouseRHandler
        db 0C7h, 47h, DC_IP
        dw OffMouseCallbackPlace + 4 - OffMouseRHandler
;LWord MouseRHandlerSRef
        dw DGROUP16
        ;add  word ptr ds:[edi].DC_SP, 4           ;simulate retf
        ;check for nested callback
        ;shr  byte ptr ds:InMouseFlag, 1
        ;shr  byte ptr ds:[edi][InMouseFlag-MouseCallbackStruct], 1
        ;$ifnot jnc          ;ignore nested call(this is an unexpected event)
        ;mov  esi, edi
        ;add  edi, 34h
        push edi
        push es
        ;mov  ecx, 34h/4
        mov eax, ss
        mov [edi + OffMouseHandlerESP - OffMouseCallbackStruct], esp
        ;rep  movs dword ptr es:[edi], dword ptr ds:[esi]
        lar eax, eax
        shr eax, 23     ;copy segment-32 bit to carry;  S32Bit
        _ifnot jc
        movzx esp, sp
        _endif
        movzx eax, word [edi + DC_EAX]
        movzx ebx, word [edi + DC_EBX]
        movsx ecx, word [edi + DC_ECX]
        movsx edx, word [edi + DC_EDX]
        movsx esi, word [edi + DC_ESI]
        movsx edi, word [edi + DC_EDI]
        mov es, [OffFlatSelector]
        push es
        pop ds
        pushfd                   ;for compatibility with buggy hookers
        call far [cs:OffMouseHookProc]
        ;mov ax, 900h
        ;int 31h
        ;cli        ;hooker may enable interrupts, but callback
                   ;must disable interrupts before setting reenterancy flag
        mov esp, [cs:OffMouseHandlerESP]
        pop es
        pop edi
        ;mov  byte ptr es:[edi][InMouseFlag-MouseCallbackStruct1], 1
        ;mov  byte ptr es:InMouseFlag, 1
        ;$endif
        iretd
;assume  es:nothing

;--------------------- Macros for translation definitions -------------------
        SEGM EData
%assign FnIdx 1
%macro RegFnIndex 2
        zequ zcat3(%1,%2,Idx), FnIdx*2
        %assign FnIdx FnIdx+1
        dw %2
        dw %1 wrt EGroup
%endmacro

%assign ttpend 0
%assign ttpos 0
%assign ttmax 0
%macro TransEntry 3
        times (%1) - ttpos db 0
        db zcat3(%2,%3,Idx)
        %assign ttpos (%1) + 1
%endmacro
%macro TransTableEnd 0
        times ttmax + 1 - ttpos db 0
        %assign ttpend 0
%endmacro
%macro DefTransTable 2
%if ttpend
        TransTableEnd
%endif
        %assign ttpend 1
        %assign ttmax %2
        %assign ttpos 0
%1:
        db ttmax
%endmacro
%macro TTTX 0
%endmacro


InternalIntNum equ 0


;---------------------------indexes for translation tables-------------------
MethodTable0:
        RegFnIndex w_ascii$, p_dsdx
        RegFnIndex wr_pas, p_dsdx
        RegFnIndex SetDTA, p_dsdx
        RegFnIndex r_ptr, p_dsbx
        RegFnIndex r_ptr, p_esbx
        RegFnIndex w_vect, 205h
        RegFnIndex GetDTA, p_esdx
        RegFnIndex r_vect, 204h
        RegFnIndex w_asciiz_de, p_dsdx
        RegFnIndex w_asciizc, p_dsdx
        RegFnIndex w_asciizc_zc, p_dsdx
        RegFnIndex rw_file, 4001h
        RegFnIndex rw_file, 3f00h
      ;read or write file
;RegFnIndex wr_64,        p_dssi      ;get cur dir
        RegFnIndex allocmem, 100h        ;realloc mem
        RegFnIndex freemem, 101h        ;realloc mem
        RegFnIndex reallocmem, 102h        ;realloc mem
        RegFnIndex w_asciiz_r_dta, p_dsdx     ;find first
        RegFnIndex DTACall, 0           ;find next
        RegFnIndex w_asciiz_dsdx_esdi, p_dsdx ;rename file
        RegFnIndex r_createunic, p_dsdx      ;create unic
        RegFnIndex MouseSetHandler, p_esdx
        RegFnIndex DosExecute, p_dsdx      ;exec
        RegFnIndex ExitDPMI, 0
        RegFnIndex wr_64, p_esdx
        RegFnIndex wr_bx, p_esdx
;RegFnIndex GetExtVer,
        RegFnIndex wr_17, p_esdx
        RegFnIndex wr_cx3, p_esdx
        RegFnIndex r_psp, 0
        RegFnIndex wr_256, p_esdi
        RegFnIndex r_ptr_zc, p_esdi
        RegFnIndex VBE4F00, p_esdi
        RegFnIndex wr_ecx4, p_esdi
        RegFnIndex DosCallErr, 0
        RegFnIndex r_ptr_zcd, p_dsbx
        RegFnIndex onalnff_r_ptr_zcd, p_dsbx
        RegFnIndex onal0_r_ptr, p_dsbx
        RegFnIndex dc_zc, 0
        RegFnIndex onaxnff_zabcd, 0
        RegFnIndex r_country, p_dsdx
        RegFnIndex lseek_, 0
        RegFnIndex file_attrs, p_dsdx
        RegFnIndex wr_64_de, p_dssi
        RegFnIndex zd_de, 0
        RegFnIndex r_cx_za, p_dsdx
        RegFnIndex w_cx_za, p_dsdx
        RegFnIndex dc_za, 0

;----------------------------translation tables------------------------------
;DefTransTable int21Table, 21h, 09h, 0FFh
        DefTransTable int21Table, 6Ch
;@@eee=1
        TransEntry 09h, w_ascii$, p_dsdx
        TransEntry 0Ah, wr_pas, p_dsdx
        TransEntry 1Ah, SetDTA, p_dsdx     ;set dta
        TransEntry 1Bh, r_ptr_zcd, p_dsbx  ;get FAT info
        TransEntry 1Ch, onalnff_r_ptr_zcd, p_dsbx   ;get FAT info for spec drive
        TransEntry 1Fh, onal0_r_ptr, p_dsbx ;get DPB
        TransEntry 25h, w_vect, 205h       ;set int vector
        TransEntry 2Ah, dc_zc, 0           ;get system date
        TransEntry 2Fh, GetDTA, p_esdx
        TransEntry 32h, onal0_r_ptr, p_dsbx ;get DPB
        TransEntry 34h, r_ptr, p_esbx      ;get InDos flag
        TransEntry 35h, r_vect, 204h       ;get interrupt vector
        TransEntry 36h, onaxnff_zabcd, 0   ;get disk info
        TransEntry 38h, r_country, p_dsdx  ;get/set country code
        TransEntry 39h, w_asciiz_de, p_dsdx ;Create subdir
        TransEntry 3Ah, w_asciiz_de, p_dsdx ;remove subdir
        TransEntry 3Bh, w_asciizc, p_dsdx  ;set directory
        TransEntry 3Ch, w_asciizc, p_dsdx  ;create file
        TransEntry 3Dh, w_asciizc, p_dsdx  ;open file
        TransEntry 3Eh, DosCallErr, 0      ;close file
        TransEntry 3Fh, rw_file, 3f00h     ;read from file
        TransEntry 40h, rw_file, 4001h     ;write to file
        TransEntry 41h, w_asciiz_de, p_dsdx ;delete file
        TransEntry 42h, lseek_, 0          ;lseek
        TransEntry 43h, file_attrs, p_dsdx ;get or set file attr
        TransEntry 45h, dc_za, 0           ;dup file handle
        TransEntry 47h, wr_64_de, p_dssi   ;get cur dir
        TransEntry 48h, allocmem, 100h     ;alloc mem
        TransEntry 49h, freemem, 101h      ;free mem
        TransEntry 4Ah, reallocmem, 102h   ;realloc mem
        TransEntry 4Bh, DosExecute, p_dsdx ;exec
        TransEntry 4Ch, ExitDPMI, 0        ;exit
        TransEntry 4Eh, w_asciiz_r_dta, p_dsdx    ;find first
        TransEntry 4Fh, DTACall, 0                ;find next
;TransEntry 51h, r_psp, 0                  ;get PSP segment
        TransEntry 52h, r_ptr, p_esbx             ;get list of list
        TransEntry 56h, w_asciiz_dsdx_esdi, p_dsdx ;rename file
;TransEntry 57h, xxx                      ;get last write date/time
        TransEntry 5Ah, r_createunic, p_dsdx      ;create unicue file
        TransEntry 5Bh, w_asciizc, p_dsdx         ;create new file
;TransEntry 5Ch,                          ;lock region
        TransEntry 62h, r_psp, 0                  ;Get PSP selector
        TransEntry 67h, DosCallErr, 0             ;Set handle count
        TransEntry 68h, DosCallErr, 0             ;Flush file handle
        TransEntry 6Ah, DosCallErr, 0             ;Flush file handle
        TransEntry 6Ch, w_asciizc_zc, p_dsdx      ;open file extended

;@@eee=0
        DefTransTable int2144Table, 0Fh
        TransEntry 0, zd_de, 0                ;get device info
        TransEntry 1, DosCallErr, 0           ;set device info
        TransEntry 2, r_cx_za, p_dsdx         ;char ioctl read
        TransEntry 3, w_cx_za, p_dsdx         ;char ioctl write
        TransEntry 4, r_cx_za, p_dsdx         ;block ioctl read
        TransEntry 5, w_cx_za, p_dsdx         ;block ioctl write
        TransEntry 6, DosCallErr, 0           ;get input status
        TransEntry 7, DosCallErr, 0           ;get output status
        TransEntry 8, dc_za, 0                ;is device removable?
        TransEntry 9, zd_de, 0                ;is device remote?
        TransEntry 0Ah, zd_de, 0              ;is handle remote?
        TransEntry 0Bh, DosCallErr, 0         ;set sharing retry count
        TransEntry 0Eh, DosCallErr, 0         ;logical drive map
        TransEntry 0Fh, DosCallErr, 0         ;-----
        TransTableEnd



        DefTransTable int33Table, 17h
        TransEntry 09h, wr_64, p_esdx           ;set graphics cursor
        TransEntry 0Ch, MouseSetHandler, p_esdx ;set mouse callback
        TransEntry 14h, MouseSetHandler, p_esdx ;exchange mouse callback
        TransEntry 16h, wr_bx, p_esdx
        TransEntry 17h, wr_bx, p_esdx

        DefTransTable int1010Table, 17h
        TransEntry 2, wr_17, p_esdx   ;set all palette
        TransEntry 9, wr_17, p_esdx   ;get all palette
        TransEntry 12h, wr_cx3, p_esdx  ;set DAC block
        TransEntry 17h, wr_cx3, p_esdx  ;get DAC block

;DefTransTable int101030Table, 0
;TransEntry 0, r_ptr, p_bpdi     ;get font pointer

        DefTransTable int104FTable, 0Ah
        TransEntry 0, VBE4F00, p_esdi     ;get
        TransEntry 1, wr_256, p_esdi      ;get vmode info
        TransEntry 9, wr_ecx4, p_esdi     ;
        TransEntry 0Ah, r_ptr_zc, p_esdi
        TransTableEnd




;-------------------------- Exception handlers ------------------------------
TermMsg: db 13, 10
  db 'Unhandled exception #', 2, ', error code ', 4, ' at ', 4, ':', 8, 13, 10
  db 'eax=', 8, ' ebx=', 8, ' ecx=', 8, ' edx=', 8, 13, 10
  db 'esp=', 8, ' ebp=', 8, ' esi=', 8, ' edi=', 8, 13, 10
  db 'eflags=', 8, '  unrelocated eip=', 8, 13, 10, 0
Term1Msg: db '=', 4, ' base=', 8, ' limit=', 8, ' acc=', 4, 13, 10, 0
Term2Msg: db 'cs', 0, 'ds', 0, 'es', 0, 'ss', 0, 'fs', 0, 'gs', 0
TermStkcMsg: db '[ss:esp]: ', 8, ' ', 8, ' ', 8, ' ', 8, ' ', 8, ' ', 8, ' ', 8, ' ', 13, 10, 0
TermInvSelMsg: db '=', 4, ' invalid selector', 13, 10, 0
TermEmpSelMsg: db '=', 4, ' null selector', 13, 10, 0
        LByte ZeroDivMsg
  db 'ZRDX runtime error: divide overflow', 13, 10, '$'
        LByte TSOverflowMsg
  db 'ZRDX runtime error: transfer buffer overflow', 13, 10, '$'
        ESEG EData
;---------------------------exception 0 handler------------------------------
        LLabel Exc0handler
        push eax
        push edi
        push es
%assign F 3 * 4
        mov edi, [cs:OffClientInt0Vector + 4]
        mov eax, cs
        cmp ax, di
        mov eax, [cs:OffClientInt0Vector + 0]
        _ifnot jne
        cmp eax, OffDefaultInt0Handler
        je GoDefaultExcHandler
        _endif
        xchg [esp + F + EXC_CS], edi
        xchg [esp + F + EXC_EIP], eax
        push edi
        sub dword [esp + F + 4 + EXC_ESP], 3 * 4
        les edi, [esp + F + 4 + EXC_ESP]
        cld
        stosd
        pop eax
        stosd
        mov eax, [esp + F + EXC_EFlags]
        stosd
        pop es
        pop edi
        pop eax
        LLabel Exc3Handler
        retf
;LLabel Exc1Handler
;        push 1
;        jmp  GlobalExceptionEntry
;        and  byte ptr [esp][1].EXC_EFlags, not 1    ;clear TF
;        retf
        ;exc 0 start here
;------------------------entries for all exceptions--------------------------
ExceptionHandler:
%assign ExcptNum 0
%rep 16
%if (ExcptNum > 7) || (ExcptNum = 6)
            push ExcptNum
            jmp short GlobalExceptionEntry
%endif
%assign ExcptNum ExcptNum + 1
%endrep
GoDefaultExcHandler:
        pop es
        pop edi
        pop eax
        push 0
        ;jmp GlobalExceptionEntry
;------------------------- common exceptin handler --------------------------
GlobalExceptionEntry:
        push gs
        push fs
        push dword [esp + (3 * 4) + EXC_SS]     ;ss
        push es
        push ds
        push eax                     ;reserve space for unrelocated eip
        push dword [esp + (7 * 4) + EXC_EFlags]    ;eflags
        push edi
        push esi
        push ebp
        mov ebp, esp                ;and use [ebp] later for dec size of code
%assign F 11 * 4
        push dword [ebp + F + EXC_ESP]         ;esp
        push edx
        push ecx
        push ebx
        push eax
        mov ax, 0 ;selector
        LLabel SelReference1
               ;ra:2, ec, rip, rf, rsp
        mov ds, eax

        ;push dword ptr [ebp+F].EXC_EIP ;eip
        mov eax, [ebp + F + EXC_EIP] ;eip
        push eax
        sub eax, [OffImageBaseAddr]
        mov [ebp + (4 * 4)], eax
        push dword [ebp + F + EXC_CS]        ;cs
        push dword [ebp + F + EXC_Errcode]   ;err code
        push dword [ebp + F - 4]             ;exception num
        ;movzx eax, OrigVideoMode  ;
        ;and  al, 7Fh
        ;int  10h                        ;clear screen and restore initial video mode
        ;move stack content to new location
        push ds
        or edx, -(1)
        mov [ebp + F + EXC_CS], cs
        mov eax, [OffExcReturnAddr]
        mov [ebp + F + EXC_EIP], eax
        ;mov  ss:[ebp+F].EXC_EIP, offset EGroup:ExcHandler2Entry
        cmp eax, ExcHandler2Entry
        _ifnot jne
        mov esi, esp
        mov es, [OffFlatSelector]     ;TransferSelector
        movzx edi, word [OffTransferSegment]
        shl edi, 4
        add edi, 1000h - 200       ;allocate space for stack in TransferBuffer
        mov dword [OffTransferStack], 800h
        xor ecx, ecx
        mov cl, 21               ;number of dwords in exception state
        cld
        mov [ebp + F + EXC_SS], es
        mov [ebp + F + EXC_ESP], edi
        pushfd
        pop dword [ebp + F + EXC_EFlags]   ;replace client eflags with my own
        rep ss movsd
        ;mov  esp, esi
        _endif
        add esp, 21 * 4
        retf           ;return to my secondary exception handler with my stack

ExcHandler2Entry:
        pop ds
        cld               ;some dpmi hosts don't set eflags content correctly
        ;mov  ds, edx             ;reload my own ds, because some dpmi hosts
                                 ;destroy it during returning from exception
        mov esi, TermMsg
%ifndef Release
        mov ah, 0
        int 16h
        cmp al, 27
        je Term@159
%endif
        ;mov ax, 3
        ;int 10h
        call PrintFStr
        pop eax       ;num
        pop eax       ;err code
        pop edi       ;cs
        sub ecx, 7
        mov esi, [esp + (5 * 4)]     ;esp
        mov es, [esp + (13 * 4)]     ;ss
        mov dword [OffExcReturnAddr], ExcHandler3Entry
        _do
          mov edx, [es:esi]
ExcHandler3Entry:
          add esi, 4
          mov [esp + ecx*4 + (7 * 4)], edx
          inc ecx
        _enddo jnz
        mov esi, TermStkcMsg
        call PrintFStr
        add esp, 11 * 4

        push edi         ;push cs again
        mov ebp, 6
        mov edi, Term2Msg
        _do
        mov esi, edi
        add edi, 3
        call PrintFStr
        pop ebx      ;selector
        lar eax, ebx
        shr eax, 8
        push eax
        xor edx, edx
        xor ecx, ecx
        mov ax, 6
        int 31h
        shl ecx, 16
        mov cx, dx
        lsl edx, ebx
        mov esi, Term1Msg
        push edx
        push ecx
        push ebx
        _ifnot jz
          mov esi, TermInvSelMsg
          cmp bx, 3
          _ifnot ja
            mov esi, TermEmpSelMsg
          _endif
        _endif
        call PrintFStr
        add esp, 16
        dec ebp
        _enddo jnz
Term@159:
        mov ax, 4CFEh
        pushfd
        push cs
        call int21entry
        jmp $
CallInt10:
        pushfd
        call far [EID10 + EID_OldIntVect]
        ret
PrintFStr:
        pushad
        mov ebp, esp
        _do
        lodsb
        cmp al, 0
        _break je
        cmp al, 8
        _ifnot jbe
        mov ah, 0Eh
        mov bx, 07h
        ;int 10h
        call CallInt10
        _else jmp
        add ebp, 4
        push dword [ebp + 32]
        movzx eax, al
        push eax
        call PrintN
        _endif
        _enddo jmp
        popad
        ret

        LLabel Starter
        int 31h
        mov eax, ebx
        mov esi, ebx
        mov edi, ebx
        retf
%ifndef Release
Print8:
        push dword [esp + 4]
        push 8
        call PrintN
        pushad
        mov ax, 0E00h + ' '
        call CallInt10
        ;int 10h
        popad
        ret 4

%endif
PrintN:
        push eax
        push ecx
;@@Digit EQU DWORD PTR ss:[esp+8].8
;@@N     EQU DWORD PTR ss:[esp+8].4
        mov eax, [esp + 8 + 8]
        mov cl, 8
        sub cl, [esp + 8 + 4]
        shl cl, 2
        rol eax, cl
        mov ecx, [esp + 8 + 4]
        _do
        rol eax, 4
        pushad
        and al, 1111b
        add al, '0'
        cmp al, '9'
        _ifnot jbe
        add al, 'A' - '9' - 1
        _endif
        mov ah, 0Eh
        mov bx, 07h
        call CallInt10
        ;int 10h
        popad
        _enddo loop
        pop ecx
        pop eax
        ret 8


int75handler: ;handler for numeric coprocessor interrupt
        push eax
        mov al, 0
        out 0F0h, al
        mov al, 20h
        out 0A0h, al
        out 020h, al
        pop eax
        int 2
        sti
        iretd
TransferStackOverflow:
        mov edx, OffTSOverflowMsg
        jmp FatalExtExit
        DPROC DefaultInt0Handler
        mov edx, OffZeroDivMsg
FatalExtExit:
        push cs
        pop ds
        mov ah, 9
        int 21h
        mov ax, 4CFFh
        int 21h


        ESEG EText
