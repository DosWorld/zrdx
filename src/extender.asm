;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

VSegm EBSS
DFD OldVectors, 10
DTASize = 40h
DFB DefaultDTA, 44 ;43          ;DTA, when client started
DFB MouseCallbackStruct, 34h    ;DC structure for mouse callback
DFD MouseHandlerESP             ;saved ESP for mouse handler
DFD ImageBaseAddr
EVSeg  EBSS

NOWARN Res
DataPtr EQU ss:[ebp]
WARN Res

Segm EData
LDWord ExcReturnAddr
       DD offset EGroup:ExcHandler2Entry
LDWord ClientInt0Vector
       DD OffDefaultInt0Handler
       DD 0          ;must be filled with cs
;StackLo DD OffxStack
;SvStack DD OffxStackEnd
LDWord FlatSelector
       DD 0
LDWord SavedPSP
       DD 0
LDword TransferStack
        DD -DTASize          ;current pointer to top of the transfer stack
LWord   TransferSelector        ;selector of transfer stack
        DW 0, 0
TaskDTA DD OffDefaultDTA ;pointer to current client DTA
        DW 0, 0
LDWord  TransferDTA
        dd -DTASize         ;pointer to DTA in RM area, reported to DOS
LDWord MouseHookProc
        DD Offset EGroup:DefMouseHookProc, 0 ;offset and segment   DD ?
HookFromMouse32  DD 0   ;address returned by mouse on exchange vectors
ifndef RELEASE
  OutPos DD 0; 0B8000h+160*10  ;DEBUG only
endif
LWord TransferSegment             ;RM segment of the transfer stack
        DW ?
LByte OrigVideoMode
        DB 3
ESeg EData
assume ds:EGroup, ss:nothing, cs:ETextG
Segm  EText
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
TTTX    MACRO
        ;call TTT
        ENDM

EIntDescr struc
        EID_FirstJmp     DD ?
        EID_OldIntVect   DD ?
        EID_OldIntVectHi DW ?
                         DB ?    ;reserved for alignment
        EID_IntNum       DB ?
        ends
Segm EData
EID0  label EIntDescr
EID33  EIntDescr <int33, 0,0,0,33h>
EID10  EIntDescr <int10, 0,0,0,10h>
EID21  EIntDescr <int21,0,0,0,21h>
ESeg EData
LLabel  FirstExtHandler
        $FirstExtHandler=$
        pushfd
        push offset ds:EID33
        jmp short IntEntry
        ExtHandlerStep = $-$FirstExtHandler

        pushfd
        push offset ds:EID10
        jmp short IntEntry

int21entry:
        pushfd
        push offset ds:EID21
IntEntry:
        sub  esp, DC_EID-DC_EAX-4
        pushad
        mov  ebp, esp
        IFDEF D4G
        mov edi, seg LGroup
        ELSE
        mov  edi, 0
LLabel SelReference0
        ENDIF
        mov [ebp].DC_DS0, ds
        mov  ds, edi
        mov  esi, [ebp].DC_EID
        jmp  [esi].EID_FirstJmp
;new entry
Int33:  or   ah, ah
        jnz  ToOldInt0
        mov  edi, Offset ds:int33Table
        jmp  CallMethod
Int10:  mov  edi, offset ds:int1010Table
        IFNDEF Release
        cmp ah, 0
        je  rrrr1
        cmp  ax, 4f02h      ;ignore vesa set videomode
        $ifnot jne
         mov dword ptr DataPtr.EAX, 4Fh
         jmp  rrrr1
        $endif
        ENDIF
        cmp  ah, 10h
        je   CallMethod
        mov  edi, offset ds:int104FTable
        cmp  ah, 4Fh
        je   CallMethod
        cmp  ax, 1130h
        jne  ToOldInt0
        mov  eax, 0
        org  $-4
        DW   p_esbp, offset cs:r_ptr
        jmp   CallMethod1
Int21:  cmp  eax, 0FF00h
        je   GetExtVer?
        mov  edi, offset ds:int2144Table
        cmp  ah, 44h
        je   CallMethod
        mov  edi, offset ds:int21Table
CallMethod3:
        mov  al, ah
CallMethod:
        cmp  al, [edi]
        ja   ToOldInt0
        movzx eax, al
        mov  al, [edi+eax+1]
        shl  eax, 1
        jz   ToOldInt0
        mov  eax, dword ptr MethodTable0[eax-4]
CallMethod1:
        mov  edi, [ebp].DC_REFLAGS     ;saved eflags
        mov  ebx, eax
        mov  dword ptr DataPtr.DC_Flags, edi
        shr  eax, 16
        xor  edi, edi
        mov  DataPtr.DC_FS0, fs
        mov  DataPtr.DC_ES0, es
        mov  dword ptr DataPtr.DC_ES, edi
        mov  dword ptr DataPtr.DC_FS, edi
        cld
        ifdef D4G
        add  eax, offset cs:base_fn_addr
        endif
        call eax
        mov  al, byte ptr DataPtr.DC_FLAGS
        mov  es,  DataPtr.DC_ES0
        mov  fs,  DataPtr.DC_FS0
        mov  byte ptr [ebp].DC_REFLAGS1, al
rrrr1:
        mov ds,  DataPtr.DC_DS0
        popad
        add esp, DC_REIP1-DC_EAX-4
        iretd
base_fn_addr:
GetExtVer?:
        cmp dx, 78h
        jne ToOldInt0
        mov DataPtr.DC_EAX, 0FFFF3447h
        jmp rrrr1
;------------------------------ int 4Ch -------------------------------------
;free all callback handlers
ExitDPMI:
        mov  edx, 0
        org  $-4
LDWord MouseCallbackPlace1
        DD  0
        shld ecx, edx, 16
        mov  ax, 304h
        int  31h
;must save after call: esi, edx, ecx
ToOldInt1:
        pop eax      ;drop ret address
ToOldInt0:
        movzx edi, [esi].EID_OldIntVectHi
        mov  esi,  [esi].EID_OldIntVect
        mov  ds, [ebp].DC_DS0
        mov  [ebp].DC_REIP, esi
        mov  [ebp].DC_RCS, edi
        popad
        add  esp, DC_REIP-DC_EAX-4
        iretd

;read the pointer
GetasciizLen:
        mov al, 0
GetasciiLen:
        movzx ecx, bh           ;DataPtr.DC_SegmentIndex
        mov es, DataPtr[ecx].DC_FirstSR0
        mov cl, bl              ;edi, DataPtr.DC_OffsetIndex
        mov edi, DataPtr[ecx].DC_FirstR
        mov ecx, -1
        repne scasb
        not ecx
        ret
GetAsciizLenPush:
        call GetAsciizLen
;Entry: ecx - size, ???-index
;Exit:  edx-old transfer stack, fs:esi(ecx) - saved data, eax - size
;ecx - zero
PushStr PROC  ;copy data to transfer stack and save registers
        movzx eax, bh      ;, DataPtr.DC_SegmentIndex
        mov  edi, dword ptr TransferSegment
        and  ebx, 7Fh
        mov  DataPtr[eax].DC_FirstSR, di
        les  edi, fword ptr TransferStack
        mov  fs, DataPtr[eax].DC_FirstSR0
        mov  edx, edi
        sub  edi, ecx
        mov  esi, DataPtr[ebx]
        jb   TransferStackOverflow
        and  edi, not 3                 ;align new stack pointer on dword
        mov  eax, ecx
        mov  DataPtr[ebx].DC_FirstR, edi
        shr  ecx, 2
        mov  TransferStack, edi
        rep  movs dword ptr es:[edi], fs:[esi]
        mov  cl, al
        and  cl, 3
        rep  movs byte ptr es:[edi], fs:[esi]
        sub  esi, eax                   ;restore esi
        ret
        ENDP
ResStr  PROC  ;copy data to transfer stack and save registers
        movzx edi, bh      ;, DataPtr.DC_SegmentIndex
        mov  eax, dword ptr TransferSegment
        and  ebx, 7Fh
        mov  DataPtr[edi].DC_FirstSR, ax
        mov  edx, TransferStack
        mov  fs, DataPtr[edi].DC_FirstSR0
        sub  edx, ecx
        jb   TransferStackOverflow
        and  edx, -4
        mov  esi, edx
        xchg esi, DataPtr[ebx].DC_FirstR
        mov  eax, ecx
        mov  TransferStack, edx
        ret
        ENDP

wr_cx3: ;movzx ecx, word ptr DataPtr.DC_ECX
        imul  ecx, 3
        jmp PushCallPop
wr_17:  mov ecx, 17
        jmp PushCallPop
wr_bx:  movzx ecx, word ptr DataPtr.DC_EBX
        jmp PushCallPop
wr_64:  mov ecx, 64
        jmp PushCallPop
wr_256: mov ecx, 256
        jmp PushCallPop
wr_ecx4:shl ecx, 2
        jmp PushCallPop
wr_pas:
        mov  fs,  DataPtr.DC_DS0
        movzx ecx, byte ptr fs:[edx]
        inc  ecx
        inc  ecx
PushCallPop:
        call PushStr
CallPop:
        call DosCall
        jmp  PopStr

r_createunic:
        call GetAsciizLen
        add  ecx, 13
;PushCallPopZEAX:
        call PushCallPop
ZeroEAX:
        mov  word ptr DataPtr.DC_EAX[2], cx
        ret
w_asciizc:
        mov al, 0
        call w_ascii
        jmp  ZeroEAX

;get directory
wr_64_de:
        mov  ecx, 64
        call PushStr
        ;jmp  CallEPop
;ResCallEPop:
;        call ResStr
CallEPop:
        call DosCallErr
PopStr  PROC  ;copy data from transfer stack and restore registers
        mov  ecx, fs
        mov  edi, esi
        mov  DataPtr[ebx].DC_FirstR, esi
        mov  es, ecx
        lfs  esi, fword ptr TransferStack
        mov  ecx, eax
        shr  ecx, 2
        and  al, 3
        rep  movs dword ptr es:[edi], fs:[esi]
        mov  cl, al
        rep  movs byte ptr es:[edi], fs:[esi]
        mov  TransferStack, edx
        ret
        ENDP
w_ascii$:
        mov  al, '$'
w_ascii:
        call GetAsciiLen
PushCallRest:
        call PushStr
CallRest:
        call DosCall
Rest:
        mov DataPtr[ebx].DC_FirstR, esi
Rest1:
        mov TransferStack, edx
        ret

w_asciiz_de:
        call GetAsciizLenPush
DosCallERest:
        call DosCallErr
        jmp Rest

w_asciiz_r_dta:
        call GetAsciizLenPush
        call DTACall
        jmp Rest

assume es:nothing
r_country:
        xor  ecx, ecx
        cmp  edx, -1
        $ifnot je
          mov  cl, 34
          call PushStr
        $endif
        call DosCallErr
        $ifnot jnz
        or   eax, eax
        $ifnot jz
          call PopStr
          mov  word ptr DataPtr.DC_EBX[2], cx
        $endif
        ret
        $endif
        or eax, eax
        jne Rest
        ret
w_asciizc_zc:
        cmp  byte ptr DataPtr.DC_EAX[0], 0
        jne  ToOldInt1
        call w_asciizc
        jz  z_c
        retn
r_ptr_zc: call r_ptr
z_c:
        mov word ptr DataPtr.DC_ECX[2], cx
        ret
dc_zc:  call DosCall
        jmp z_c

onalnff_r_ptr_zcd:
        call DosCall
        cmp  byte ptr DataPtr.DC_EAX, -1
        $ifnot je
        call ptr_cvt
        jmp  z_cd
r_ptr_zcd: call r_ptr
z_cd:
        mov word ptr DataPtr.DC_ECX[2], cx
z_d:
        mov word ptr DataPtr.DC_EDX[2], cx
        $endif
        ret
lseek_: call DosCall
        mov  word ptr DataPtr.DC_EAX[2], cx
        jz   z_d
        ret
zd_de:  call DosCallErr
        jz   z_d
        ret

;if(ax ne -1) movzx eax,ebx,ecx,edx
;else mov eax, -1
onaxnff_zabcd:
        call DosCall
        dec  ecx
        cmp  word ptr DataPtr.DC_EAX, cx
        $ifnot je
        inc  ecx
        mov  word ptr DataPtr.DC_EAX[2], cx
        mov  word ptr DataPtr.DC_EBX[2], cx
        jmp  z_cd
        $endif
        mov  DataPtr.DC_EAX, ecx
        ret

file_attrs:
        push DataPtr.DC_EAX
        call w_asciiz_de
        pop  eax
        $ifnot jnz
        or   al, al
        jz   z_c
        $endif
        ret

w_cx_za:call PushCallRest
        jmp za_
r_cx_za:call ResStr
        call CallPop
        jmp za_
;call dos then movzx eax
dc_za:  call DosCall
za_:
        mov word ptr DataPtr.DC_EAX[2], cx
        ret

;--------------------- back pointers convertors group -----------------------

;call dos and convert ptr if al eq 0
onal0_r_ptr:
        call DosCall
        cmp byte ptr DataPtr.DC_EAX, cl
        je  ptr_cvt
        ret

;entry: ebx - pointer descriptor
;exit:  ecx == 0
r_ptr:
        call DosCall
ptr_cvt:
        movzx edi, bh           ;DataPtr.DC_SegmentIndex
        mov  eax, FlatSelector
        and  ebx, 7Fh
        mov  DataPtr[edi].DC_FirstSR0, ax
        movzx eax, DataPtr[edi].DC_FirstSR
        shl  eax, 4
        movzx esi, word ptr DataPtr[ebx].DC_FirstR
        add  eax, esi
        mov  DataPtr[ebx].DC_FirstR, eax
        ret

;-------------------- special routine for VBE 4F00h -------------------------
Segm EData
LByte DispTable4F00
      DB 6, 0Eh, 16H, 1Ah, 1Eh
ESeg EData
VBE4F00:
        xor  ecx, ecx
        mov  edi, DataPtr.DC_EDI
        mov  ch, 1
        cmp  dword ptr es:[edi], 'VBE2'
        $ifnot jne
          mov ch, 2
        $endif
        call PushCallPop
        cmp  word ptr DataPtr.DC_EAX[0], 4Fh
        $ifnot jne
        mov  esi, OffDispTable4F00
        ;mov  ecx, 5
        mov  cl, 5       ;size of DispTable
        mov  edi, DataPtr.DC_EDI
        xor  eax, eax
        $do
        lodsb
        movzx edx, word ptr es:[edi+eax]
        movzx ebx, word ptr es:[edi+eax+2]
        shl   ebx, 4
        add   edx, ebx
        mov   es:[edi+eax], edx
        $enddo loop
        $endif
        ret
;----------------- call dos with DTA instread of transfer stack -------------
DTACall:
        push esi
        lfs esi, fword ptr TaskDTA
        mov edi, TransferDTA
        mov es,  TransferSelector
        mov ecx, 43
        rep movs byte ptr es:[edi], fs:[esi]
        lea esi, [edi-43]
        call DosCallErr
        les edi, fword ptr TaskDTA
        mov fs,  TransferSelector
        mov ecx, 43
        rep movs byte ptr es:[edi], fs:[esi]
        pop esi
        ret
;--------------------------- call dos and clear eah if CY--------------------
DosCallErr:
        call DosCall
        $ifnot jz
          mov word ptr DataPtr.DC_EAX[2], cx
        $endif
        ret
;----------------------------- call dos simple ------------------------------
;preserve eax, ebx, edx, ebp, esi
DosCall PROC  ;call DPMI interrupt
        push eax ebx
        mov  eax, ss
        mov  ebx, DataPtr.DC_EID
        mov  edi, ebp
        mov  es,  eax
        xor  ecx, ecx
        mov  eax, 300h
        mov  dword ptr DataPtr.DC_SP, ecx
        movzx ebx, [ebx].EID_IntNum
        ;sub  esp, 1000h
        int  31h
        ;add  esp, 1000h
        pop  ebx eax
        test byte ptr DataPtr.DC_Flags, 1
        ret
        ENDP
;--------------------------------- get psp ----------------------------------
r_psp:  mov eax, SavedPSP
        mov DataPtr.DC_EBX, eax
        ret
;----------------------------- memory procedures ----------------------------
reallocmem:
DPMIMemCall:
        xchg eax, ebx             ;load function number to ax
        mov  dx, DataPtr.DC_ES0
        mov  ebx, DataPtr.DC_EBX
        and  byte ptr DataPtr.DC_Flags, not 1  ;clear carry
        int  31h
        $ifnot jnc
          movzx eax, ax
          or  byte ptr DataPtr.DC_Flags, 1  ;set carry
          mov dword ptr DataPtr.DC_EAX, eax
          cmp al, 8
          $ifnot jne
            movzx ebx, bx
            mov  DataPtr.DC_EBX, ebx
          $endif
          stc
        $endif
        ret
allocmem:
        call DPMIMemCall
        $ifnot jc
          movzx edx, dx
          mov DataPtr.DC_EAX, edx
        $endif
        ret
freemem:
        call DPMIMemCall
        $ifnot jc
          mov DataPtr.DC_ES0, 0
        $endif
        ret

;--------------------------- interrupt vectors get/set ------------------------
w_vect:
        mov   al, byte ptr DataPtr.DC_EAX
        movzx ecx, DataPtr.DC_DS0
        or    al, al
        $ifnot jne
          mov ClientInt0Vector[4], ecx
          mov ClientInt0Vector[0], edx
          ret
        $endif
DPMIVectCall:
        xchg eax, ebx
        and  byte ptr DataPtr.DC_Flags, not 1  ;clear carry
        int  31h
        $ifnot jnc
          or byte ptr DataPtr.DC_Flags, 1      ;set carry
        $endif
        ret
r_vect:
        mov  al, byte ptr DataPtr.DC_EAX
        or   al, al
        $ifnot jne
          mov ecx, ClientInt0Vector[4]
          mov edx, ClientInt0Vector[0]
        $else jmp
          call DPMIVectCall
        $endif
        mov  DataPtr.DC_ES0, cx
        mov  DataPtr.DC_EBX, edx
        ret
;----------------------------- get and set DTA ------------------------------
GetDTA: mov  eax, TaskDTA
        mov  DataPtr.DC_EBX, eax
        mov  eax, TaskDTA[4]
        mov  DataPtr.DC_ES0, ax
        ret
SetDTA: mov  eax, TransferDTA
        mov  TaskDTA, edx
        mov  DataPtr.DC_EDX, eax
        mov  eax, dword ptr TransferSegment
        mov  DataPtr.DC_DS, ax
        mov  eax, dword ptr DataPtr.DC_DS0
        mov  word ptr TaskDTA[4], ax
        call DosCall
        mov  DataPtr.DC_EDX, edx
        ret
;---------------------------------- read and write ---------------------------
rw_file:
;Исходный указатель - esi, исходная длина - SavedSize
;Считанная длина - edx
;Старый указатель буфера - ebp
ReadWriteFile PROC PASCAL
@@MinReserved = 1000h
@@MinCluster  = 512
@@bytesleft equ edx
        mov  eax, dword ptr TransferSegment
        mov  DataPtr.DC_DS, ax
        mov  eax, TransferStack
        push edx                ;save original edx
        push eax                ;save TransferStack
        mov  esi, edx
        mov  @@bytesleft, ecx   ;DataPtr.DC_ECX
        mov  edi, eax
        push @@bytesleft
        sub  eax, @@MinReserved     ;eax is signed now!
        cmp  eax, @@MinCluster
        $ifnot jg
          mov eax, @@MinCluster
        $endif
        and  eax, -@@MinCluster
        cmp  eax, @@bytesleft
        $ifnot jb
          mov eax, @@bytesleft
        $endif
        sub  edi, eax
        jb   TransferStackOverflow
        and  edi, not 3
        push eax               ;@@buffersize
        @@buffersize equ dword ptr ss:[ebp-20]
        mov  TransferStack,  edi
        mov  DataPtr.DC_EDX, edi
        $do
        mov  ecx, @@buffersize
        cmp  ecx, @@bytesleft
        $ifnot jb
          mov ecx, @@bytesleft
        $endif
        mov  DataPtr.DC_ECX, ecx
        mov  eax, ecx
        or   bl, bl
        $ifnot je
          les edi, fword ptr TransferStack
          mov fs, DataPtr.DC_DS0
          call MoveStr
        $endif
        mov  byte ptr DataPtr.DC_EAX[1], bh
        call DosCall
        jnz  @@Err
        movzx ecx, word ptr DataPtr.DC_EAX
        or   bl, bl
        $ifnot jne
          push ecx
          mov  edi, esi
          mov  es, DataPtr.DC_DS0
          lfs  esi, fword ptr TransferStack
          call MoveStr
          mov  esi, edi
          pop  ecx
        $endif
        sub  @@bytesleft, ecx
        jbe  @@Exit
        cmp  eax, ecx
        $enddo jbe
@@Exit:
        pop  eax      ;drop buffer size
        pop  eax      ;@@read_rq
        mov  esi, eax
        sub  eax, @@bytesleft
@@Ret:
        mov  DataPtr.DC_EAX, eax
        mov  DataPtr.DC_ECX, esi    ;restore read_rq
        pop  TransferStack
        pop  DataPtr.DC_EDX
        retn
@@Err:  pop  eax        ;drop buffer size
        pop  esi        ;read_rq
        mov  eax, 4201h
        xchg word ptr DataPtr.DC_EAX, ax
        sub  @@bytesleft, esi
        jz   @@Ret
        mov  DataPtr.DC_EDX, @@bytesleft
        shr  @@bytesleft, 16
        mov  DataPtr.DC_ECX, @@bytesleft
        call DosCall
        jmp  @@Ret
;es:edi - destination, fs:esi - source, ecx - num
MoveStr:
        push ecx
        shr  ecx, 2
        rep  movs dword ptr es:[edi], fs:[esi]
        pop  ecx
        and  ecx, 3
        rep  movs byte ptr es:[edi], fs:[esi]
        retn
        ENDP

;---------------------------------- exec ------------------------------------
DosExecute PROC
        cmp  byte ptr DataPtr.DC_EAX, 0
        jne  ToOldInt1
        call GetAsciizLenPush        ;push command name on ds:edx
        push edx esi                 ;save last transfer stack and original edx
        push gs
        add  eax, (12h+20+8) and (not 3)  ;shift TransferStack
        sub  edx, eax
        jb   @@TransferStackOverflow ;overflow
        mov  TransferStack, edx      ;reserve space for CB
        lea  edi, [edx+12h]
        mov  es:[edx].6, edi         ;offset of first fcb
        mov  es:[edx].10,edi         ;offset of second fcb
        mov  al, 0
        mov  cl, 20
        rep  stos byte ptr es:[edi]
        mov  gs, DataPtr.DC_ES0
        mov  ebx, DataPtr.DC_EBX     ;load pointer to PM parameter block
        lfs  esi, fword ptr gs:[ebx].6
        mov  cl, byte ptr fs:[esi]   ;load size of command line
        inc  ecx
        inc  ecx
        call @@PushStr
        mov  es:[edx].2, ax             ;store to cb it address
        lfs  esi, fword ptr gs:[ebx].0  ;load environment pointer
        xor  ecx, ecx
        $do
        cmp  word ptr fs:[esi+ecx], 0
        $break je
        inc  cx
        $loop jns
        ;too long or bad environment
        mov  al, 10
        jmp  @@RetErrorCode
        $enddo
        inc  ecx
        inc  ecx
        push ecx                    ;save environment size
        mov  ax, 100h
        lea  ebx, [ecx+15]
        shr  ebx, 4
        mov  ecx, edx
        int  31h
        jc   @@TransferStackOverflow1 ;no DOS memory for environment
        ;shrd e
        ;call @@PushStr             ;move environment
        mov  es:[ecx], ax           ;save it to CB

        ;shr  eax, 4                 ;convert offset to segment disp
        mov  ax, word ptr TransferSegment   ;TransferSegment
        mov  DataPtr.DC_ES, ax
        mov  es:[ecx].4, ax         ;set segment address of transfer stack
        mov  es:[ecx].8, ax         ;set segment address of transfer stack
        mov  es:[ecx].12, ax        ;set segment address of transfer stack
        mov  eax, DataPtr.DC_EBX
        mov  DataPtr.DC_EBX, ecx
        mov  es, edx
        pop  ecx
        xor  edi, edi
        rep  movs byte ptr es:[edi], fs:[esi]
        call DosCallErr
        mov  DataPtr.DC_EBX, eax
        mov  ax, 101h
        int  31h                     ;free environment segment
        $ifnot jnc
          mov  ebx, edx              ;if error
          mov  ax, 0001h
          int  31h                   ;try to free environment selector
        $endif
@@Rest:
        pop  gs                      ;original gs
        pop  DataPtr.DC_EDX          ;original edx
        pop  edx                     ;TransferStack
        jmp  Rest1                   ;restore only Transfer stack form edx
@@TransferStackOverflow1: pop eax
@@TransferStackOverflow:
        mov  al, 8                    ;insufficient memory
@@RetErrorCode:
        and  eax, 7Fh
        mov  DataPtr.DC_EAX, eax      ;write error code
        or   DataPtr.DC_Flags, 1      ;stc
        jmp  @@Rest

@@PushStr:
        mov edi, TransferStack
        sub edi, ecx
        jb  @@TransferStackOverflow1
        and edi, not 0Fh     ;align on paragraph
        mov TransferStack, edi
        mov eax, edi
        rep movs byte ptr es:[edi], byte ptr fs:[esi]
        retn
ENDP

;--------------------------- rename file ------------------------------------
;don't needed with short jumps
w_asciiz_dsdx_esdi:       ;for rename file
        call GetAsciizLenPush
        push esi
        push edx
        mov  bx, p_esdi         ;word ptr DataPtr.DC_OffsetIndex, 0    ;es:edi internal index
        call w_asciiz_de
        pop  TransferStack
        pop  DataPtr.DC_EDX
        ret

;--------------------------- mouse set handler ------------------------------
MouseSetHandler PROC
        mov  ax, 900h
        int  31h                      ;disable virtual interrupts
        push eax                      ;save old virtual interrupts state
        movzx eax, DataPtr.DC_ES0
        mov  ebx, edx
        mov  esi, eax
        xchg MouseHookProc[0], ebx
        xchg MouseHookProc[4], esi
        mov  edi, cs
        cmp  di, ax
        mov  ecx, HookFromMouse32
        $ifnot jne
          cmp  edx, offset cs:DefMouseHookProc
          je @@DosCall
        $endif
        or   eax, edx
        mov  ecx, 0
        org $-4
LWord MouseCallBackX
        DW  seg dgroup16
        DW  ROffMouseRHandlerEntry
        jne  @@DosCall
        xor  ecx, ecx
@@DosCall:
        mov  DataPtr.DC_ES, cx
        shr  ecx, 16
        mov  word ptr DataPtr.DC_EDX, cx
        mov  al, byte ptr DataPtr.DC_EAX[0]
        call DosCall
        xchg DataPtr.DC_EDX, edx
        cmp  al, 14h            ;exchange function ?
        $ifnot jne
          mov  DataPtr.DC_EDX, ebx
          mov  DataPtr.DC_ES,  si
          cmp  ebx, offset cs:DefMouseHookProc
          $ifnot jne
            mov  edi, cs
            cmp  di, si
            $ifnot jne
              shl  edx, 16
              mov  dx, DataPtr.DC_ES
              mov  HookFromMouse32, edx
            $endif
          $endif
        $endif
        pop eax
        int 31h                ;restore old virtual interrupts state
        retn
        ENDP
DefMouseHookProc: retf
;------------------------------ Mouse callback ------------------------------
assume es:EGroup
MouseCallbackHandler:
        ;cld
        push es
        ;lodsd                           ;load return address
        pop  ds
        ;mov  ax, TransferSegment
        ;add  ax, TransferBufferSize/16
        ;shl  eax, 16
        ;mov  ax, OffMouseCallbackPlace+4-OffMouseRHandler
        mov  dword ptr ds:[edi].DC_IP, 0
        org $-4
        DW OffMouseCallbackPlace+4-OffMouseRHandler
;LWord MouseRHandlerSRef
        DW seg dgroup16
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
        mov  eax, ss
        mov  dword ptr [edi][MouseHandlerESP-MouseCallbackStruct], esp
        ;rep  movs dword ptr es:[edi], dword ptr ds:[esi]
        lar  eax, eax
        shr  eax, 23     ;copy segment-32 bit to carry;  S32Bit
        $ifnot jc
        movzx esp, sp
        $endif
        movzx eax, word ptr ds:[edi].DC_EAX
        movzx ebx, word ptr ds:[edi].DC_EBX
        movsx ecx, word ptr ds:[edi].DC_ECX
        movsx edx, word ptr ds:[edi].DC_EDX
        movsx esi, word ptr ds:[edi].DC_ESI
        movsx edi, word ptr ds:[edi].DC_EDI
        mov  es, FlatSelector
        push es
        pop  ds
        pushfd                   ;for compatibility with buggy hookers
        call fword ptr cs:MouseHookProc
        ;mov ax, 900h
        ;int 31h
        ;cli        ;hooker may enable interrupts, but callback
                   ;must disable interrupts before setting reenterancy flag
        mov  esp, cs:MouseHandlerESP
        pop  es
        pop  edi
        ;mov  byte ptr es:[edi][InMouseFlag-MouseCallbackStruct1], 1
        ;mov  byte ptr es:InMouseFlag, 1
        ;$endif
        iretd
assume  es:nothing

;--------------------- Macros for translation definitions -------------------
Segm EData
FnIdx = 1
RegFnIndex MACRO Name, P
        Name&P&Idx = FnIdx*2
        FnIdx = FnIdx+1
        DW P
        ifdef D4G
          DW Name-base_fn_addr
        else
          DW offset EGroup:Name
        endif
        ENDM

TransEntry MACRO Num, Fn, P
        local T
        T = $
        org CurTransTable+Num
        DB Fn&P&Idx
        org T
        ENDM

InternalIntNum = 0
DefTransTable MACRO Name, MaxNum
        Name label word
        DB MaxNum
        CurTransTable = $
        DB MaxNum+1 dup(0)
        EndTransTable = $
        ENDM

NHookedInterrupts = 3
p_ds = 200h
p_es = 0
p_ax = DC_EAX
p_cx = DC_ECX
p_dx = DC_EDX
p_bx = DC_EBX
p_bp = DC_EBP
p_si = DC_ESI
p_di = DC_EDI
p_dsdx = p_ds+p_dx
p_dsbx = p_ds+p_bx
p_esbx = p_es+p_bx
p_esdi = p_es+p_di
p_esdx = p_es+p_dx
p_dssi = p_ds+p_si
p_esbp = p_es+p_bp

;---------------------------indexes for translation tables-------------------
MethodTable0 LABEL DWORD
RegFnIndex w_ascii$,     p_dsdx
RegFnIndex wr_pas,       p_dsdx
RegFnIndex setdta,       p_dsdx
RegFnIndex r_ptr,        p_dsbx
RegFnIndex r_ptr,        p_esbx
RegFnIndex w_vect,       205h
RegFnIndex getdta,       p_esdx
RegFnIndex r_vect,       204h
RegFnIndex w_asciiz_de,  p_dsdx
RegFnIndex w_asciizc,    p_dsdx
RegFnIndex w_asciizc_zc, p_dsdx
RegFnIndex rw_file,      4001h
RegFnIndex rw_file,      3f00h
      ;read or write file
;RegFnIndex wr_64,        p_dssi      ;get cur dir
RegFnIndex allocmem,     100h        ;realloc mem
RegFnIndex freemem,      101h        ;realloc mem
RegFnIndex reallocmem,   102h        ;realloc mem
RegFnIndex w_asciiz_r_dta,p_dsdx     ;find first
RegFnIndex DTACall,      0           ;find next
RegFnIndex w_asciiz_dsdx_esdi,p_dsdx ;rename file
RegFnIndex r_createunic, p_dsdx      ;create unic
RegFnIndex MouseSetHandler, p_esdx
RegFnIndex DosExecute,   p_dsdx      ;exec
RegFnIndex ExitDPMI,     0
RegFnIndex wr_64,        p_esdx
RegFnIndex wr_bx,        p_esdx
;RegFnIndex GetExtVer,
RegFnIndex wr_17,        p_esdx
RegFnIndex wr_cx3,       p_esdx
RegFnIndex r_psp,        0
RegFnIndex wr_256,       p_esdi
RegFnIndex r_ptr_zc,     p_esdi
RegFnIndex VBE4F00,      p_esdi
RegFnIndex wr_ecx4,      p_esdi
RegFnIndex DosCallErr,   0
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
TransEntry 1Ah, setdta, p_dsdx     ;set dta
TransEntry 1Bh, r_ptr_zcd, p_dsbx  ;get FAT info
TransEntry 1Ch, onalnff_r_ptr_zcd, p_dsbx   ;get FAT info for spec drive
TransEntry 1Fh, onal0_r_ptr, p_dsbx;get DPB
TransEntry 25h, w_vect, 205h       ;set int vector
TransEntry 2Ah, dc_zc, 0           ;get system date
TransEntry 2Fh, getdta, p_esdx
TransEntry 32h, onal0_r_ptr, p_dsbx;get DPB
TransEntry 34h, r_ptr, p_esbx      ;get InDos flag
TransEntry 35h, r_vect, 204h       ;get interrupt vector
TransEntry 36h, onaxnff_zabcd, 0   ;get disk info
TransEntry 38h, r_country, p_dsdx  ;get/set country code
TransEntry 39h, w_asciiz_de, p_dsdx;Create subdir
TransEntry 3Ah, w_asciiz_de, p_dsdx;remove subdir
TransEntry 3Bh, w_asciizc, p_dsdx  ;set directory
TransEntry 3Ch, w_asciizc, p_dsdx  ;create file
TransEntry 3Dh, w_asciizc, p_dsdx  ;open file
TransEntry 3Eh, DosCallErr, 0      ;close file
TransEntry 3Fh, rw_file, 3F00h     ;read from file
TransEntry 40h, rw_file, 4001h     ;write to file
TransEntry 41h, w_asciiz_de, p_dsdx;delete file
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
TransEntry 56h, w_asciiz_dsdx_esdi,p_dsdx ;rename file
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
DefTransTable Int2144Table, 0Fh
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

comment %
DefTransTable Int2171Table,
TransEntry 39h, w_asciiz_de, p_dsdx   ;make   directory
TransEntry 3Ah, w_asciiz_de, p_dsdx   ;remove directory
TransEntry 3Bh, w_asciiz_de, p_dsdx   ;change directory
TransEntry 41h, w_asciiz_de, p_dsdx   ;delete file
TransEntry 43h, w_asciiz_de, p_dsdx   ;get/set attributes
;TransEntry 47h, w_asciiz_dexx, p_dsdx   ;get current directory
TransEntry 39h, w_asciiz_de, p_dsdx   ;make directory
;TransEntry

FindFirst:
        call PushStr
        mov  bx, xxx
        mov  ecx, xxx
        push xx xxx
        call ResStr
        call DosCallErr
        $ifnot jnz
          call PopStr
        $endif
        call Rest
        jmp  ZeroECX
FindNext:
        mov  ecx, xxx
        call ResStr
        call DosCallErr
        $ifnot jnz
          call PopStr
        $endif
        jmp  ZeroECX
Canonicalize:
        call PushStr
        mov  ecx, 261
        mov  bx, xxx
        call ResCallEPop

%


DefTransTable int33Table, 17h
TransEntry 09h, wr_64, p_esdx           ;set graphics cursor
TransEntry 0Ch, MouseSetHandler, p_esdx ;set mouse callback
TransEntry 14h, MouseSetHandler, p_esdx ;exchange mouse callback
TransEntry 16h, wr_bx, p_esdx
TransEntry 17h, wr_bx, p_esdx

DefTransTable int1010Table, 17h
TransEntry 2,   wr_17, p_esdx   ;set all palette
TransEntry 9,   wr_17, p_esdx   ;get all palette
TransEntry 12h, wr_cx3, p_esdx  ;set DAC block
TransEntry 17h, wr_cx3, p_esdx  ;get DAC block

;DefTransTable int101030Table, 0
;TransEntry 0, r_ptr, p_bpdi     ;get font pointer

DefTransTable int104FTable, 0Ah
TransEntry 0, VBE4F00, p_esdi     ;get
TransEntry 1, wr_256, p_esdi      ;get vmode info
TransEntry 9, wr_ecx4, p_esdi     ;
TransEntry 0Ah, r_ptr_zc, p_esdi




;-------------------------- Exception handlers ------------------------------
TermMsg DB 13,10
  DB 'Unhandled exception #', 2, ', error code ', 4, ' at ',4,':', 8, 13, 10
  DB 'eax=', 8,' ebx=', 8,' ecx=', 8,' edx=', 8, 13, 10
  DB 'esp=', 8,' ebp=', 8,' esi=', 8,' edi=', 8, 13, 10
  DB 'eflags=', 8, '  unrelocated eip=', 8, 13, 10, 0
Term1Msg DB '=',4,' base=',8,' limit=',8,' acc=',4, 13, 10, 0
Term2Msg DB 'cs',0,'ds',0,'es',0,'ss',0,'fs',0,'gs',0
TermStkcMsg DB '[ss:esp]: ',8,' ',8,' ',8,' ',8,' ',8,' ',8,' ',8,' ',13,10,0
TermInvSelMsg DB '=',4,' invalid selector', 13, 10, 0
TermEmpSelMsg DB '=',4,' null selector', 13, 10, 0
LByte ZeroDivMsg
  DB 'ZRDX runtime error: divide overflow', 13, 10, '$'
LByte TSOverflowMsg
  DB 'ZRDX runtime error: transfer buffer overflow', 13, 10, '$'
ESeg EData
;---------------------------exception 0 handler------------------------------
LLabel Exc0handler
        push eax edi es
        F=3*4
        mov edi, cs:ClientInt0Vector[4]
        mov eax, cs
        cmp ax, di
        mov eax, cs:ClientInt0Vector[0]
        $ifnot jne
        cmp eax, OffDefaultInt0Handler
        je  GoDefaultExcHandler
        $endif
        xchg ss:[esp+F].EXC_CS, edi
        xchg ss:[esp+F].EXC_EIP, eax
        push edi
        sub [esp+F+4].EXC_ESP, 3*4
        les edi, fword ptr [esp+F+4].EXC_ESP
        cld
        stosd
        pop eax
        stosd
        mov eax, [esp+F].EXC_EFlags
        stosd
        pop es edi eax
LLabel Exc3Handler
        retf
;LLabel Exc1Handler
;        push 1
;        jmp  GlobalExceptionEntry
;        and  byte ptr [esp][1].EXC_EFlags, not 1    ;clear TF
;        retf
        ;exc 0 start here
;------------------------entries for all exceptions--------------------------
ExceptionHandler PROC C
        ExcptNum = 0
        REPT 16
          IF ExcptNum GT 7 or ExcptNum EQ 6
            push ExcptNum
            jmp short GlobalExceptionEntry
          ENDIF
          ExcptNum = ExcptNum+1
        ENDM
GoDefaultExcHandler:
        pop es edi eax
        push 0
        ;jmp GlobalExceptionEntry
;------------------------- common exceptin handler --------------------------
GlobalExceptionEntry:
        push gs fs
        push [esp+3*4].EXC_SS     ;ss
        push es ds
        push eax                     ;reserve space for unrelocated eip
        push [esp+7*4].EXC_EFlags    ;eflags
        push edi esi ebp
        mov  ebp, esp                ;and use [ebp] later for dec size of code
        F = 11*4
        push [ebp+F].EXC_ESP         ;esp
        push edx ecx ebx eax
        mov  ax, 0 ;selector
LLabel SelReference1
               ;ra:2, ec, rip, rf, rsp
        mov  ds, eax

        ;push dword ptr [ebp+F].EXC_EIP ;eip
        mov  eax, dword ptr [ebp+F].EXC_EIP ;eip
        push eax
        sub  eax, ImageBaseAddr
        mov  ss:[ebp+4*4], eax
        push [ebp+F].EXC_CS        ;cs
        push [ebp+F].EXC_Errcode   ;err code
        push dword ptr [ebp+F-4]             ;exception num
        ;movzx eax, OrigVideoMode  ;
        ;and  al, 7Fh
        ;int  10h                        ;clear screen and restore initial video mode
        ;move stack content to new location
        push  ds
        or   edx, -1
        mov  ss:[ebp+F].EXC_CS, cs
        mov  eax, ExcReturnAddr
        mov  ss:[ebp+F].EXC_EIP, eax
        ;mov  ss:[ebp+F].EXC_EIP, offset EGroup:ExcHandler2Entry
        cmp  eax, offset EGroup:ExcHandler2Entry
        $ifnot jne
        mov  esi, esp
        mov  es, FlatSelector     ;TransferSelector
        movzx edi, TransferSegment
        shl  edi, 4
        add  edi, 1000h-200       ;allocate space for stack in TransferBuffer
        mov  TransferStack, 800h
        xor  ecx, ecx
        mov  cl, 21               ;number of dwords in exception state
        cld
        mov  ss:[ebp+F].EXC_SS, es
        mov  ss:[ebp+F].EXC_ESP, edi
        pushfd
        pop  ss:[ebp+F].EXC_EFLAGS   ;replace client eflags with my own
        rep  movs dword ptr es:[edi], dword ptr ss:[esi]
        ;mov  esp, esi
        $endif
        add  esp, 21*4
        retf           ;return to my secondary exception handler with my stack

ExcHandler2Entry:
        pop  ds
        cld               ;some dpmi hosts don't set eflags content correctly
        ;mov  ds, edx             ;reload my own ds, because some dpmi hosts
                                 ;destroy it during returning from exception
        mov  esi, offset EGroup:TermMsg
        IFNDEF Release
        mov ah, 0
        int 16h
        cmp al, 27
        je  @@Term
        ENDIF
        ;mov ax, 3
        ;int 10h
        call PrintFStr
        pop  eax       ;num
        pop  eax       ;err code
        pop  edi       ;cs
        sub  ecx, 7
        mov  esi, [esp+5*4]     ;esp
        mov  es, [esp+13*4]     ;ss
        mov  ExcReturnAddr, offset Egroup:ExcHandler3Entry
        $do
          mov  edx, dword ptr es:[esi]
ExcHandler3Entry:
          add  esi, 4
          mov  ss:[esp+ecx*4+7*4], edx
          inc ecx
        $enddo jnz
        mov  esi, offset EGroup:TermStkcMsg
        call PrintFStr
        add  esp, 11*4

        push edi         ;push cs again
        mov  ebp, 6
        mov  edi, offset EGroup:Term2Msg
        $do
        mov  esi, edi
        add  edi, 3
        call PrintFStr
        pop  ebx      ;selector
        lar  eax, ebx
        shr  eax, 8
        push eax
        xor  edx, edx
        xor  ecx, ecx
        mov  ax, 6
        int  31h
        shl  ecx, 16
        mov  cx, dx
        lsl  edx, ebx
        mov  esi, offset EGroup:Term1Msg
        push edx
        push ecx
        push ebx
        $ifnot jz
          mov  esi, offset EGroup:TermInvSelMsg
          cmp  bx, 3
          $ifnot ja
            mov  esi, offset EGroup:TermEmpSelMsg
          $endif
        $endif
        call PrintFStr
        add  esp, 16
        dec  ebp
        $enddo jnz
    @@Term:
        mov ax, 4CFEh
        pushfd
        push cs
        call int21Entry
        jmp $
CallInt10:
        pushfd
        call fword ptr ds:EID10.EID_OldIntVect
        retn
PrintFStr:
        pushad
        mov ebp, esp
        $do
        lodsb
        cmp al, 0
        $break je
        cmp al, 8
        $ifnot jbe
        mov ah, 0Eh
        mov bx, 07h
        ;int 10h
        call CallInt10
        $else jmp
        add  ebp, 4
        push dword ptr [ebp+32]
        movzx eax, al
        push eax
        call PrintN
        $endif
        $enddo jmp
        popad
        ret
ExceptionHandler ENDP
LLabel  Starter
        int 31h
        mov eax, ebx
        mov esi, ebx
        mov edi, ebx
        retf
ifndef Release
Print8  PROC PASCAL ;@@DW:DWORD
        push dword ptr [esp+4]
        push 8
        call PrintN
        pushad
        mov ax, 0E00h+' '
        call CallInt10
        ;int 10h
        popad
        retn 4
        ENDP
endif
PrintN  PROC PASCAL;, @@Digit:DWORD, @@N:DWORD
        push eax ecx
@@Digit EQU DWORD PTR ss:[esp+8].8
@@N     EQU DWORD PTR ss:[esp+8].4
        mov eax, @@Digit
        mov cl, 8
        sub cl, byte ptr @@N
        shl cl, 2
        rol eax, cl
        mov ecx, @@N
        $do
        rol eax, 4
        pushad
        and al, 1111b
        add al, '0'
        cmp al, '9'
        $ifnot jbe
        add al, 'A'-'9'-1
        $endif
        mov ah, 0Eh
        mov bx, 07h
        call CallInt10
        ;int 10h
        popad
        $enddo loop
        pop ecx eax
        retn 8
        ENDP

int75handler:              ;handler for numeric coprocessor interrupt
        push eax
        mov  al, 0
        out  0F0h, al
        mov  al, 20h
        out  0A0h, al
        out  020h, al
        pop  eax
        int  2
        sti
        iretd
TransferStackOverflow:
        mov  edx, OffTSOverflowMsg
        jmp  FatalExtExit
DPROC   DefaultInt0Handler
        mov  edx, OffZeroDivMsg
FatalExtExit:
        push cs
        pop  ds
        mov  ah, 9
        int  21h
        mov  ax, 4CFFh
        int  21h
        endp

eseg EText
