;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

DPMIFn MACRO Hi, Lo
        _T = $
        org offset Table2&Hi&Label+Lo*4
        DD _T-CurSegBase
        org _T
        ENDM

DefineDPMITable2 MACRO P0, P1
        LDWord Table2&P0
        Table2&P0&Label label byte
        IF P1 GT 0
          DD P1 dup(OffInvalidFunction)
        ENDIF
        ENDM

BuildDPMITable2N MACRO P0, P1
        DB P1
        ENDM

BuildDPMITable1 MACRO P0, P1
        DD OffTable2&P0
        ENDM

RegisterDPMITables MACRO P
        LByte DPMITable2N
        IRP T, <P>
        BuildDPMITable2N T
        ENDM
        align 4
        IRP T, <P>
        DefineDPMITable2 T
        ENDM
        LDWord DPMITable2
        IRP T, <P>
        BuildDPMITable1 T
        ENDM
        ENDM
Segm Text
RegisterDPMITables <<0,0Eh>,<1,03>,<2,6>,<3,7>,<4,1>,<5,4>,<6,5>,<7,4>,<8,1>,<9,3>,<A,1>,<B,4>>
;assume cs:dgroup, ss:dgroup, es:nothing, ds:dgroup
assume cs:Text, ss:nothing, es:nothing, ds:nothing
DPMIFrame   struc
        DPMIFrame_Ret DD ?
        DPMIFrame_EBP DD ?
        DPMIFrame_ESI DD ?
        DPMIFrame_DS  DD ?
        DPMIFrame_ES  DD ?
        DPMIFrame_FS  DD ?
        DPMIFrame_GS  DD ?
        DPMIFrame_SF  CFrame <>
DPMIFrame  ends
DPROC DPMIIntEntry
        cli
        push gs fs es ds esi ebp
        F = 6*4
        mov  ebp, ss
        cmp  ah, 0Bh
        mov  ds, ebp
        movzx esi, al
        movzx ebp, ah
        ja   @@FnNumError
        cmp  al, byte ptr ss:DPMITable2N[ebp]
        mov  ebp, ss:DPMITable2[ebp*4]
        jae  @@FnNumError
        call dword ptr [ebp+esi*4]
        ;clc
        or   ebp, ebp           ;faster then direct clc
DPMIIret:
        lds  esi, fword ptr ss:[esp+F].CFrESP ;load client ss:esp
        lea  esi, [esi+12]            ;instread of add to save CF
        push ds                      ;client ss
        push esi                     ;client new esp

        mov  ebp, ds:[esi-4]         ;load eflags from client iret frame
        rcr  ebp, 1                  ;copy CF to BIT0 of ebp
        rol  ebp, 1
        push ebp                     ;eflags
        push dword ptr ds:[esi-8]    ;cs
        push dword ptr ds:[esi-12]   ;eip
        mov  es, ss:[esp+5*4+3*4]
        mov  fs, ss:[esp+5*4+4*4]
        ;mov  ebp, ss:[esp+5*4+5*4]
        mov  gs, ss:[esp+5*4+5*4]
        ;mov  gs, ebp
        mov  ebp, ss:[esp+5*4]
        lds  esi, ss:[esp+5*4+4]
        iretd

DPMIError3N:
        pop ebp
DPMIError2N:
        pop ebp
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
DPMIError1:          ;pop 1 dword ...
        pop ebp          ;drop return address
        jmp DPMIError
LLabel  InvalidFunction
@@FnNumError:
        mov al, 1
DPMIError:
        mov ah, 80h
DPMIErrorN:
        stc
        jmp DPMIIret
FnRet   MACRO
        ret
        ENDM

DPMIFn 0 00
        push ecx
        call AllocSelectors
        jc   DPMIError1
        FnRet

DPMIFn 0 01
        cmp  bx, word ptr [esp].DPMIFrame_SF.CFrSS
        je   DPMIError1InvSel       ;cannot free current client stack !
        call CheckSelector
FreeDescriptor1:
        btr  dword ptr ds:LDTFree, esi;clear alloc bit in flag vector
        mov  byte ptr [ebp].5, 0      ;clear descriptor to prevent next use
        ;clear all freed client selectors on stack
        lea  esi, [ebp-OffLDT+7]
        push eax
        mov  ebp, -4*4
        $do
          mov  eax, esi
          xor  eax, [ebp+esp+4*4+4].DPMIFrame_DS
          and  eax, 0FFFCh
          $ifnot jnz
            mov  dword ptr [ebp+esp+4*4+4].DPMIFrame_DS, eax
          $endif
          add  ebp, 4
        $enddo jnz
        pop eax
        FnRet

DPMIFn 0 02
        movzx ebp, word ptr LDTLimit
        shr  ebp, 3
        $do
          bt  LDTDOS, ebp
          $ifnot jnc
            mov  esi, ss:LDT[ebp*8]
            shr  esi, 4
            cmp  bx, si
            je   @@RetThis
          $endif
        $while
          dec  ebp
        $enddo jnz
        push 1
        call AllocSelectors
        jc   DPMIError1
        bts  LDTDOS, esi
        movzx esi, bx
        shl  esi, 4
        mov  word ptr [ebp].0, 0FFFFh
        mov  [ebp].2, esi
        mov  byte ptr [ebp].5, 0F3h    ;Data R/W PL3  mode32
        ;mov  byte ptr [ebp].7, 0
        FnRet
        @@RetThis:
        lea ebp, [ebp*8+7]
        mov ax, bp
        FnRet

DPMIFn 0 03
        mov ax, 8
        FnRet

DPMIFn 0 06
        call CheckSelector  ;(bx)
        mov  ch, [ebp].7
        mov  cl, [ebp].4
        mov  dx, [ebp].2
        FnRet

DPMIFn 0 07
        call CheckSelector  ;(bx)
        mov  [ebp].7, ch
        mov  [ebp].4, cl
        mov  [ebp].2, dx
        FnRet

DPMIFn 0 08
        call CheckSelector
        push edx
        shl  edx, 16
        mov  dx, cx
        cmp  cx, 0Fh
        $ifnot jbe
          ror  edx, 12
          add  dx, 10h
          jnc  DPMIError2InvVal2
          or   dl, 80h
        $endif
        and  byte ptr ss:[ebp].6, 70h
        or   byte ptr ss:[ebp].6, dl
        pop  edx
        mov  ss:[ebp], dx
        comment ^
        push ecx
        cmp  cx, 0Fh
        $ifnot jbe
        shl  ecx, 16
        mov  cx, dx
        rol  ecx, 4
        xor  cx, 0FFF0h
        test cx, 0FFF0h
        jne  DPMIError2InvVal1
        ;and  cl, 0Fh
        or   cl, 80h
        and  byte ptr ss:[ebp].6, 70h
        or   byte ptr ss:[ebp].6, cl
        shr  ecx, 16
        mov  word ptr ss:[ebp], cx
        $else jmp
          mov  word ptr ss:[ebp], dx
          and  byte ptr ss:[ebp].6, 70h
          or   byte ptr ss:[ebp].6, cl
        $endif
        pop  ecx ^
        FnRet
@@CheckSelectorInCX:
        lar  esi, ecx
        jnz  DPMIError2InvVal
        shr  esi, 9
        and  esi, 0FEh shr 1
        cmp  esi, 0FAh shr 1
        jne  DPMIError2InvVal
        ret
DPMIError2InvVal2:
        pop  edx
        push ebp
DPMIError2InvVal1:
        pop  ebp
DPMIError1InvVal1:
        jmp  DPMIError1InvVal
;check access right
@@CheckACC:
        xchg ax, si
        xor  al, 70h
        test al, 70h
        jnz  DPMIError2InvVal  ;this bits must be 1
        test al, 80h          ;descriptor is prezent?
        $ifnot jz             ;do not check other if not prezent
          test ah, 20h
          jnz  DPMIError2InvVal
          and al, 1110b   ;first step - valid code descriptor ?
          cmp al, 1010b   ;code-must be nonconform and readable
          $ifnot je        ;not a valid code
            test al, 1000b
            jnz DPMIError2InvVal  ;else bad descriptor
          $endif
        $endif
        xchg ax, si
        retn

DPMIFn 0 09
        call CheckSelector  ;(bx)
        mov  esi, ecx
        call @@CheckACC
        push ecx
        mov  [ebp].5, cl
        and  ch, 11010000b
        and  byte ptr ss:[ebp].6, 101111b
        or   ss:[ebp].6, ch
        pop  ecx
        FnRet

DPMIFn 0 0Ah
        call CheckSelector  ;(bx)
        push dword ptr ss:[ebp]
        push edx
        mov  edx, dword ptr [ebp+4]
        and  dh, not 1100b     ;now data with up extend
        or   dh, 10b           ;enable write
        push 1
        call AllocSelectors
        mov  esi, edx
        pop  edx
        jc   DPMIError2
        mov  [ebp+4], esi
        pop  dword ptr ss:[ebp]
        FnRet

DPMIFn 0 0Bh
        call CheckSelector
        mov  esi, [ebp]
        mov  es:[edi], esi
        mov  esi, [ebp].4
        mov  es:[edi].4, esi
        FnRet

DPMIFn 0 0Ch
        call CheckSelector
        movzx esi, word ptr es:[edi].5
        call @@CheckACC
        mov esi, es:[edi]
        mov [ebp], esi
        mov esi, es:[edi].4
        mov [ebp].4, esi
        FnRet
DPMIFn 0 0Dh
        movzx esi, bx
        xor  esi, 7
        test esi, 7
        jnz  DPMIError1InvSel    ;not an LDT PL3 selector
        cmp  esi, 16*8+8
        jae  DPMIError1InvSel    ;too large selector
        shr  esi, 3
        jz   DPMIError1InvSel    ;null selector
        bts  LDTFree, esi
        jc   DPMIError1InvSel    ;not free selector
        ret

DPMIFn  1 0
        push 1
        call AllocSelectors
        jc   DPMIError1
        push eax       ;save selector
        mov  ah, 48h   ;Get Block
        ;mov  ax, 0
        ;mov  ss, eax
        call Dos1Call
        $ifnot jnc
          btr LDTFree, esi           ;clear alloc bit
          and dword ptr[ebp].4, 0    ;clear descriptor
          jmp DPMIError2N
        $endif
        pop  esi                     ;add esp, 4
        mov  dx, si
        ;jmp  SetupDOSSelector
;ebp - descriptor, ax - RM segment, bx - size in paragraphs
DPROC SetupDOSSelector
        push eax ebx
        movzx eax, ax
        shl eax, 4                 ;convert segment to LA
        mov [ebp].2, eax           ;write base
        shl ebx, 4                 ;convert size to bytes
        dec ebx                    ;if bx==0, DOS MUST return error
        mov [ebp], bx
        shr ebx, 16
        and ebx, 0Fh
        ;or  bl, 40h                 ;32-bit selector
        mov word ptr [ebp].6, bx
        mov byte ptr [ebp].5, 0F3h  ;data R/W PL3
        pop ebx eax
        ret
        ENDP

DPMIFn 1 2
        call CheckSelectorInDX
        push eax
        call @@GetSegment
        mov  ah, 4Ah
        call Dos1Call
        jc   DPMIError2N
        ;mov  eax, esi          ;reload segment from esi
        pop  eax
        jmp SetupDOSSelector
        ;FnRet

DPMIFn  1 1
        call CheckSelectorInDX
        push esi
        call @@GetSegment
        mov  ebp, eax
        cmp  dx, word ptr [esp].DPMIFrame_SF.CFrSS
        je   DPMIError1           ;cannot free current client stack !
        mov  ah, 49h              ;Free block
        call Dos1Call
        jc   DPMIError2N          ;@@Err101
        pop  esi
        xchg eax, ebp
        lea  ebp, LDT[esi*8]
        jmp  FreeDescriptor1
;convert descriptor based on ebp to RM segment in si and move eax to ebp
@@GetSegment:
        mov  esi, [ebp].2
        test esi, 1111b      ;Base must be segment aligned
        jnz  @@Err101_
        cmp  byte ptr [ebp].7, 0
        jne  @@Err101_       ;too large base
        shl  esi, 8          ;clear 8 hi bits
        shr  esi, 12         ;convert to segment
        cmp  esi, 10000h
        jae  @@Err101_       ;too large base
        ret
@@Err101_:mov ax, 9          ;return "invalid segment"
        jmp DPMIError3N

; call int 21h from PL1
; RM<->PM transfers:
; before call: si -> es, ax, bx
; after return: ax, bx
DPROC Dos1Call
        ;push ebp
        ;mov  ebp, ds:[21h*4]        ;int 21
        push dword ptr ds:[21h*4]
        call DosPCall
        ;pop  ebp
        retn
;call RM proc from PL1
;before call: si -> es, eax, ebx will be transferred to RM
;            ebp - rm proc far address
;after return: eax, ebx will be transferred from RM
;modify ebp
DosPCall:
        push esi edi ebp fs gs
        F = 6*4     ;"pushad" + Dos1Call ret
        mov  ebp, OffRMStack
        RRT
        mov  edi, KernelStack1
        push edi                 ;save Kernel1Stack
        F = F+4
        push dword ptr ds:[edi-8]      ;client ESP    ;setup client stack
        pop  dword ptr ss:[ebp][OffPMStack-OffRMStack]
        push dword ptr ds:[edi-4]      ;elient SS
        pop  dword ptr ss:[ebp][OffPMStack-OffRMStack][4]
        mov  edi, [ebp]
        push edi                 ;save old real stack
        F = F+4
        movzx ebp, di
        shr  edi, 16
        dec  bp
        sub  ebp, size VMIStruct-1
        ;jb   @@RStackOverflow
        push edi
        shl  edi, 4
        add  edi, ebp
        pop  [edi].VMI_SS
        add  ebp,  VMI_EAX
        mov  [edi].VMI_ESP, ebp
        mov  [edi].VMI_ES, esi     ;es
        IFDEF VMM
          mov  [edi].VMI_DS, esi     ;es
        ENDIF
        mov  dword ptr [edi].VMI_EAX, eax
        mov  eax, ss:[esp+F]
        mov  dword ptr [edi].VMI_IP, eax
        mov  dword ptr [edi].VMI_EndIP, 0
        org  $-4
        DW   Dos1CallRetSwitchCode, seg DGROUP16
        ;clc           ;not needed: add ebp, VMI_EAX always clear cf
        pushfd
        pop  eax
        mov  [edi].VMI_Flags, ax
        mov  [edi].VMI_EndFlags, ax
        mov  KernelStack1, esp
        mov  eax, edi
        jmp  SwitcherToVM

LLabel Dos1CallRet1
        push ss
        pop  ds
        push ss
        pop  es
        pop  RMStack
        RRT
        pop  KernelStack1
        mov  eax, [ebp].RMS_EAX
        bt   [ebp].RMS_Flags, 0  ;copy carry flag from DOS to CF
        pop  gs fs ebp edi esi
        retn 4
        ENDP

;simple jmp to fixed point at PL1
DPROC Dos1CallRet
        push Data1Selector
        push KernelStack1
        push Code1Selector
        push OffDos1CallRet1
        retf                   ;retf to pl1
        ENDP
;bx - selector
;output: ebp -> pointer to descriptor or error handler call if selector incorrect
;destoy: esi
CheckSelectorInDX:
        movzx esi, dx
        jmp short CheckSelector1

DPROC CheckSelector
        movzx esi, bx
CheckSelector1:
        ;xor  esi, 7
        test esi, 4
        jz  DPMIError2InvSel     ;client selector must points to LDT RPL 0-3
        shr esi, 3
        bt  dword ptr LDTFree, esi
        jnc DPMIError2InvSel
        bt  dword ptr LDTDOS, esi
        jc  DPMIError2InvSel
        lea ebp, LDT[esi*8]
        retn
        ENDP

;stack - number of selectors to allocate
;return: ax - first allocated selector, ebp - pointer to descriptor
;esi - index of descriptor
DPROC   AllocSelectors
        @@T equ ebp
        @@TW equ bp
        @@T1 equ esi
        cmp  word ptr [esp+4], 1   ;Error if less then 1 selector requred
        mov  al, 21h
        jb   @@Err
        mov  al, 11h
        mov  @@T1, 16-1
        $do
 @@NextScan:
        inc  @@T1
        cmp  @@T1, 2000h
        jae  @@Err
        bt   dword ptr LDTFree, @@T1
        $enddo jc
        movzx @@T, word ptr [esp+4]   ;Count
        $do  jmp
        cmp  @@T1, 2000h
        jae  @@Err
        bt   dword ptr LDTFree, @@T1
        jc   @@NextScan
        $while
        inc  @@T1
        dec  @@T
        $enddo jnz
;interval found
        lea  @@T, [@@T1*8+7]
        sub  @@TW, LDTLimit
        $ifnot jbe
          push @@T
          push @@T
          movzx @@T, LDTLimit
          add  @@T, OffLDT
          push @@T
          push 0
          call AllocPages
          jc   @@Err
          pop  @@T
          add  LDTLimit, @@TW
          mov  @@TW, LDTSelector
          DB   CallFarCode
          DD   0
          DW   LoadLDTGateSelector
        $endif
        movzx @@T, word ptr [esp+4]
        $do
          dec  @@T1
          bts  dword ptr LDTFree, @@T1
          and  LDT[@@T1*8], 0
          mov  dword ptr LDT[@@T1*8+4], 40F200h
          dec  @@T
        $enddo jnz
        lea  ebp, [@@T1*8+7]
        mov  ax, bp
        lea  ebp, LDT[ebp-7]   ;ebp - pointer to first descriptor
        clc
@@ret:
        retn 4
@@Err:
        stc
        jmp  @@ret
        ENDP

DPMIFn 2 0
        movzx ebp, bl
        mov  cx, [ebp*4][2]
        mov  dx, [ebp*4][0]
        FnRet
;set real mode vector
DPMIFn 2 1
        movzx ebp, bl
        mov  [ebp*4][2], cx
        mov  [ebp*4],    dx
        FnRet
;get exception handler
DPMIFn 2 2
        movzx ebp, bl
        cmp  bl, 32
        jae  DPMIError1InvVal
        lea  ebp, ClientExc[ebp*8]
        mov  edx, [ebp]
        mov  cx,  [ebp][4]
        FnRet
;set exception handler
DPMIFn 2 3
        cmp  bl, 32
        jae  DPMIError1InvVal
        call @@CheckSelectorInCX
        movzx ebp, bl
        lea  ebp, ClientExc[ebp*8]
        mov  [ebp], edx
        mov  [ebp].4, cx
        FnRet
;get PM interrupt vector
DPMIFn 2 4
        movzx ebp, bl
        shl  ebp, 3
        mov  edx, dword ptr ss:ClientIDT[ebp]
        mov  cx,  word ptr  ss:ClientIDT[ebp][4]
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
DPMIFn 2 5
        call @@CheckSelectorInCX
        movzx ebp, bl
        bt  PassupIntMap, ebp
        $ifnot jnc
        movzx esi, byte ptr ss:FirstTrap3[OffDefIntTrap3][ebp*4][3] ;load linked RM int number
        bt  PassupIntMap, esi      ;
        $ifnot jnc
          push eax edi
          xor  eax, eax
          mov  edi, esi             ;calculate relative index of passup
          $do
            bt   PassupIntMap, edi
            adc  eax, 0
            dec  edi
          $enddo jns
          lea edi, AutoPassupRJmps[eax+eax*4-5]
          RRT
          cmp  cx, Trap3Selector
          jne  @@SetClientVect
          cmp  edx, 400h            ;edx points to Trap3 call gate ?
          jb   @@SetDefaultVect
@@SetClientVect:
          mov  byte ptr [edi], PushWCode
          mov  byte ptr [edi].3, JmpShortCode
          lea  eax, [eax+eax*4+(AutoPassupRJmps-RMS_RMHandler)]
          neg  eax
          mov  [edi].4, al
          lea  eax, [ebp+FirstPassupSwitchCode+FirstSwitchCode+3]
          mov  [edi].1, ax
          bts  RIntFlags, esi  ;set flag for routing to prevision RM vector
          $ifnot jmp
@@SetDefaultVect:
          mov  byte ptr [edi], JmpFarCode
          mov  eax, SavedRealVectors[esi*4]
          mov  [edi].1, eax
          btr  RIntFlags, esi  ;clear flag for routing to current RM vector
          $endif
          pop  edi eax
        $endif
        $endif
        shl  ebp, 3
        mov  dword ptr ss:ClientIDT[ebp],   edx
        mov  word ptr  ss:ClientIDT[ebp][4], cx
        cmp  ebp, 7*8   ;set int7 always in IDT to allow fast FPU emulation
        je   @@SetInt7
        test byte ptr ss:IDT[ebp][2], 10b  ;PL0/1 handler installed ?
        $ifnot jz                          ;else set in IDT
@@SetInt7:
          mov  word ptr ss:IDT[ebp][2], cx
          mov  word ptr ss:IDT[ebp], dx
          shld esi, edx, 16               ;place high 16 bits of edx to si
          mov  word ptr ss:IDT[ebp][6], si
        $endif
        FnRet

DPMIFn 9 0
;clear virtual interrupts
        lds  esi, fword ptr [esp].DPMIFrame_SF.CFrESP
        mov  al, byte ptr ds:[esi].9
        shr  al, 1
        and  al, 1
        and  byte ptr ds:[esi].9, not 2    ;clear IF in client iret frame
        FnRet

DPMIFn 9 1
;set virtual interrupts
        lds  esi, fword ptr [esp].DPMIFrame_SF.CFrESP
        mov  al, byte ptr ds:[esi].9
        shr  al, 1
        and  al, 1
        or   byte ptr ds:[esi].9, 2    ;clear IF in client iret frame
        FnRet

DPMIFn 9 2
;get virtual interrupts
        lds  esi, fword ptr [esp].DPMIFrame_SF.CFrESP
        mov  al, byte ptr ds:[esi].9
        shr  al, 1
        and  al, 1
        FnRet

LLabel APIEntryPoint
        stc
        retf

DPMIFn A 0
        mov edi, OffAPIEntryPoint
        mov dword ptr [esp].DPMIFrame_ES, Code3Selector
        FnRet

DPMIFn 6 4
        xor bx, bx
        mov cx, 1000h
IFNDEF VMM
  DPMIFn 6 0
ENDIF
DPMIFn 6 1
DPMIFn 6 2
DPMIFn 6 3
DPMIFn 7 2
DPMIFn 7 3
        FnRet

DPMIFn 5 0
        pushad
        xor  eax, eax
        dec  eax
        mov  ecx, 30h/4
        cld
        rep  stosd
        xor  edx, edx
        mov  dword ptr es:[edi-30h].20h, edx ;swap file size
        mov  ax, 0DE03h      ;get VCPI free pages
        cmp  VCPIMemAvailable, dl
        $ifnot je
          VCPITrap
        $endif
        add  edx, nFreePages ;add free pages in my pool
        add  edx, FreeXMSCount
        RRT
        mov  eax, TotalVCPIPages
        add  eax, TotalXMSPages
        RRT
        IFNDEF Release
          ;mov eax, 3300
          ;mov edx, 3300
          ;add  eax, 10000
          ;add  edx, 10000
        ENDIF
        mov  [edi-30h].18h, eax
        mov  [edi-30h].0Ch, eax
        mov  [edi-30h].4, edx ;maximum unlocked page allocation
        mov  [edi-30h].8, edx ;maximum locked page allocation(same)
        mov  [edi-30h].10h, edx
        mov  [edi-30h].14h, edx
        mov  [edi-30h].1Ch, edx
        mov  eax, edx
        shr  eax, 10
        inc  eax
        inc  eax
        sub  edx, eax
        $ifnot ja
          xor edx, edx
        $endif
        shl  edx, 12         ;convert pages to bytes
        mov  [edi-30h], edx  ;maximum free block
        popad
        FnRet


;get version
DPMIFn 4 00
        mov  ax, 9h
        mov  bx, 1
        mov  cl, CPUType
        mov  dx, 870h
        FnRet

DPMIFn 3 06
        mov  cx, RawSwitchCode
        mov  edi, OffPMRawSwitchTrap3
@@L045:
        mov  word ptr [esp].DPMIFrame_ESI, Trap3Selector
        mov  bx, seg DGROUP16
        FnRet
DPMIFn 3 05
        mov  ax, 12
        mov  cx, ROffRMSaveState
        mov  edi, OffPMSaveStateTrap3
        jmp  @@L045
DPMIFn 3 00
DPMIFn 3 01
DPMIFn 3 02
        @@CSR equ esi
        lds  @@CSR, fword ptr [esp].DPMIFrame_SF.CFrESP  ;client stack
        mov  ebp, OffRMStack
        RRT
        sub  @@CSR, size RMIEStruct - 12
        mov  [ebp][OffPMStack-OffRMStack], @@CSR
        mov  word ptr [ebp][OffPMStack-OffRMStack][4], ds
        @@CS equ ds:[@@CSR]

;Save all PM registers on client stack
        mov  @@CS.RMIE_EDX, edx
        mov  @@CS.RMIE_EAX, eax
        mov  edx, [esp].DPMIFrame_EBP ;ebp, saved on current stack
        mov  @@CS.RMIE_EBX, ebx
        mov  @@CS.RMIE_EBP, edx
        mov  @@CS.RMIE_ECX, ecx
        mov  edx, [esp].DPMIFrame_ESI ;esi, saved on current stack
        push  eax
        F = 4
        mov  @@CS.RMIE_ESI, edx
        mov  @@CS.RMIE_EDI, edi            ;@@CS[26] - pointer to client DC_Struct
        mov  edx, [esp+F].DPMIFrame_DS  ;@@CS[20] - saved client DS:ESI
        movzx ecx, cx
        mov  @@CS.RMIE_DS, dx
        shr  al, 1
        mov  ebp, ss:[ebp]              ;RMStack->ebp
        sbb  edx, edx                   ;expand CF to edx
        mov  @@CS.RMIE_ES, es
        mov  @@CS.RMIE_FS, fs
        mov  @@CS.RMIE_GS, gs
        mov  @@CS.RMIE_RealStack, ebp
;calculate iret frame additional length(only for iret forms)

;free: eax, edx, esi, ebp
        inc  edx       ;convert to 1/0
        mov  esi, dword ptr es:[edi].DC_SP
        shl  edx, 1
                      ;edx == 2 if al==(0|2)
;calculate RM stack frame lenght
        or   esi, esi
        lea  eax, [ecx*2+edx+size VMIStruct-2-1] ;full stack frame length-1  -> ax
;replace kernel stack on client's if it is nonzero
        $ifnot jz
          mov  ebp, esi
        $endif
;calculate RM stack linear address
        movzx esi, bp
        dec  si
        sub  esi, eax
        jb   @@RStackOverflow
        shr  ebp, 16
        mov  eax, ebp
        shl  ebp, 4
        add  ebp, esi
        mov  [ebp].VMI_SS, eax   ;RM SS
        add  esi, VMI_EAX
        mov  [ebp].VMI_ESP, esi  ;RM ESP

;copy parameters
        $ifnot jcxz
          mov  esi, [esp+F].DPMIFrame_SF.CFrESP   ;reload client esp to esi
          push edi
          push es
          add  esi, 12                            ;interrupt frame length
          lea  edi, [ebp+edx+size VMIStruct-2]
          push ss
          pop  es
          cld
          rep  movs word ptr es:[edi], ds:[esi]
          pop  es
          pop  edi
          ;add  ebp, edx
          ;$do
          ;  mov  ax, ds:[esi+ecx*2-2+12]    ;get parameter from client stack
          ;  mov  ss:[ebp+ecx*2-2+size VMIStruct-2], ax ;put it on the RM stack
          ;$enddo loop
          ;sub  ebp, edx
        $endif
;set flags value for RM iret, if requered
        or   edx, edx          ;interrupt stack frame mode ?
        push es
        pop  ds
        mov  dx, [edi].DC_FLAGS
        $ifnot jz
          mov [ebp].VMI_EndFlags, dx ;put flags to RM stack iret frame
          and dh, not 3       ;clear TF and IF for initial flags
        $endif
        pop  eax               ;saved client eax
        mov  [ebp].VMI_Flags, dx    ;set initial flags

        or   al, al            ;Fn number == 0  ?
        mov  edx, dword ptr [edi].DC_IP
        $ifnot jnz
          movzx edx, bl
          mov  edx, ss:[edx*4]  ;call CS:IP from current interrupt vector for function 0 only
        $endif
        mov  dword ptr [ebp].VMI_EndIP, 0
        org  $-4
        DW RMIERetSwitchCode, seg DGROUP16

;ss:ebp - RM switch stack, ds:edi - dos call struct, edx - start address
SwitchToVMWithTransfer:
        movzx eax, word ptr [edi].DC_GS
        mov  dword ptr[ebp].VMI_IP, edx
;prepare VCPI stack frame
        movzx ecx, [edi].DC_FS
        mov  [ebp].VMI_GS, eax
        movzx edx, [edi].DC_ES
        mov  [ebp].VMI_FS, ecx
        movzx eax, [edi].DC_DS
        mov  [ebp].VMI_ES, edx
        mov  [ebp].VMI_DS, eax

;load all other registers from DPMI table
        mov  edx, [edi].DC_EAX
        mov  eax, ebp
        mov  [ebp].VMI_EAX, edx

        mov  ebx, [edi].DC_EBX
        mov  ecx, [edi].DC_ECX
        mov  edx, [edi].DC_EDX
        mov  esi, [edi].DC_ESI
        mov  ebp, [edi].DC_EBP
        mov  edi, [edi].DC_EDI
        VCPICallTrap
        ;jmp  SwitcherToVM
@@RStackOverflow:
        pop  eax
        mov  esi, [esp].DPMIFrame_SF.CFrESP  ;@@CSR
        mov  edx, @@CS[12-size RMIEStruct].RMIE_EDX
        mov  ecx, @@CS[12-size RMIEStruct].RMIE_ECX
        jmp  DPMIError1
;allocate RM callback
DPMIFn 3 3
        mov  esi, (nMaxCallbacks * size CBTStruct)/3
        $do
          sub  esi, 4; (size CBTStruct)/3
          jb   DPMIError1                    ;free callback not found
          cmp  CallbacksTable[esi+esi*2].CBT_CS, 0
        $enddo jne
        mov  ebp, [esp].DPMIFrame_DS
        or   ebp, ebp
        je   DPMIError1
        lea  dx, [si+FirstSwitchCode+MaxSystemSwitchCode]
        lea  esi, CallbacksTable[esi+esi*2]
        mov  [esi].CBT_CS, bp
        mov  ebp, [esp].DPMIFrame_ESI
        mov  [esi].CBT_EIP, ebp
        mov  [esi].CBT_SPtrOff, edi
        mov  [esi].CBT_SPtrSeg, es
        mov  cx, seg DGROUP16
        FnRet
;Free RM callback
DPMIFn 3 4
        cmp  cx, seg DGROUP16
        jne  DPMIError1
        movzx esi, dx
        lea  esi, [esi+esi*2-(FirstSwitchCode+MaxSystemSwitchCode)*3]
        test esi, 3         ;address must be dword aligned
        jne  DPMIError1
        cmp  esi, nMaxCallbacks*(size CBTStruct/3)
        jae  DPMIError1
        and  dword ptr CallBacksTable[esi].CBT_CS, 0
        FnRet
ENDP

comment #
Phisical address mapping:

PHMap(){
  MapRegionStart = lMapRegionStart&~0xFFF;
  MapRegionEnd   = (lMapRegionStart+lMapRegionSize+0xFFF)&~0xFFF;

  NewLastMappedPage = LastMappedPage - (MapRegionEnd - MapRegionStart)
  if(NewLastMappedPage < XXX)error();
  while(LastMappedPageDirIndex> NewMappedPageDirIndex){
    AllocPages();
  }
  TMappedPage = LastMappedPage>>12;
  LastMappedPage = NewMappedPage;
  for(;MapRegionEnd > MapRegionStart;){
    TMappedPage--;
    IndexOnDir  = LastMappedPage>>22;
    IndexOnPage = TMappedPage&0x3FF;
    FreePageRef = Dir[IndexOnDir];
    FreePage[IndexOnPage] = (MapRegionEnd -= 0x1000);
  }
  return LastMappedPage+(lMapRegionStart & 0xFFF);
}
#
DPROC PhMap
@@MapRegionStart equ eax
@@MapRegionStartB equ al
@@MapRegionEnd   equ ebp
@@LastDI         equ edx
@@FirstDI        equ @@MapRegionStart
@@TMappedPage    equ esi
@@T              equ @@LastDI

DPMIFn 8 0
        pushad
        push ds
        pop  es
        shl  ebx, 16
        mov  esi, [esp+8*4].DPMIFrame_ESI
        shl  esi, 16
        mov  bx, cx
        mov  si, di
        mov  @@MapRegionStart, ebx
        and  @@MapRegionStart, not 0FFFh
        lea  @@MapRegionEnd, [ebx+esi+0FFFh]
        xor  ebx, @@MapRegionStart
        and  @@MapRegionEnd, not 0FFFh
        push @@MapRegionStart
        push @@MapRegionEnd
        sub  @@FirstDI, @@MapRegionEnd
        mov  @@LastDI, LastMappedPage
        add  @@FirstDI, @@LastDI
        jnc  @@Err
        mov  ecx, RootMCB.MCB_Prev
        cmp  @@FirstDI, [ecx].MCB_EndOffset
        jb   @@Err
        push @@FirstDI
        ;uses @@FirstDI, @@LastDI, ecx, esi, edi, ebp
        shr  @@FirstDI, 22
        shr  @@LastDI, 22
        $do  jmp
        IFNDEF VMM
          lea  edi, PageDir[@@FirstDI*4]
          mov  ecx, @@LastDI
          sub  ecx, @@FirstDI
          call XAllocPages
          jz   @@Recover
          add  @@FirstDI, ecx
        ELSE
          push eax
          call alloc_page
          xchg edi, eax
          pop  eax
          jc   @@Recover
          mov  PageDir[@@FirstDI*4], edi
          inc  @@FirstDI
        ENDIF
        $while
        cmp  @@LastDI, @@FirstDI
        $enddo jne
        pop  @@TMappedPage
        pop  @@MapRegionEnd
        pop  @@MapRegionStart
        add  ebx, @@TMappedPage
        mov  word ptr [esp].PA_ECX, bx
        shr  ebx, 16
        mov  word ptr [esp].PA_EBX, bx
        mov  LastMappedPage, @@TMappedPage
        mov  @@MapRegionStartB, 67h
LLabel PatchPoint4
        shr  @@TMappedPage, 12
        $do  jmp
        mov  @@T, @@TMappedPage
        shr  @@T, 10
        cli
        mov  @@T, PageDir[@@T*4]
        mov  PageTableEntry, @@T
        InvalidateTLB
        mov  @@T, @@TMappedPage
        and  @@T, 3FFh
        mov  PageTableWin[@@T*4], @@MapRegionStart
        add  @@MapRegionStart, 1000h
        sti
        inc  @@TMappedPage
        $while
        cmp  @@MapRegionEnd, @@MapRegionStart
        $enddo ja
        popad
        retn
@@Recover:
        xchg eax, edx ;write eax to edx
        pop  ebx
        call FreeHMPages
@@Err:  pop  eax
        pop  eax
        popad
        jmp DPMIError1

        ENDP
;ebx - first page
;edx(@@LastDI) - last page
FreeHMPages:
        shr  ebx, 22
        $do jmp
          IFNDEF VMM
            xor  ecx, ecx
            lea  esi, PageDir[ebx*4]
            inc  ecx
            call XFreePages
          ELSE
            push PageDir[ebx*4]
            call free_page
          ENDIF
          inc  ebx
        $while
          cmp  ebx, edx
        $enddo jne
        ret

ESeg Text
