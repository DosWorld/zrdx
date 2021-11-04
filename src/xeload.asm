;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov
;                     Loader for XE packed modules
;                  This code is based on XELOADER stub,
;             written by Kevin Tseng, the author of XLE packer


xeh_ID                  = 0
xeh_version             = 2
xeh_image_offset        = 4
xeh_relocs_offset       = 8
xeh_objtbl_offset       = 12
xeh_codedata_length     = 16
xeh_image_length        = 20
xeh_relocs_length       = 24
xeh_objtbl_length       = 28
xeh_memory_length       = 32
xeh_uc_image_length     = 36
xeh_uc_relocs_length    = 40
xeh_bss_length          = 44
xeh_flags               = 48
xeh_entry_point         = 52
xeh_stack_point         = 56
xeh_resource_offset     = 60
xeh_image_comp          = 64
xeh_relocs_comp         = 65
xeh_LENGTH              = 66

xeh_ic_none             = 0
xeh_ic_lzss             = 1
xeh_ic_lzh              = 2
xeh_ic_apx              = 3 ;+
xeh_ic_xpa              = 4 ;+
xeh_ic_max              = 5 ;+
xeh_rl_none             = 0
xeh_rl_lzss             = 1
xeh_rl_lzh              = 2
xeh_rl_apx              = 3 ;+
xeh_rl_xpa              = 4 ;+
xeh_rl_max              = 5 ;+

_DFLL  MACRO Name, N
      Name equ [ebp][N]
      ENDM

DFLL MACRO Name, N
     _DFLL Name, %_LBase
     _LBase = _LBase+N
     ENDM
     _LBase = -128

VSegm IEBSS
DFB LoaderStack, 400h; dup(?)
;DFD FileHandle
;DFD lSavedPSP
DFL LoaderStackEnd
EVSeg IEBSS

DFLL __psp      ,4;     dw      ?
DFLL fhandle, 4 ;dd      ?
DFLL lecodesel, 4
DFLL relomem,4 ;dd      ?
DFLL leoffs ,4 ;dd      ?
DFLL progmem,4 ;dd      ?
DFLL rlocmem,4 ;dd      ?
DFLL progbsz,4 ;dd      ?
DFLL rlocbsz,4 ;dd      ?
DFLL proghnd,4 ;dd      ?
DFLL rlochnd,4 ;dd      ?
DFLL tprogmem,4 ;dd     ?      ;SB

DFLL buff    , 200h    ; db      0ach dup (?)

Segm IEText
assume ds:nothing, cs:IETextG, ss:LGroup
Loader PROC

@@DefLDErr MACRO ErrLabel, ErrMsg, NoJmp
      Segm IEData
      @@LDErrM = $
LLabel ErrLabel&EM
      DB ErrMsg
      ESeg IEData
      ErrLabel:
      mov edi, offset LGroup:@@LDErrM
      jmp short @@DispLDError
      ENDM
      @@DefLDErr  ErrNoDPMIMemory,'out of DPMI memory$'
      @@DefLDErr  @@ErrFile        ,"can't read EXE file$"
      @@DefLDErr  @@ErrEXE         ,'bad EXE format$'
      @@DefLDErr  @@ErrComp        ,'invalid compression type$'
      @@DefLDErr  @@ErrReloc       ,'invalid reloc type$'
      @@DefLDErr  ErrAllocSel      ,"can't allocate selector$"
      @@DefLDErr  ErrLock          ,"can't lock extender$"
      @@DefLDErr  @@ErrTransferBuf,"can't allocate transfer buffer$"

Segm IEData
LLabel LoadErrMsg
        DB 'ZRDX loader error: $'
LoadErrMsg1 DB 13, 10, '$'
;must be at the bottom of iedata!
LLabel localmembase
LDWord lSavedPSP
       DD 0
LDWord FileHandle
       DD 0
LDWord LECodeSelector
       DD 0
ESeg IEData
@@DispLDError:
        push ss
        pop  ds
        mov  ah, 9
        mov  edx, offset LGroup:LoadErrMsg
        int  21h
        mov  edx, edi
        int  21h
        mov  edx, offset LGroup:LoadErrMsg1
        int  21h
        mov  ax, 4CFFh
        int  21h
LoaderEntry:
        mov  edi, esp
        push ss
        pop  es
        and  dword ptr es:[edi].DC_SP, 0
        mov  byte ptr es:[edi].DC_EAX[1], 4Ah
        mov  ax, seg dgroup16
        sub  eax, 10h
        mov  word ptr es:[edi].DC_ES, ax
        mov  word ptr es:[edi].DC_EBX, MouseRHandlerPSize+10h
        org  $-2
LLabel MemBlock0Size
        DW   MouseRHandlerPSize+10h
        pushfd
        pop  eax
        mov  word ptr es:[edi].DC_Flags, ax
        mov  bx, 21h
        mov  ax, 300h
        int  31h

        mov  bx, 810h
LLabel  PatchPointTStSz
        mov  ax, 100h
        int  31h
        $ifnot jnc
          cmp  bx, 100h
          jb   @@ErrTransferBuf
          mov  ax, 100h
          int  31h
          jc   @@ErrTransferBuf
        $endif
        movzx ebx, bx
        shl  ebx, 4
        add  ds:TransferStack, ebx
        add  ds:TransferDTA, ebx
        mov  ds:TransferSelector, dx
        mov  gs, edx
        mov  ds:TransferSegment, ax
        mov  dx, offset OffDefaultDTA
        mov  ah, 1Ah
        int  21h

        ;---- setup exception handlers -------
        mov  ecx, ebp                ;extender code selector
        mov  dx, OffExc3Handler
        mov  bl, 3
        mov  ax, 203h
        int  31h
        mov  dx, OffExc0Handler
        mov  bl, 0
        mov  ax, 203h
        int  31h
        mov  dx, offset EGroup:ExceptionHandler
        mov  di, 10111111b
        $do
          shr  edi, 1
          $ifnot jc
            mov  ax, 203h
            int  31h
            add  edx, 4
          $endif
          inc  ebx
          cmp  bl, 32
        $enddo jb

        push fs
        pop  ds
        push ds
        pop  es

around:
        cld
        mov     ebp, offset ds:localmembase+128

; read EXE block

        ;mov     ecx, 40h            ;changed from 32 to 40h by SB
        ;lea     edx, buff
        call    read_buff200

        mov     ecx, dword ptr buff[40h-4]

        mov     leoffs,ecx

; seek to, read LE header

        call    seek1

; read in xE

        mov     ecx, xeh_LENGTH
        call    read_buff

; xE?

        cmp     word ptr buff,"EX"
        jne     @@ErrEXE

; allocate enough memory for relocs and program

        mov     ecx,dword ptr buff+xeh_memory_length
        mov     al,byte ptr buff+xeh_image_comp
        cmp     al,xeh_ic_max
        ;jae     comperr
        jae     @@ErrComp
        or      al,al
        jz      no_image_comp
        cmp     dword ptr buff+xeh_bss_length,19
        jae     no_image_comp
        add     ecx,19
no_image_comp:
        add     ecx,2
        mov     progbsz,ecx

        call    dpmiallocmem
        mov     progmem,ebx
        mov     proghnd,esi

        mov     ecx,dword ptr buff+xeh_uc_relocs_length
        mov     al,byte ptr buff+xeh_relocs_comp
        cmp     al,xeh_rl_max
        jae     @@ErrComp
        ;jae     comperr
        or      al,al
        jz      no_reloc_comp
        add     ecx,19
no_reloc_comp:
        cmp     ecx,dword ptr buff+xeh_objtbl_length
        jae     relocs_larger_than_objtbl
        mov     ecx,dword ptr buff+xeh_objtbl_length
relocs_larger_than_objtbl:
        mov     rlocbsz,ecx

        call    dpmiallocmem
        mov     rlocmem,ebx
        mov     rlochnd,esi

; read in program image

        mov     ecx,dword ptr buff+xeh_image_offset
        call    seek
        mov     ecx,dword ptr buff+xeh_image_length
        mov     edx,progmem
        mov     al,byte ptr buff+xeh_image_comp
        or      al,al
        jz      no_image_comp_2

;it's either LZSS or LZH

        add     edx,progbsz
        sub     edx,dword ptr buff+xeh_image_length
        sub     edx,2           ;LZH _may_ read two bytes extra, so try to
                                ;avoid page faults
        push    edx
        call    read
        pop     esi
        mov     edi,progmem

        cmp     byte ptr buff+xeh_image_comp,xeh_ic_xpa
        jz      image_comp_xpa
        cmp     byte ptr buff+xeh_image_comp,xeh_ic_apx
        jz      image_comp_apx

        ;cmp     byte ptr buff+xeh_image_comp,xeh_ic_lzh ;-
        ;jz      image_comp_lzh ;-
        ;call    delz ;-
        ;jmp     short image_comp_done ;-
image_comp_lzh:
        ;mov     eax,dword ptr buff+xeh_uc_image_length ;-
        ;call    delzh ;-
        jmp     short image_finish

image_comp_apx:
        call    deapx ;
        jmp     short image_finish
image_comp_xpa:
        call    dexpa ;
        jmp     short image_finish

image_comp_done:
        ;jmp     short image_finish ;-

no_image_comp_2: ;no comp
        call    read
image_finish:

; clear BSS region
; this must be done before relocs due to WLINK's trick
; (see note in xLE.C) ;+

        mov     edi,progmem
        mov     ecx,dword ptr buff+xeh_bss_length
        add     edi,dword ptr buff+xeh_uc_image_length
        xor     eax,eax
        rep     stosb

; Read in object table and make holes between objects.

        mov     ecx,dword ptr buff+xeh_objtbl_offset
        call    seek
        mov     ecx,dword ptr buff+xeh_objtbl_length
        mov     edx,rlocmem
        call    read

; EBX: image data left
; EDX: object data pointer
; EBP: program pointer

        mov     edx,rlocmem
        ;mov     ebp,progmem
        mov     eax, progmem
        mov     tprogmem, eax
        mov     ebx,dword ptr buff+xeh_uc_image_length
        std
object_loop:
        movzx   ecx,word ptr [edx+0]    ;number of 4K image pages
        shl     ecx,12
        ;add     ebp,ecx
        add     tprogmem, ecx    ;+SB
        sub     ebx,ecx
        sbb     eax,eax
        or      ebx,eax
        xor     ebx,eax

        movzx   eax,word ptr [edx+2]    ;number of 4K zeroed pages
        cmp     eax,0000ffffh
        jz      object_loop@done
        shl     eax,12
        add     edx,4
        mov     ecx,ebx

;        lea     esi,[ebp+ecx-4]         ;move remainder of image up to make room
        lea     esi, [ecx-4]      ;+SB
        add     esi, tprogmem     ;+SB
        lea     edi,[esi+eax]
        shr     ecx,2
        std
        rep     movsd

        cld                             ;zero area
        ;mov     edi,ebp
        ;add     ebp,eax
        mov     edi, tprogmem      ;SB
        add     tprogmem, eax      ;SB
        mov     ecx,eax
        shr     ecx,2
        xor     eax,eax
        rep     stosd

        jmp     short object_loop
object_loop@done:

; do we even have any relocs?  if not, skip this section

        mov     eax,dword ptr buff+xeh_uc_relocs_length
        or      eax,eax
        jz      no_relocs

; read in relocs

        mov     ecx,dword ptr buff+xeh_relocs_offset
        call    seek
        mov     ecx,dword ptr buff+xeh_relocs_length
        mov     edx,rlocmem
        mov     al,byte ptr buff+xeh_relocs_comp
        or      al,al
        jz      no_relocs_comp_2

; it's either LZSS or LZH

        add     edx,rlocbsz
        sub     edx,dword ptr buff+xeh_relocs_length
        push    edx
        call    read
        pop     esi
        mov     edi,rlocmem

        cmp     byte ptr buff+xeh_relocs_comp,xeh_rl_xpa
        jz      relocs_comp_xpa
        cmp     byte ptr buff+xeh_relocs_comp,xeh_rl_apx
        jz      relocs_comp_apx

        ;cmp     byte ptr buff+xeh_relocs_comp,xeh_rl_lzh ;-
        ;jz      relocs_comp_lzh ;-
        ;call    delz ;-
        ;jmp     short relocs_comp_done ;-
relocs_comp_lzh:
        ;mov     eax,dword ptr buff+xeh_uc_relocs_length ;-
        ;call    delzh ;-
        jmp     short relocs_load_finish

relocs_comp_apx:
        call    deapx ;
        jmp     short relocs_load_finish
relocs_comp_xpa:
        call    dexpa ;
        jmp     short relocs_load_finish

relocs_comp_done:
        ;jmp     short relocs_load_finish ;-

no_relocs_comp_2: ;no comp
        call    read
relocs_load_finish:

; apply relocs to image

        mov     esi,rlocmem
        mov     edi,progmem
apply_relocs_loop:
        mov     dl,[esi]        ;reloc type
        or      dl,dl
        jz      end_apply_relocs
        cmp     dl,4
        ;jae     invalid_reloc_err
        jae     @@ErrReloc
        xor     ecx,ecx
        mov     cx,[esi+1]
        or      ecx,ecx
        jnz     not_zero_relocs
        mov     ecx,00010000h
not_zero_relocs:
        mov     ebx,[esi+3]
        add     esi,7

        ;apply the reloc

apply_reloc_loop_2:
        test    dl,1
        jz      not_reloc32
        add     [edi+ebx],edi
        add     ebx,4
not_reloc32:
        test    dl,2
        jz      not_reloc1648
        mov     al,[edi+ebx]
        add     ebx,2
        or      al,al
        jz      useCS
        mov     [edi+ebx-2],ds
        jmp     short not_reloc1648
useCS:
        ;mov     [edi+ebx-2],cs
        mov     eax, lecodesel
        mov     [edi+ebx-2], ax
not_reloc1648:
        dec     ecx
        jz      apply_relocs_loop
        push ebp    ;+SB
        xor     ebp,ebp
        xor     eax,eax
diff_loop:
        mov     al,[esi]
        inc     esi
        push    eax
        shl     ebp,7
        and     al,7fh
        add     ebp,eax
        pop     eax
        test    al,80h
        jnz     diff_loop
        add     ebx,ebp
        pop  ebp   ;+SB
        jmp     short apply_reloc_loop_2
end_apply_relocs:

        ;free relocs memory

        mov     ax, 0502h
        mov     edi, rlochnd
        shld    esi, edi, 16
        int     31h

no_relocs:

        mov     ebx, lecodesel
        mov     eax, dword ptr buff+xeh_stack_point
        add     eax, progmem
        and     eax, 0fffffffch
        mov     ecx, dword ptr buff+xeh_entry_point
        add     ecx, progmem
        ;
        ;
        mov     edi, 0
        org  $-4
LDWord LoaderCodeMemHandle
        DD   0
        shld    esi, edi, 16
        ;shr     esi, 16
        mov     es,__psp        ;set ES
        push    ds
        pop     ss
        mov     esp,eax
        push    ebx
        push    ecx
        push    0FFFFh     ;extender code selector es:ExtenderSel
        org  $-4
LDWord ExtenderSel
        DD  0
        push    OffStarter ;extender starter routine
        mov   ax, 502h
        retf



;**************************************************
;
;       ALLOCMEM
;
;       ECX     number of bytes requested
;
;       Return:
;       EBX     linear address of memory
;       ESI     handle of memory

dpmiallocmem:
        mov     ax,0501h
        mov     ebx,ecx
        shr     ebx,16
        int     31h
        ;jc      memerr
        jc      ErrNoDPMIMemory
        shl     ebx,16
        mov     bx,cx
        shl     esi,16
        mov     si,di
        ret

;**************************************************
;       SEEK
;
;       ECX     offset to seek to

seek:
        add     ecx, leoffs      ;+SB
seek1:
        mov     ax,4200h
        mov     ebx,fhandle
        mov     edx,ecx
        shr     ecx,16
        int     21h
        ;jc      readerr
        jc      @@ErrFile
        ret

;**************************************************
;       READ
;
;       ECX     bytes to read
;       EDX     address to read to

read_buff200:
        xor  ecx, ecx
        mov  ch, 2
read_buff:
        lea  edx, buff
read_ss:mov  ebx, ss
        jmp  short read1

read:   mov  ebx, ds
read1:  push ds
        mov  ds, ebx
        mov     ah, 3fh
        mov     ebx, fhandle
        int     21h
        pop     ds
        ;jc      readerr
        jc      @@ErrFile
        cmp     eax, ecx
        ;jne     readerr
        jne     @@ErrFile
        ret

;**************************************************
;       DEapx - THE BIG ROUTINE
;
;       EDI - destination memory
;       ESI - source memory
;
;       Modified: flags ECX ESI EDI

deapx:
        cld
        mov     ecx,[esi]               ;get # of codes
        add     esi,4
        rep     movsb
        ret

;**************************************************
;void aP_depack_asm(unsigned char *, unsigned char *);
;       DExpa - THE BIG ROUTINE
;
;       EDI - destination memory
;       ESI - source memory
;
;       Modified: eax ebx ecx edx ebp esi edi

dexpa:
       push    ebp

       cld
       mov     al, 80h
       jmp     short nexttag
@@LoadByte2:
       lodsb
       rcl al, 1
       jmp @@EndLoadByte2
@@LoadByte1:
       lodsb
       rcl al, 1
       jc  not_literal
literal:
       movsb
nexttag:
       shl  al, 1
       jz   @@LoadByte1
       jnc  literal

not_literal:
       ;call    getbit
       shl  al, 1
       jz   @@LoadByte2
@@EndLoadByte2:
       jnc     codepair
       xor     ebx, ebx
       xor     ecx, ecx
       shl     al, 1
       jnz     @@NLoadByte3
       lodsb
       rcl     al,1
@@NLoadByte3:
       ;call    getbit
       jnc     shortmatch
       mov     bl, 8
       add     ecx, 1         ;inc ecx & clc
@@L0:  adc     bl, bl
       jc      exitlmatchloop
       shl     al, 1
       jnz     @@L0
       lodsb
       rcl     al, 1
       jmp     @@L0
exitlmatchloop:
       jnz     domatch
       mov     ds:[edi], bl
       inc     edi
       jmp     short nexttag
       ;jmp     short domatch_continue
codepair:
       call    getgamma
       dec     ecx
       dec     ecx
       jnz     normalcodepair
       mov     ebx, ebp
       call    getgamma
       jmp     short domatch
normalcodepair:
       dec     ecx
       shl     ecx, 8
       mov     cl, [esi]  ;1 u
       inc     esi        ;1 v
       mov     ebp, ecx   ;1u    \
       mov     ebx, ecx   ;1v    / 1
       call    getgamma
       cmp     bh, 5           ;new 1
       jb      not_gt          ;new 2
       inc     ecx             ;new 3
not_gt:                        ;new 4
       cmp     ebx, 127
       ja      domatch
       inc     ecx
       inc     ecx
       jmp     short domatch
shortmatch:
       mov     bl, byte ptr ds:[esi]
       inc     esi
       shr     ebx, 1
       jz      donedepacking
       adc     ecx, 2
       mov     ebp, ebx        ;new 5
domatch:
       push    esi
       mov     esi, edi
       sub     esi, ebx
       rep     movsb
       pop     esi
       jmp     short nexttag

;       neg  ebx
;matchloop:
;       mov  ah, [ebx+edi]
;       mov  [edi], ah
;       inc  edi
;       dec  ecx
;       jnz  matchloop

;       jmp     short nexttag

;getgammabit:
;        rcl  ecx, 1
;getbit:
;        shl  al, 1
;        jz   load_byte
;        ret
;
;load_byte:
;        ;mov  al, [esi]
;        ;inc  esi
;        lodsb
;        rcl  al, 1
;        ret

;getgamma:
;        xor  ecx, ecx
;        stc
;        jmp  getgammaentry
;getgammaloop:
;        call getbit
;getgammaentry:
;        call getgammabit
;        jc   getgammaloop
;        call getbit
;        rcl  ecx, 1
;        ret
getgamma:
        mov  ecx, 1
        jmp  getgammaentry
@@LoadByteG1:
        lodsb
        rcl al, 1
        jnc  endgammaloop
getgammaloop:
        shl  al, 1
        jnz  @@NLoadByteG0
        lodsb
        rcl  al, 1
@@NLoadByteG0:
        rcl  ecx, 1
getgammaentry:
        shl  al, 1
        jz   @@LoadByteG1
        jc   getgammaloop
endgammaloop:
        shl  al, 1
        jnz  @@NLoadByteG2
        lodsb
        rcl  al, 1
@@NLoadByteG2:
        rcl  ecx, 1
        retn

donedepacking:
       pop     ebp

       ret

ESeg IEText
Loader        endp
