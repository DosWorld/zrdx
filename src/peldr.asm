;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

;IMAGE_FILE_HEADER and IMAGE_OPTIONAL_HEADER offsets from the "PE" signature
PEMachine equ 4
PENumSections equ 6
PEOptHdrSize equ 20
PEFlags equ 22
PEOptMagic equ 24
PEEntryRVA equ 40
PEImageBase equ 52
PESectAlign equ 56
PEImageSize equ 80
PEHeadersSize equ 84
PEStackReserve equ 96
PENumDirs equ 116
PEDirImport equ 128
PEDirImportSize equ 132
PEDirReloc equ 160
PEDirRelocSize equ 164
PEDirTLS equ 192
PEDirTLSSize equ 196
PEDirDelay equ 224
PEDirDelaySize equ 228
PEOptHdr equ 24
;IMAGE_SECTION_HEADER
PSVirtualSize equ 8
PSVirtualAddr equ 12
PSRawSize equ 16
PSRawPtr equ 20
PSHeader_size equ 40
MaxPESections equ 24

        VSegm IEBSS
        DFD LoaderStack, 400
        DFL LoaderStackEnd
        DFB Header, 400h
        DFD LoaderMemHandle
        DFD ImageSize
        DFD ImageHandle
        DFD ImageBaseA
        DFD StackHandle
        DFD CodeSel
        DFD DataSel
        DFD EntryPoint
        DFD PEHeaderPos
        DFD StackTop
        EVSeg IEBSS
        SEGM IEData
        LDWord lSavedPSP
       dd 0
        ESEG IEData
        SEGM IEText
DPMICall:
        push ecx
        push edi
        mov ecx, ebx
        shr ebx, 16
        mov edi, esi
        shr esi, 16
        int 31h
        _ifnot jc
          shl ebx, 16
          mov bx, cx
          shl esi, 16
          or si, di   ;this instruction clear CF instead of "mov si, di"
          ;clc
        _endif
        pop edi
        pop ecx
        ret


Loader:

%macro DefLDErr 2
        SEGM IEData
%%m:
        DefLLabel zcat2(%1,EM)
        db %2
        ESEG IEData
%1:
        mov edi, %%m wrt LGROUP
        jmp short DispLDError
%endmacro
%macro DOSINT 0-1
        int 21h
%endmacro
      DefLDErr ErrNoDPMIMemory, 'out of DPMI memory$'
      DefLDErr ErrFile@129, "can't read EXE file$"
      DefLDErr ErrEXE@129, 'bad PE format$'
      DefLDErr ErrPEReloc, "can't relocate PE image$"
      DefLDErr ErrPEImport, 'PE imports are not supported$'
      DefLDErr ErrPETls, 'PE TLS is not supported$'
      DefLDErr ErrAllocSel, "can't allocate selector$"
      DefLDErr ErrLock, "can't lock extender$"
      DefLDErr ErrTransferBuf@129, "can't allocate transfer buffer$"
        SEGM IEData
        LLabel LoadErrMsg
        db 'ZRDX loader error: $'
LoadErrMsg1: db 13, 10, '$'
        ESEG IEData
DispLDError:
        push ss
        pop ds
        mov ah, 9
        mov edx, OffLoadErrMsg
        int 21h
        mov edx, edi
        int 21h
        mov edx, LoadErrMsg1
        int 21h
        mov ax, 4CFFh
        int 21h

LoaderEntry:
        mov edi, esp
        push ss
        pop es
        and dword [es:edi + DC_SP], 0
        mov byte [es:edi + DC_EAX + 1], 4Ah
        mov ax, DGROUP16
        sub eax, 10h
        mov [es:edi + DC_ES], ax
        mov word [es:edi + DC_EBX], MouseRHandlerPSize + 10h
        LBack MemBlock0Size, 2
        pushfd
        pop eax
        mov [es:edi + DC_Flags], ax
        mov bx, 21h
        mov ax, 300h
        int 31h
        mov bx, 0
        LLabel PatchPointTStSz
        mov ax, 100h
        int 31h
        _ifnot jnc
          cmp bx, 100h
          jb ErrTransferBuf@129
          mov ax, 100h
          int 31h
          jc ErrTransferBuf@129
        _endif
        movzx ebx, bx
        shl ebx, 4
        add [OffTransferStack], ebx
        add [OffTransferDTA], ebx
        mov [OffTransferSelector], dx
        mov gs, edx
        mov [OffTransferSegment], ax
        mov dx, OffDefaultDTA
        mov ah, 1Ah
        int 21h

        ;int 3

        ;---- setup exception handlers -------
        mov ecx, ebp                ;extender code selector
        mov dx, OffExc3Handler
        mov bl, 3
        mov ax, 203h
        int 31h
        mov dx, OffExc0handler
        mov bl, 0
        mov ax, 203h
        int 31h
        mov dx, ExceptionHandler
        mov di, 10111111b
        _do
          shr edi, 1
          _ifnot jc
            mov ax, 203h
            int 31h
            add edx, 4
          _endif
          inc ebx
          cmp bl, 16
        _enddo jb
        push ds
        push fs
        pop ds
        pop fs
%ifndef Release
        ;jmp $
%endif
LoaderDbgEntry:
        mov ebp, OffHeader
        push ss
        push ebp
        push 0
        push 040h
        call FileRead
        cmp al, 040h
        jne ErrFile@129
        mov eax, [ebp + 3Ch]
        mov [ss:FileDisp], eax
        push ss
        push ebp
        push 0
        push 040h
        call FileRead
        cmp al, 040h
        jne ErrFile@129
        cmp word [ebp], 'MZ'
        jne ErrEXE@129
        push ss
        push ebp
        mov edx, [ebp + 3Ch]
        mov [ss:OffPEHeaderPos], edx
        push edx
        push 400h
        call FileRead
        movzx eax, ax
        cmp eax, PEOptHdr + 96
        jb ErrFile@129
        mov edi, eax
        cmp dword [ebp], 'PE'
        jne PEBadHeader
        cmp word [ebp + PEMachine], 14Ch
        jne PEBadHeader
        cmp word [ebp + PEOptMagic], 10Bh
        jne PEBadHeader
        test byte [ebp + PEFlags], 2
        jz PEBadHeader
        test byte [ebp + PEFlags + 1], 20h
        jnz PEBadHeader
        movzx ecx, word [ebp + PENumSections]
        or ecx, ecx
        jz PEBadHeader
        cmp ecx, MaxPESections
        ja PEBadHeader
        jmp short PEHeaderFields
PEBadHeader:
        jmp ErrEXE@129
PEHeaderFields:
        movzx edx, word [ebp + PEOptHdrSize]
        cmp edx, 96
        jb PEBadHeader
        mov ebx, edx
        sub ebx, 96
        shr ebx, 3
        cmp [ebp + PENumDirs], ebx
        ja PEBadHeader
        lea eax, [ecx + ecx*4]
        lea eax, [edx + eax*8 + PEOptHdr]
        cmp eax, edi
        ja PEBadHeader
        add eax, [ss:OffPEHeaderPos]
        jc PEBadHeader
        cmp [ebp + PEHeadersSize], eax
        jb PEBadHeader
        cmp dword [ebp + PENumDirs], 2
        jb PEImportsOK
        mov eax, [ebp + PEDirImport]
        or eax, [ebp + PEDirImportSize]
        jne ErrPEImport
PEImportsOK:
        cmp dword [ebp + PENumDirs], 14
        jb PEDelayOK
        mov eax, [ebp + PEDirDelay]
        or eax, [ebp + PEDirDelaySize]
        jne ErrPEImport
PEDelayOK:
        cmp dword [ebp + PENumDirs], 10
        jb PETlsOK
        mov eax, [ebp + PEDirTLS]
        or eax, [ebp + PEDirTLSSize]
        jne ErrPETls
PETlsOK:
        mov ebx, [ebp + PEImageSize]
        cmp ebx, [ebp + PEHeadersSize]
        jb ErrEXE@129
        add ebx, 0FFFh
        jc ErrEXE@129
        and ebx, ~(0FFFh)
        jz ErrEXE@129
        mov [ss:OffImageSize], ebx
        mov ax, 501h
        call DPMICall
        jc ErrNoDPMIMemory
        mov [ss:OffImageHandle], esi
        mov [ss:OffImageBaseA], ebx
        mov [fs:OffImageBaseAddr], ebx
        mov edi, ebx
        mov ecx, [ss:OffImageSize]
        shr ecx, 2
        xor eax, eax
        push es
        push ds
        pop es
        rep stosd
        pop es
        push ds
        push dword [ss:OffImageBaseA]
        push 0
        push dword [ebp + PEHeadersSize]
        call FileRead
        cmp eax, [ebp + PEHeadersSize]
        jne ErrFile@129
        mov edi, [ebp + PEHeadersSize]
        movzx ecx, word [ebp + PENumSections]
        movzx esi, word [ebp + PEOptHdrSize]
        lea esi, [ebp + esi + PEOptHdr]
        _do
          mov edx, [ss:esi + PSRawSize]
          mov eax, [ss:esi + PSVirtualAddr]
          cmp eax, edi
          jb PEBadSection
          mov ebx, [ss:esi + PSVirtualSize]
          cmp ebx, edx
          _ifnot jae
            mov ebx, edx
          _endif
          add ebx, eax
          jc PEBadSection
          cmp ebx, [ebp + PEImageSize]
          ja PEBadSection
          mov edi, ebx
          test edx, edx
          _ifnot jz
            cmp dword [ss:esi + PSRawPtr], 0
            je PEBadSection
            add eax, [ss:OffImageBaseA]
            push ecx
            push esi
            push ds
            push eax
            push dword [ss:esi + PSRawPtr]
            push edx
            call FileRead
            pop esi
            pop ecx
            cmp eax, edx
            jne ErrFile@129
          _endif
          add esi, PSHeader_size
        _enddo loop
        jmp short PESectionsOK
PEBadSection:
        jmp ErrEXE@129
PESectionsOK:
        mov edi, [ss:OffImageBaseA]
        mov edx, edi
        sub edx, [ebp + PEImageBase]
        _ifnot jz
          cmp dword [ebp + PENumDirs], 6
          jb PEBadReloc
          mov ecx, [ebp + PEDirRelocSize]
          or ecx, ecx
          jz PEBadReloc
          mov esi, [ebp + PEDirReloc]
          mov eax, esi
          add eax, ecx
          jc PEBadReloc
          cmp eax, [ebp + PEImageSize]
          ja PEBadReloc
          add esi, edi
          add ecx, esi
          _do
            cmp esi, ecx
            _break jae
            mov eax, ecx
            sub eax, esi
            cmp eax, 8
            jb PEBadReloc
            mov ebx, [esi + 4]
            test bl, 1
            jnz PEBadReloc
            cmp ebx, 8
            jb PEBadReloc
            mov eax, ecx
            sub eax, esi
            cmp ebx, eax
            ja PEBadReloc
            push ecx
            lea ecx, [esi + ebx]
            push ecx
            mov ebx, [esi]
            add esi, 8
            _do
              cmp esi, [esp]
              _break jae
              movzx eax, word [esi]
              add esi, 2
              mov ecx, eax
              shr ecx, 12
              and eax, 0FFFh
              cmp ecx, 3
              _ifnot jne
                add eax, ebx
                jc PEBadReloc
                mov ecx, eax
                add ecx, 4
                jc PEBadReloc
                cmp ecx, [ebp + PEImageSize]
                ja PEBadReloc
                add [edi + eax], edx
              _else jmp
                jecxz SkipReloc
                jmp PEBadReloc
              _endif
SkipReloc:
            _enddo jmp
            pop esi
            pop ecx
          _enddo jmp
        _endif
        jmp short PERelocsOK
PEBadReloc:
        jmp ErrPEReloc
PERelocsOK:
        mov eax, [ebp + PEEntryRVA]
        cmp eax, [ebp + PEImageSize]
        jae ErrEXE@129
        add eax, [ss:OffImageBaseA]
        mov [ss:OffEntryPoint], eax
        mov ebx, [ebp + PEStackReserve]
        cmp ebx, 10000h
        _ifnot jae
          mov ebx, 10000h
        _endif
        cmp ebx, 400000h
        _ifnot jbe
          mov ebx, 400000h
        _endif
        add ebx, 0FFFh
        and ebx, ~(0FFFh)
        mov edx, ebx
        mov ax, 501h
        call DPMICall
        jc ErrNoDPMIMemory
        mov [ss:OffStackHandle], esi
        add ebx, edx
        lea eax, [ebx - 16]
        mov dword [eax], 21CD4CB4h
        sub ebx, 20
        mov [ebx], eax
        mov [ss:OffStackTop], ebx
        xor ebx, ebx
        mov ecx, 0FFFFF000h
        mov dx, 409Ah
        call CreateSelector
        mov [ss:OffCodeSel], ebx
        xor ebx, ebx
        mov ecx, 0FFFFF000h
        mov dx, 4092h
        call CreateSelector
        mov [ss:OffDataSel], ebx
        mov ebx, [ss:OffFileHandle]
        mov ah, 3Eh
        DOSINT
        mov eax, [ss:OffEntryPoint]
        mov ebx, [ss:OffCodeSel]
        push dword [ss:OffDataSel]
        push dword [ss:OffStackTop]
        lss esp, [esp]
        push ebx
        push eax
        mov edi, 0
        LBack LoaderCodeMemHandle, 4
        shld esi, edi, 16
        push 0FFFFh
        LBack ExtenderSel, 4
        push OffStarter
        mov es, [es:OfflSavedPSP]
        mov ax, 502h
        xor ebx, ebx
        mov ecx, ebx
        mov edx, ebx
        mov ebp, ebx
        mov fs, bx
        push ss
        pop ds
        retf

        SEGM IEData
FileDisp: dd 0
        LDWord FileHandle
             dd 0
        ESEG IEData
FileRead:
         push ebx
         push ecx
         push edx
;@@MemSel  EQU DWORD PTR ss:[esp+12].16
;@@MemPtr  EQU DWORD PTR ss:[esp+12].12
;@@FilePtr EQU DWORD PTR SS:[esp+12].8
;@@Size    EQU DWORD PTR SS:[esp+12].4
         mov edx, [esp + 12 + 8]
         add edx, [ss:FileDisp]
         jc PEReadError
         mov ecx, edx
         shr ecx, 16
         mov ebx, [ss:OffFileHandle]
         mov ax, 4200h
         ;call XX
         DOSINT 42
         jc PEReadError
         ;int  21h
         ;call EnableTrace
         push ds
         lds edx, [esp + 12 + 12 + 4]
         mov ecx, [esp + 12 + 4 + 4]
         mov ah, 3Fh
         DOSINT
         jc PEReadError
         ;push eax       ;write dump
         ;mov ebx, DumpHandle
         ;mov ah, 40h
         ;DOSINT
         ;pop eax
         pop ds
         ;call DisableTrace
        ;push ecx
        ;push 8
        ;call PrintN
        ;push eax
        ;push 8
        ;call PrintN
         pop edx
         pop ecx
         pop ebx
         ret 16

PEReadError:
         jmp ErrFile@129

;create a selector for the object by the object index
;if the object is 16-bit then base = object base, limit = object length
;otherwise base = 0, limit = 4G

;ebx-base, ecx-size, dx-attrs
;return selector in ebx
;ax destroyed
CreateSelector:
               push edi
               sub esp, 8        ;alloc space for descriptor
;Cache EQU ss:[esp]
               cmp ecx, 100000h  ;may use byte limit ?
               _ifnot jb
               or dh, 80h
               add ecx, 0FFFh
               shr ecx, 12
               _endif
               mov ax, cs
               and eax, 3
               shl eax, 5         ;setup privelegy level
               or dl, al
               or dl, 1
               mov [esp], ecx
               shr ecx, 16
               or dh, cl
               mov [esp + 2], ebx
               shr ebx, 24
               mov [esp + 7], bl
               mov [esp + 5], dx
               mov cx, 1
               xor eax, eax
               int 31h
               jc ErrAllocSel
               ;$ifnot jc
               movzx ebx, ax
               mov ax, 0Ch
               mov edi, esp
               push es
               push ss
               pop es
               int 31h
               pop es
               jc ErrAllocSel
               ;$endif
               ;mov ebx, bx
               add esp, 8
               pop edi
               ret

        ESEG IEText
