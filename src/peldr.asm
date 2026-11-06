;             This file is part of the ZRDX OSE project
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
PEDirCom equ 232
PEDirComSize equ 236
PEOptHdr equ 24
PEFileAlign equ 60
PEDirExport equ 120
PEDirExportSize equ 124
;IMAGE_SECTION_HEADER
PSVirtualSize equ 8
PSVirtualAddr equ 12
PSRawSize equ 16
PSRawPtr equ 20
PSHeader_size equ 40
MaxPESections equ 24
;PELoadCtx: header buffer first, so [ebp + PExxx] reach the
;header; the fields follow it.
CtxFile equ 400h
CtxDisp equ 404h
CtxHdrPos equ 408h
CtxSP equ 40Ch
CtxSize equ 410h
CtxHandle equ 414h
CtxBase equ 418h
CtxEntry equ 41Ch
CtxKind equ 420h
PECtxSize equ 424h

        VSegm IEBSS
        DFD LoaderStack, 400
        DFL LoaderStackEnd
        DFB Header, PECtxSize
        DFD LoaderMemHandle
        DFD StackHandle
        DFD CodeSel
        DFD DataSel
        DFD StackTop
        EVSeg IEBSS
        SEGM IEData
        LDWord lSavedPSP
       dd 0
%ifdef PEMAX
        LDWord ErrName
        dd 0
%endif
        ESEG IEData
        SEGM IEText
%ifdef NATIVEPE
%define MB0Name MemBlock0SizeL
%else
%define MB0Name MemBlock0Size
%endif
%macro ExtCall 2
        push 0FFFFh
        LBack %1, 4
        push Off %+ %2
        call far [esp]
        lea esp, [esp + 8]              ;keep CF
%endmacro
%macro DefDPMICall 1
%1:
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
%endmacro
        DefDPMICall DPMICall

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
      DefLDErr ErrPEFmt, 'unsupported PE format$'
      DefLDErr ErrPEImport, 'PE directory is not supported$'
%ifdef PEMAX
      DefLDErr ErrPEDll, "can't open PE DLL$"
      DefLDErr ErrPEDll2, 'bad PE DLL image$'
      DefLDErr ErrPEUnres, 'unresolved PE import$'
      DefLDErr ErrPEImpTab, 'bad PE import table$'
      DefLDErr ErrPEDup, 'PE module name conflict$'
      DefLDErr ErrPEInit, 'PE DLL initialization failed$'
      DefLDErr ErrPEHost, 'DPMI host lacks a function$'
%endif
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
%ifdef PEMAX
        cmp dword [ss:OffErrName], 0
        _ifnot je
          mov ds, [ss:OffDataSel]
          mov edx, [ss:OffErrName]
          int 21h
          push ss
          pop ds
        _endif
%endif
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
        LBack MB0Name, 2
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
        mov eax, [ss:OffFileHandle]
        mov [ebp + CtxFile], eax
        mov dword [ebp + CtxKind], 0
%ifdef PEMAX
        ExtCall ExtenderSelB, PELoadImage
%else
        call PELoadImage
%endif
        jnc PEImageLoaded
        cmp al, 1
        je ErrFile@129
        cmp al, 3
        je ErrPEReloc
        cmp al, 4
        je ErrPEImport
        cmp al, 5
        je ErrPETls
        cmp al, 6
        je ErrNoDPMIMemory
        cmp al, 7
        je ErrPEFmt
        jmp ErrEXE@129
PEImageLoaded:
%ifdef PEMAX
        ExtCall ExtenderSelC, PERegMain
        jc ErrNoDPMIMemory
%endif
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
%ifdef PEMAX
        sub ebx, 48                     ;PEBind error text buffer
%endif
        lea eax, [ebx - 16]
%ifdef PEMAX
        mov byte [eax], 9Ah
        mov dword [eax + 1], OffPEExit
        mov word [eax + 5], 0FFFFh
        LBack ExtenderSelE, 2
        mov dword [eax + 7], 21CD4CB4h
%else
        mov dword [eax], 21CD4CB4h
%endif
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
%ifdef PEMAX
        mov eax, [ss:OffStackTop]
        mov edx, [ss:OffDataSel]
        mov esi, [ss:OffCodeSel]
        ExtCall ExtenderSelD, PEBind
        jnc PEBound
        mov edx, [ss:OffStackTop]
        add edx, 20
        mov [ss:OffErrName], edx
        cmp al, 1
        je ErrPEDll
        cmp al, 2
        je ErrPEDll2
        cmp al, 3
        je ErrPEUnres
        cmp al, 4
        je ErrPEImpTab
        cmp al, 5
        je ErrPEDup
        cmp al, 7
        je ErrPEInit
        cmp al, 8
        je ErrPEHost
        jmp ErrNoDPMIMemory
PEBound:
%endif
        mov eax, [ebp + CtxEntry]
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
        LDWord FileHandle
             dd 0
        ESEG IEData
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

%ifdef PEMAX
        SEGM EText
%include "peload.asm"
%include "pereg.asm"
%include "peexp.asm"
%include "peimp.asm"
        ESEG EText
%else
%include "peload.asm"
%endif

        ESEG IEText
