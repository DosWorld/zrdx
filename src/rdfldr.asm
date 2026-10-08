;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

;RDOFF2 record types
RDFReloc equ 1
RDFImport equ 2
RDFGlobal equ 3
RDFDLL equ 4
RDFBSS equ 5
RDFSegReloc equ 6
RDFFarImport equ 7
RDFHeaderPos equ 14

        VSegm IEBSS
        DFD LoaderStack, 400
        DFL LoaderStackEnd
        DFB Header, 40h
        DFD HdrBlock
        DFD HdrLen
        DFD CodeLen
        DFD DataLen
        DFD BssLen
        DFD CodeNum
        DFD DataNum
        DFD BssNum
        DFD CodeFPos
        DFD DataFPos
        DFD CodeBase
        DFD DataBase
        DFD BssBase
        DFD ImageSize
        DFD ImageHandle
        DFD HdrHandle
        DFD StackHandle
        DFD CodeSel
        DFD DataSel
        DFD EntryPoint
        DFD StackTop
        DFD PSPLinear
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
      DefLDErr ErrEXE@129, 'bad RDF format$'
      DefLDErr ErrRDFSeg, 'bad RDF segment$'
      DefLDErr ErrRDFRec, 'unsupported RDF record$'
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
          jb near ErrTransferBuf@129
          mov ax, 100h
          int 31h
          jc near ErrTransferBuf@129
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
        jne near ErrFile@129
        mov eax, [ebp + 3Ch]
        mov [ss:FileDisp], eax
        push ss
        push ebp
        push 0
        push RDFHeaderPos
        call FileRead
        cmp al, RDFHeaderPos
        jne near ErrFile@129
        cmp dword [ebp], 'RDOF'
        jne near ErrEXE@129
        cmp word [ebp + 4], 'F2'
        jne near ErrEXE@129
        mov ebx, [ebp + 10]
        cmp ebx, 1000000h
        jae near ErrEXE@129
        mov [ss:OffHdrLen], ebx
        add ebx, 16
        mov ax, 501h
        call DPMICall
        jc near ErrNoDPMIMemory
        mov [ss:OffHdrBlock], ebx
        mov [ss:OffHdrHandle], esi
        push ds
        push ebx
        push RDFHeaderPos
        push dword [ss:OffHdrLen]
        call FileRead
        cmp eax, [ss:OffHdrLen]
        jne near ErrFile@129
        mov esi, [ss:OffHdrBlock]
        mov edi, esi
        add edi, [ss:OffHdrLen]
        xor edx, edx
        _do
          cmp esi, edi
          _break jae
          movzx ecx, byte [esi + 1]
          movzx eax, byte [esi]
          cmp eax, RDFBSS
          _ifnot jne
            add edx, [esi + 2]
            jc near ErrEXE@129
            cmp edx, 10000000h
            jae near ErrEXE@129
          _endif
          cmp eax, RDFImport
          je near ErrRDFRec
          cmp eax, RDFDLL
          je near ErrRDFRec
          cmp eax, RDFSegReloc
          je near ErrRDFRec
          cmp eax, RDFFarImport
          je near ErrRDFRec
          lea esi, [esi + ecx + 2]
        _enddo jmp
        mov [ss:OffBssLen], edx
        mov dword [ss:OffCodeNum], 0FFFFh
        mov dword [ss:OffDataNum], 0FFFFh
        xor eax, eax
        mov [ss:OffCodeLen], eax
        mov [ss:OffDataLen], eax
        mov edi, RDFHeaderPos
        add edi, [ss:OffHdrLen]
        _do
          push ss
          push ebp
          push edi
          push 10
          call FileRead
          cmp eax, 10
          _break jb
          movzx eax, word [ebp]
          or eax, eax
          _break jz
          mov ecx, [ebp + 6]
          cmp ecx, 10000000h
          jae near ErrEXE@129
          cmp eax, 1
          _ifnot jne
            cmp dword [ss:OffCodeNum], 0FFFFh
            jne near ErrRDFSeg
            mov [ss:OffCodeLen], ecx
            movzx eax, word [ebp + 2]
            mov [ss:OffCodeNum], eax
            lea eax, [edi + 10]
            mov [ss:OffCodeFPos], eax
          _else jmp
            cmp eax, 2
            _ifnot jne
              cmp dword [ss:OffDataNum], 0FFFFh
              jne near ErrRDFSeg
              mov [ss:OffDataLen], ecx
              movzx eax, word [ebp + 2]
              mov [ss:OffDataNum], eax
              lea eax, [edi + 10]
              mov [ss:OffDataFPos], eax
            _endif
          _endif
          lea edi, [edi + ecx + 10]
        _enddo jmp
        mov eax, [ss:OffCodeNum]
        cmp eax, 0FFFFh
        _ifnot jne
          xor eax, eax
          cmp dword [ss:OffDataNum], 0
          _ifnot jne
            mov eax, 1
          _endif
          mov [ss:OffCodeNum], eax
        _endif
        mov eax, [ss:OffDataNum]
        cmp eax, 0FFFFh
        _ifnot jne
          mov eax, [ss:OffCodeNum]
          inc eax
          mov [ss:OffDataNum], eax
        _endif
        mov eax, [ss:OffCodeNum]
        cmp eax, [ss:OffDataNum]
        _ifnot jae
          mov eax, [ss:OffDataNum]
        _endif
        inc eax
        mov [ss:OffBssNum], eax
        mov ebx, [ss:OffCodeLen]
        add ebx, 15
        and ebx, ~(15)
        mov ecx, [ss:OffDataLen]
        add ecx, 15
        and ecx, ~(15)
        add ebx, ecx
        add ebx, [ss:OffBssLen]
        add ebx, 4096 + 32
        and ebx, ~(15)
        mov [ss:OffImageSize], ebx
        mov ax, 501h
        call DPMICall
        jc near ErrNoDPMIMemory
        mov [ss:OffImageHandle], esi
        mov [fs:OffImageBaseAddr], ebx
        mov [ss:OffCodeBase], ebx
        mov edi, ebx
        mov ecx, [ss:OffImageSize]
        shr ecx, 2
        xor eax, eax
        push es
        push ds
        pop es
        rep stosd
        pop es
        mov eax, [ss:OffCodeLen]
        add eax, 15
        and eax, ~(15)
        add eax, [ss:OffCodeBase]
        mov [ss:OffDataBase], eax
        mov eax, [ss:OffDataLen]
        add eax, 15
        and eax, ~(15)
        add eax, [ss:OffDataBase]
        mov [ss:OffBssBase], eax
        mov eax, [ss:OffCodeLen]
        or eax, eax
        _ifnot jz
          push ds
          push dword [ss:OffCodeBase]
          push dword [ss:OffCodeFPos]
          push eax
          call FileRead
          cmp eax, [ss:OffCodeLen]
          jne near ErrFile@129
        _endif
        mov eax, [ss:OffDataLen]
        or eax, eax
        _ifnot jz
          push ds
          push dword [ss:OffDataBase]
          push dword [ss:OffDataFPos]
          push eax
          call FileRead
          cmp eax, [ss:OffDataLen]
          jne near ErrFile@129
        _endif
        mov esi, [ss:OffHdrBlock]
        mov edi, esi
        add edi, [ss:OffHdrLen]
        _do
          cmp esi, edi
          _break jae
          movzx ecx, byte [esi + 1]
          cmp byte [esi], RDFReloc
          _ifnot jne
            push esi
            push edi
            push ecx
            movzx eax, byte [esi + 2]
            and eax, 3Fh
            call SegBase
            jc near ErrRDFSeg
            mov ebx, eax
            movzx eax, byte [esi + 2]
            and eax, 3Fh
            call SegLen
            jc near ErrRDFSeg
            mov edx, [esi + 3]
            movzx ecx, byte [esi + 7]
            add ecx, edx
            jc near ErrRDFSeg
            cmp ecx, eax
            ja near ErrRDFSeg
            mov ecx, [esi + 3]
            add ecx, ebx
            movzx eax, word [esi + 8]
            call SegBase
            jc near ErrRDFSeg
            test byte [esi + 2], 40h
            _ifnot jz
              sub eax, ebx
            _endif
            movzx edx, byte [esi + 7]
            cmp edx, 1
            _ifnot jne
              add [ecx], al
            _else jmp
              cmp edx, 2
              _ifnot jne
                add [ecx], ax
              _else jmp
                add [ecx], eax
              _endif
            _endif
            pop ecx
            pop edi
            pop esi
          _endif
          lea esi, [esi + ecx + 2]
        _enddo jmp
        mov eax, [ss:OffCodeBase]
        mov [ss:OffEntryPoint], eax
        mov esi, [ss:OffHdrBlock]
        mov edi, esi
        add edi, [ss:OffHdrLen]
        _do
          cmp esi, edi
          _break jae
          movzx ecx, byte [esi + 1]
          cmp byte [esi], RDFGlobal
          _ifnot jne
            lea edx, [esi + 8]
            mov ebx, OffStartName
            _do
              mov al, [edx]
              cmp al, [ss:ebx]
              _break jne
              or al, al
              _break jz
              inc edx
              inc ebx
            _enddo jmp
            cmp al, [ss:ebx]
            _ifnot jne
              or al, al
              _ifnot jnz
                movzx eax, byte [esi + 3]
                call SegBase
                jc near ErrRDFSeg
                add eax, [esi + 4]
                mov [ss:OffEntryPoint], eax
                _break jmp
              _endif
            _endif
          _endif
          lea esi, [esi + ecx + 2]
        _enddo jmp
        mov esi, [ss:OffHdrHandle]
        mov ax, 502h
        call DPMICall
        mov ebx, 10000h
        mov ax, 501h
        call DPMICall
        jc near ErrNoDPMIMemory
        mov [ss:OffStackHandle], esi
        add ebx, 10000h - 16
        mov eax, [ss:OffImageSize]
        add eax, [ss:OffCodeBase]
        sub eax, 16
        mov edx, ebx
        sub edx, 4
        mov [edx], eax
        mov [ss:OffStackTop], edx
        mov dword [eax], 21CD4CB4h
        mov bx, [es:OfflSavedPSP]
        mov ax, 6
        int 31h
        shl ecx, 16
        mov cx, dx
        mov [ss:OffPSPLinear], ecx
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
        mov ecx, [ss:OffCodeBase]
        mov edx, [ss:OffPSPLinear]
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
        mov ebx, ecx
        xor ecx, ecx
        mov ebp, ecx
        mov fs, cx
        mov ax, 502h
        push ss
        pop ds
        retf

SegLen:
        cmp ax, [ss:OffCodeNum]
        _ifnot jne
          mov eax, [ss:OffCodeLen]
          clc
          ret
        _endif
        cmp ax, [ss:OffDataNum]
        _ifnot jne
          mov eax, [ss:OffDataLen]
          clc
          ret
        _endif
        stc
        ret

SegBase:
        cmp ax, [ss:OffCodeNum]
        _ifnot jne
          mov eax, [ss:OffCodeBase]
          clc
          ret
        _endif
        cmp ax, [ss:OffDataNum]
        _ifnot jne
          mov eax, [ss:OffDataBase]
          clc
          ret
        _endif
        cmp ax, [ss:OffBssNum]
        _ifnot jne
          mov eax, [ss:OffBssBase]
          clc
          ret
        _endif
        stc
        ret

        SEGM IEData
        LLabel StartName
        db 'start', 0
        ESEG IEData

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
         mov ecx, edx
         shr ecx, 16
         mov ebx, [ss:OffFileHandle]
         mov ax, 4200h
         ;call XX
         DOSINT 42
         ;int  21h
         _ifnot jc
         ;call EnableTrace
         push ds
         lds edx, [esp + 12 + 12 + 4]
         mov ecx, [esp + 12 + 4 + 4]
         mov ah, 3Fh
         DOSINT
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
         _endif
         pop edx
         pop ecx
         pop ebx
         ret 16

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
               jc near ErrAllocSel
               ;$ifnot jc
               movzx ebx, ax
               mov ax, 0Ch
               mov edi, esp
               push es
               push ss
               pop es
               int 31h
               pop es
               jc near ErrAllocSel
               ;$endif
               ;mov ebx, bx
               add esp, 8
               pop edi
               ret

        ESEG IEText
