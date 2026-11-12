;             This file is part of the ZRDX OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda
;
; PE image loading service: PELoadImage and its file reader.
; Included into IEText, or into the resident EText when PEMAX
; is defined (far call, RETF). Needs no loader data.

%ifdef PEMAX
%define PEDpmi PEDpmiX
%define PERet retf
        DefDPMICall DPMICallE

;DPMICallE that notes "function not supported" (AX = 8001h) in
;PEHostLack, so the service can report ABI error 14.
PEDpmiX:
        call DPMICallE
        jnc .ret
        cmp ax, 8001h
        jne .st
        push fs
        call PELoadFS
        mov dword [fs:OffPEHostLack], 1
        pop fs
.st:    stc
.ret:   ret
%else
%define PEDpmi DPMICall
%define PERet ret
%endif

;Load and relocate the PE image of the PELoadCtx at EBP
;(CtxFile set). Out: CF clear and CtxBase/Handle/Size/Entry
;set; or CF set, AL = 1 read, 2 bad PE, 3 reloc, 4 directory,
;5 TLS, 6 no memory, 7 unsupported format (the image block
;is freed). CtxKind: 0 main EXE (DLL flag clear), 1 DLL (set).
        DPROC PELoadImage
        push ds
        push es
        mov [ebp + CtxSP], esp
        mov dword [ebp + CtxHandle], 0
        mov dword [ebp + CtxDisp], 0
        push ss
        push ebp
        push 0
        push 040h
        call FileRead
        cmp al, 040h
        jne PEFailRead
        mov eax, [ebp + 3Ch]
        mov [ebp + CtxDisp], eax
        push ss
        push ebp
        push 0
        push 040h
        call FileRead
        cmp al, 040h
        jne PEFailRead
;a bound file has the stub first (its e_lfanew leads to the
;image MZ); a plain PE file has "PE" there (task 010: DLLs)
        cmp word [ebp], 'MZ'
        je PEMzOk
        cmp dword [ebp], 'PE'
        jne PEFailBad
        mov edx, [ebp + CtxDisp]
        mov dword [ebp + CtxDisp], 0
        jmp short PEHdrPos
PEMzOk:
        mov edx, [ebp + 3Ch]
PEHdrPos:
        push ss
        push ebp
        mov [ebp + CtxHdrPos], edx
        push edx
        push HdrSize
        call FileRead
        movzx eax, ax
        cmp eax, PEOptHdr + 96
        jb PEFailRead
        mov edi, eax
        cmp dword [ebp], 'PE'
        jne PEBadHeader
        cmp word [ebp + PEMachine], 14Ch
        jne PEFailFmt
        cmp word [ebp + PEOptMagic], 10Bh
        jne PEFailFmt
        test byte [ebp + PEFlags], 2
        jz PEBadHeader
        movzx eax, byte [ebp + PEFlags + 1]
        shr eax, 5
        and eax, 1
        cmp eax, [ebp + CtxKind]
        jne PEBadHeader
        mov eax, [ebp + PESectAlign]
        mov edx, [ebp + PEFileAlign]
        cmp eax, edx
        jb PEBadHeader
        call PEPow2
        jc PEBadHeader
        mov eax, edx
        call PEPow2
        jc PEBadHeader
        movzx ecx, word [ebp + PENumSections]
        or ecx, ecx
        jz PEBadHeader
        cmp ecx, MaxPESections
        ja PEBadHeader
        jmp short PEHeaderFields
PEBadHeader:
        jmp PEFailBad
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
        add eax, [ebp + CtxHdrPos]
        jc PEBadHeader
        cmp [ebp + PEHeadersSize], eax
        jb PEBadHeader
        cmp dword [ebp + PENumDirs], 1
        jb PEImportsOK
        mov eax, [ebp + PEDirExport]
        mov edx, [ebp + PEDirExportSize]
        mov ecx, 40
        call PECheckDir
        cmp dword [ebp + PENumDirs], 2
        jb PEImportsOK
        mov eax, [ebp + PEDirImport]
        mov edx, [ebp + PEDirImportSize]
        mov ecx, 20
        call PECheckDir
%ifndef PEMAX
        mov eax, [ebp + PEDirImport]
        or eax, [ebp + PEDirImportSize]
        jne PEFailImp
%endif
PEImportsOK:
        cmp dword [ebp + PENumDirs], 14
        jb PEDelayOK
        mov eax, [ebp + PEDirDelay]
        or eax, [ebp + PEDirDelaySize]
        jne PEFailImp
PEDelayOK:
        cmp dword [ebp + PENumDirs], 15
        jb PEComOK
        mov eax, [ebp + PEDirCom]
        or eax, [ebp + PEDirComSize]
        jne PEFailImp
PEComOK:
        cmp dword [ebp + PENumDirs], 10
        jb PETlsOK
        mov eax, [ebp + PEDirTLS]
        or eax, [ebp + PEDirTLSSize]
        jz PETlsOK
%ifndef PEMAX
        jmp PEFailTls                    ;checked by PETlsSetup
%endif
PETlsOK:
        mov ebx, [ebp + PEImageSize]
        cmp ebx, [ebp + PEHeadersSize]
        jb PEFailBad
        add ebx, 0FFFh
        jc PEFailBad
        and ebx, ~(0FFFh)
        jz PEFailBad
        mov [ebp + CtxSize], ebx
        mov ax, 501h
        call PEDpmi
        jc PEFailMem
        mov [ebp + CtxHandle], esi
        mov [ebp + CtxBase], ebx
        cmp dword [ebp + CtxKind], 0
        jne PENoBase
        mov [fs:OffImageBaseAddr], ebx
PENoBase:
        mov edi, ebx
        mov ecx, [ebp + CtxSize]
        shr ecx, 2
        xor eax, eax
        push es
        push ds
        pop es
        rep stosd
        pop es
        push ds
        push dword [ebp + CtxBase]
        push 0
        push dword [ebp + PEHeadersSize]
        call FileRead
        cmp eax, [ebp + PEHeadersSize]
        jne PEFailRead
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
            add eax, [ebp + CtxBase]
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
            jne PEFailRead
          _endif
          add esi, PSHeader_size
        _enddo loop
        jmp short PESectionsOK
PEBadSection:
        jmp PEFailBad
PESectionsOK:
        mov edi, [ebp + CtxBase]
        mov edx, edi
        sub edx, [ebp + PEImageBase]
        _ifnot jz
          cmp dword [ebp + PENumDirs], 6
          jb PENoRelocs
          mov ecx, [ebp + PEDirRelocSize]
          or ecx, ecx
          jz PENoRelocs
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
        jmp PEFailReloc
;no relocation records: fine unless the file says they were
;stripped
PENoRelocs:
        test byte [ebp + PEFlags], 1
        jnz PEBadReloc
        jmp PERelocsOK
PERelocsOK:
        mov eax, [ebp + PEEntryRVA]
        cmp eax, [ebp + PEImageSize]
        jae PEFailBad
        add eax, [ebp + CtxBase]
        mov [ebp + CtxEntry], eax
        pop es
        pop ds
        clc
        PERet
;eax = RVA, edx = size, ecx = minimum size; fail unless the
;directory is empty or lies inside the image. Clobbers eax, ebx.
PECheckDir:
        mov ebx, eax
        or ebx, edx
        jz .ok
        cmp edx, ecx
        jb PEBadHeader
        add eax, edx
        jc PEBadHeader
        cmp eax, [ebp + PEImageSize]
        ja PEBadHeader
.ok:    ret
;CF set unless eax is a power of two
PEPow2:
        lea ebx, [eax - 1]
        test ebx, eax
        jnz .no
        or eax, eax
        jz .no
        clc
        ret
.no:    stc
        ret
%macro DefPEFail 2
%1:
        mov al, %2
        jmp short PEFail
%endmacro
        DefPEFail PEFailRead, 1
        DefPEFail PEFailBad, 2
        DefPEFail PEFailReloc, 3
        DefPEFail PEFailImp, 4
        DefPEFail PEFailTls, 5
        DefPEFail PEFailFmt, 7
        DefPEFail PEFailMem, 6
PEFail:
        mov esp, [ebp + CtxSP]
        pop es
        pop ds
        push eax
        mov esi, [ebp + CtxHandle]
        or esi, esi
        _ifnot jz
          mov ax, 502h
          call PEDpmi
        _endif
        pop eax
        stc
        PERet

FileRead:
         push ebx
         push ecx
         push edx
;@@MemSel  EQU DWORD PTR ss:[esp+12].16
;@@MemPtr  EQU DWORD PTR ss:[esp+12].12
;@@FilePtr EQU DWORD PTR SS:[esp+12].8
;@@Size    EQU DWORD PTR SS:[esp+12].4
         mov edx, [esp + 12 + 8]
         add edx, [ebp + CtxDisp]
         jc PEReadError
         mov ecx, edx
         shr ecx, 16
         mov ebx, [ebp + CtxFile]
         mov ax, 4200h
         DOSINT 42
         jc PEReadError
         push ds
         lds edx, [esp + 12 + 12 + 4]
         mov ecx, [esp + 12 + 4 + 4]
         mov ah, 3Fh
         DOSINT
         jc PEReadError
         pop ds
         pop edx
         pop ecx
         pop ebx
         ret 16

PEReadError:
         jmp PEFailRead
