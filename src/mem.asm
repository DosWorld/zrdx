;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

        SEGM Text
malloc:
        DPMIFn 5, 1
%define _newmcb eax
%define _newmcbw ax
%define _offset edi
%define _offsetw di
%define _newsize ebx
        pushad
        push ds
        pop es
        shl ebx, 16
        mov bx, cx
        or ebx, ebx
        jz near MemErrVal
        add _newsize, 1000h - 1
        jc near MemErrNoMem
        and _newsize, ~(0FFFh)
        call FindLinWindow
        call MakeNewMCB
        push _newsize
        push _offset
        push 1
        call AllocPages
        _ifnot jnc
          call FreeMCB
          jmp MemErrNoMem
        _endif
ReturnMalloc:
        mov word [esp + PA_ECX], _offsetw
        shr _offset, 16
        mov word [esp + PA_EBX], _offsetw
        mov word [esp + PA_EDI], _newmcbw
        shr _newmcb, 16
        mov word [esp + 32 + DPMIFrame_ESI], _newmcbw
        popad
        ret


free:
        DPMIFn 5, 2
%define _newmcb eax
        pushad
        push ds
        pop es
        mov esi, [esp + 32 + DPMIFrame_ESI]
        shl esi, 16
        mov si, di
        cmp esi, OffMCBVector
        jb Err@16
        cmp esi, [OffMCBVectorEnd]
        jae Err@16
        test esi, 0Fh
        jnz Err@16
        cmp dword [esi + MCB_StartOffset], 0
        je Err@16
        mov ecx, [esi + MCB_EndOffset]
        sub ecx, [esi + MCB_StartOffset]
        push ecx
        push dword [esi + MCB_StartOffset]
        push 1
        call FreePages
        xchg _newmcb, esi
        call FreeMCB
        popad
        ret
Err@16:
        jmp MemErrHandle

MemError3:
        pop ebp
MemError2:
        pop ebp
MemError1:
        pop ebp
MemError:
        mov byte [esp + PA_EAX], al
        popad
        jmp DPMIError1
MemErrVal:
        mov al, 21h
        jmp MemError
MemErrNoMem:
        mov al, 12h
        jmp MemError
MemErrHandle:
        mov al, 23h
        jmp MemError

realloc:
        DPMIFn 5, 3
%define _newmcb eax
%define _newsize ebx
%define _optsize ebp
%define _curmcb esi
%define _optmcb ecx
_err equ MemErrHandle
        pushad
        push ds
        pop es
        shl ebx, 16
        mov esi, [esp + 32 + DPMIFrame_ESI]
        shl esi, 16
        mov bx, cx
        mov si, di
        or ebx, ebx
        jz near MemErrVal
        add _newsize, 1000h - 1
        jc near MemErrNoMem
        and _newsize, ~(0FFFh)
        cmp esi, OffMCBVector
        jb _err
        cmp esi, [OffMCBVectorEnd]
        jae _err
        test esi, 0Fh
        jne _err
        cmp dword [esi + MCB_StartOffset], 0
        je _err
        mov _optmcb, dword [esi + MCB_Next]
        mov _optsize, dword [ecx + MCB_StartOffset]
        sub _optsize, dword [esi + MCB_StartOffset]
        mov _optmcb, _curmcb
        cmp _optsize, _newsize
        _ifnot jb
          call FindLinWindowX
        _else jmp
          call FindLinWindow
        _endif
        cmp _curmcb, _optmcb
        _ifnot je, near
          call MakeNewMCB
          ;alloc 2 new pages;
          push _newmcb
          push _curmcb
          ;must preserve @@CurMCB, @@NewMCB, @@NewSize (esi, ebx, eax)
          push eax
          push eax        ;reserve space for @@GCount
          lea edx, [esp - 8]
          _do
%ifndef VMM
            push edi             ;allocate space for phisical address
            mov edi, esp        ;set pointer to it
            xor ecx, ecx
            inc ecx             ;mov 1 to ecx
            push esi             ;preserve esi
            call XAllocPages     ;allocate 1 page
            pop esi
            jz FreeAndErr@18
%else
            call alloc_page
            jc FreeAndErr@18
            push eax
%endif
          cmp esp, edx        ;test for end
          _enddo jne
%define _msize edi
%define _t ebp
%define _t1 ecx
          mov _msize, dword [esi + MCB_EndOffset]
          sub _msize, dword [esi + MCB_StartOffset] ;calculate size of the current block
          mov _t, _newsize
          sub _t, _msize
          _ifnot jb
            mov _t1, dword [eax + MCB_EndOffset]
            sub _t1, _t
            push _t
            push _t1
            push 1
            call AllocPages
            _toendif jnc
FreeAndErr@18:
            add edx, 8
            _do jmp
%ifndef VMM
              mov esi, esp
              xor ecx, ecx
              inc ecx
              call XFreePages
              pop esi
%else
              call free_page
%endif
            _while
            cmp esp, edx
            _enddo jne
            pop esi
            jmp MemErrNoMem
          _else ; jmp
            neg _t
            mov _t1, dword [esi + MCB_EndOffset]
            sub _t1, _t
            push _t
            push _t1
            push 1
            call FreePages
            mov _msize, _newsize
          _endif
          add edx, 8          ;restore pointer to @@GCount and stack base
          ;all OK, move pages now
%define _paddr0 ebp
%define _paddr1 ebx
%define _dirindex eax
%define _sdirindex0 esi
%define _sdirindex1 edi
%define _lcount ecx
          mov _paddr0, dword [esi + MCB_StartOffset]
          mov _paddr1, dword [eax + MCB_StartOffset]
          shr _paddr0, 12
          shr _paddr1, 12
          shr _msize, 12
          mov [edx], _msize
          cld
          _do
          mov _lcount, 3FFh
          mov _sdirindex0, _paddr0
          and _sdirindex0, _lcount
          and _lcount, _paddr1
          lea _sdirindex1, [OffSDir1Win + (_lcount * 4)]
          cmp _lcount, _sdirindex0
          _ifnot ja
            mov _lcount, _sdirindex0
          _endif
          lea _sdirindex0, [OffSDir0Win + (_sdirindex0 * 4)]
          sub _lcount, 400h
          neg _lcount
          cmp _lcount, [edx] ; @@GCount
          _ifnot jb
            mov _lcount, [edx] ; @@GCount
          _endif
          mov _dirindex, _paddr1
          shr _dirindex, 10
          lea _dirindex, [OffPageDir + (_dirindex * 4)]
          cmp dword [eax + OffnEntriesInTable - OffPageDir], 0
          _ifnot jnz
            pop dword [eax]
          _endif
          add dword [eax + OffnEntriesInTable - OffPageDir], _lcount
          mov eax, [eax]
          mov [OffPage2 + ((OffSDir1Win - KernelBase) >> 10)], eax
          mov _dirindex, _paddr0
          shr _dirindex, 10
          lea _dirindex, [OffPageDir + (_dirindex * 4)]
          push dword [eax]
          pop dword [OffPage2 + ((OffSDir0Win - KernelBase) >> 10)]
          sub dword [eax + OffnEntriesInTable - OffPageDir], _lcount
          _ifnot jnz
            push dword [eax]
            and dword [eax], 0
          _endif
          add _paddr0, _lcount
          add _paddr1, _lcount
          InvalidateTLB
          xor eax, eax
          sub [edx], _lcount          ;[esp] = @@GCount
          push ecx
          push esi
          rep movsd
          pop edi
          pop ecx
          rep stosd
          ;mov  @@GCount, [edx]
          _enddo jnz

          ;add  edx, 8
          _do jmp
%ifndef VMM
            mov esi, esp
            xor ecx, ecx
            inc ecx
            call XFreePages
            pop esi
%else
            call free_page
%endif
          _while
          cmp esp, edx
          _enddo jne
          pop esi                 ;add esp, 4
          pop _newmcb                 ;free space for @@GCount
          pop _newmcb
          call FreeMCB
          pop eax
ReturnMalloc@18:
          mov edi, [eax + MCB_StartOffset]      ;@@Offset
          jmp ReturnMalloc
        _else
          mov _msize, dword [esi + MCB_EndOffset]
          sub _msize, dword [esi + MCB_StartOffset] ;calculate size of the current block
          sub _msize, _newsize
          _ifnot jae
            neg _msize
            push _msize
            push dword [esi + MCB_EndOffset]
            push 1
            call AllocPages
            jc near MemErrNoMem
            add dword [esi + MCB_EndOffset], _msize
          _else jmp
            sub dword [esi + MCB_EndOffset], _msize
            push _msize
            push dword [esi + MCB_EndOffset]
            push 1
            call FreePages
          _endif
          xchg eax, _curmcb
          jmp ReturnMalloc@18
        _endif
;@@Err:  jmp MemError

FindLinWindow:
%define _newmcb eax
%define _newsize ebx
%define _optsize ebp
%define _curmcb esi
%define _optmcb ecx
%define _maxcount edx
%define _count edi
%define _c eax
%define _t_size esi
        mov _optmcb, dword [OffRootMCB + MCB_Prev]
        mov _optsize, dword [OffRootMCB + MCB_StartOffset] ; 0; 0FFF00000h ;LastMappedPage   !!!!!!!
        sub _optsize, dword [ecx + MCB_EndOffset]
        cmp _optsize, _newsize
        _ifnot jae
          or _optsize, -(1)
        _endif
FindLinWindowX:
        push _t_size
        push _optmcb
        mov _maxcount, dword [Offnmemblocks]
        cmp _maxcount, 10000
        _ifnot jb
          mov _maxcount, 10000
        _endif
        mov _count, 50
        cmp _count, _maxcount
        _ifnot jb
          mov _count, _maxcount
        _endif
        sub _maxcount, _count
        mov _c, dword [OffMemRover]
        inc _maxcount
        _do  ;jmp
        dec _count
        jz CheckEnd@20
Continue@20:
        mov _t_size, dword [eax + MCB_StartOffset]
        mov _c, dword [eax + MCB_Prev]
        sub _t_size, dword [eax + MCB_EndOffset]
        cmp _t_size, _newsize
        _loop jb
        cmp _t_size, _optsize
        _loop jae
        mov _optsize, _t_size
        mov _optmcb, _c
        or _maxcount, _maxcount
        _enddo jnz
Exit@20:
        mov dword [OffMemRover], _c
        pop _t_size        ;add esp, 4
        pop _t_size        ;restore value, destroyed by @@t_size
        ret
CheckEnd@20:
        cmp _maxcount, 1
        je L1@20
        cmp _optmcb, [esp]
        jne Exit@20
L1@20:
        xchg _count, _maxcount
        or _count, _count
        jnz Continue@20
        cmp _optsize, -(1)
        jne Exit@20
        mov al, 12h
        jmp MemError3


MakeNewMCB:
%define _newmcb eax
%define _newsize ebx
%define _c edx
%define _count edi
%define _optmcb ecx
%define _nextmcb edi
%define _prevmcb edx
%define _offset edi
        mov _c, dword [OffFMemRover]
        or _c, _c
        jz AllocMCB@22
        mov _newmcb, _c
        mov _count, 14
        _do
          dec _count
          jz Exit1@22
L0@22:
          mov _c, dword [edx + MCB_Next]
          cmp _c, _newmcb
          _loop ja
          mov _newmcb, _c
          dec _count
          jnz L0@22
        _enddo
Exit1@22:
        mov _nextmcb, dword [eax + MCB_Next]  ;delete MCB from free list
        mov _prevmcb, dword [eax + MCB_Prev]
        mov dword [edi + MCB_Prev], _prevmcb
        mov dword [edx + MCB_Next], _nextmcb
        cmp _newmcb, dword [OffFMemRover]
        _ifnot jne
          cmp _nextmcb, _newmcb
          _ifnot jne
            xor _nextmcb, _nextmcb
          _endif
          mov dword [OffFMemRover], _nextmcb
        _endif
Exit@22:
;new MCB succefully allocated, now setup it
;insert new MCB in the allocated list
        mov _nextmcb, dword [ecx + MCB_Next]
        mov dword [eax + MCB_Next], _nextmcb
        mov dword [eax + MCB_Prev], _optmcb
        mov dword [ecx + MCB_Next], _newmcb
        mov dword [edi + MCB_Prev], _newmcb
;set start & end locations for new MCB
        mov _offset, dword [ecx + MCB_EndOffset]
        mov dword [eax + MCB_StartOffset], _offset
        lea _c, [edi + ebx]
        mov dword [eax + MCB_EndOffset], _c
        inc dword [Offnmemblocks]
        ret
AllocMCB@22:
        mov _newmcb, dword [OffMCBVectorEnd]
        cmp _newmcb, 800000h ; OffMCBVector+
        jae Error@22
        push MCBStruct_size
        push _newmcb
        push 0
        call AllocPages
        jc Error@22
        add dword [OffMCBVectorEnd], MCBStruct_size
        jmp Exit@22
Error@22:
        mov al, 12h
        jmp MemError1


FreeMCB:
%define _mcb eax
%define _mcb1 ecx
%define _nextmcb edx
%define _prevmcb edi
%define _nextmcb1 edx
%define _prevmcb1 edi
%define _rover edx
%define _next edi
%define _size edi
        dec dword [Offnmemblocks]
        mov _nextmcb, dword [eax + MCB_Next]     ;exclude from allocated list
        mov _prevmcb, dword [eax + MCB_Prev]
        mov dword [edx + MCB_Prev], _prevmcb
        mov dword [edi + MCB_Next], _nextmcb
        cmp _mcb, dword [OffMemRover]
        _ifnot jne
          mov dword [OffMemRover], _prevmcb
        _endif
        mov _rover, dword [OffFMemRover]
        or _rover, _rover
        _ifnot jne
          mov _rover, _mcb
        _else jmp
          mov ecx, 10
          _do
          mov _rover, dword [edx + MCB_Next]
          _enddo loop
          mov _next, dword [edx + MCB_Next]
          mov dword [eax + MCB_Next], _next
          mov dword [edi + MCB_Prev], _mcb
        _endif
        mov dword [eax + MCB_Prev], _rover
        mov dword [edx + MCB_Next], _mcb
        ;insert index in the free indexes list
        and dword [eax + MCB_StartOffset], 0
        mov _mcb1, dword [OffMCBVectorEnd]
        _do jmp
          cmp dword [ecx - MCBStruct_size + MCB_StartOffset], 0
          _break jne
          sub _mcb1, MCBStruct_size
          mov _nextmcb1, dword [ecx + MCB_Next]
          mov _prevmcb1, dword [ecx + MCB_Prev]
          mov dword [edx + MCB_Prev], _prevmcb1
          mov dword [edi + MCB_Next], _nextmcb1
          cmp _mcb, _mcb1
          _ifnot jne
            mov _mcb, _prevmcb1
            cmp _nextmcb1, _mcb1
            _ifnot jne
              xor _mcb, _mcb
            _endif
          _endif
        _while
          cmp _mcb1, OffMCBVector
        _enddo ja
        mov _size, dword [OffMCBVectorEnd]
        sub _size, _mcb1
        _ifnot je
          push _size
          push _mcb1
          push 0
          call FreePages
          mov dword [OffMCBVectorEnd], _mcb1
        _endif
        mov dword [OffFMemRover], _mcb
        ret


        DPROC CleanMemVector
        push ebx
        push esi
        push eax
        mov ebx, OffMCBVector
        mov esi, ebx
        _do jmp

        cmp dword [ebx + MCB_StartOffset], 0
        _ifnot je
        mov eax, [ebx + MCB_EndOffset]
        sub eax, [ebx + MCB_StartOffset]
        push eax
        push dword [ebx + MCB_StartOffset]
        push 1
        call FreePages
        _endif
        add ebx, MCBStruct_size
        _while
        cmp ebx, [OffMCBVectorEnd]
        _enddo jb
        sub ebx, esi
        push ebx
        push esi
        push 1
        call FreePages
        pop eax
        pop esi
        pop ebx
        ret

%ifndef Release
        DPROC Random
        mov eax, [OffSeed]
        imul eax, 015a4e35h
        inc eax
        mov [OffSeed], eax
        ret

;Vl = 500
;TVector DD Vl dup(0)
;.code
        DPROC MemTest_
        pushad
        mov ecx, Vl
        mov edi, OffTVector
        xor eax, eax
        rep stosd
        mov ecx, 0000
        _do
        call Random
        xor edx, edx
        mov esi, Vl
        div esi
        lea ebp, [edx*4 + OffTVector]
        cmp dword [ebp], 0
        _ifnot jne
          call Random
          xor edx, edx
          mov esi, 200000 * 10 ;400000
          div esi
          push ecx
          lea ecx, [edx + 10]
          mov ebx, ecx
          shr ebx, 16        ;bx:cx - linear address
          mov ax, 501h
          int 31h
          pop ecx
          jc Exit1@30
          shl esi, 16
          mov si, di
          mov [ebp], esi
        _else jmp
          mov edi, [ebp]
          mov dword [ebp], 0
          mov esi, edi
          shr esi, 16
          mov ax, 502h
          int 31h
          jc Exit2@30
        _endif
        mov ah, 1
        sti
        int 16h
        cli
        _break jnz
        inc ecx
        mov ax, 0DE03h
        VCPITrap
        push edi
        push edx
        push ecx
        push dword [OffMCBVectorEnd]
        push dword [OffnFreePages]

        ;push esp 5 17
        ;call MemDump
        add esp, 5 * 4
        _enddo jmp
ret@30:
        sti
        mov ah, 0
        int 16h
        cli
        popad
        ret
Exit1@30: ; ;mov word ptr ds:[0B8000h+160*6+50], 1F31h
        jmp ret@30
Exit2@30: ; ;mov word ptr ds:[0B8000h+160*6+50], 1F32h
        jmp ret@30

%endif
        ESEG Text
;end


        SEGM Text
        LByte DPMITable2N
        db 0Eh, 3, 6, 7, 1, 4, 5, 4, 1, 3, 1, 4
        align 4
        DPMIRow 0, DPMIFn_0_0, DPMIFn_0_1, DPMIFn_0_2, DPMIFn_0_3, InvalidFunction, InvalidFunction, DPMIFn_0_6, DPMIFn_0_7, DPMIFn_0_8, DPMIFn_0_9, DPMIFn_0_10, DPMIFn_0_11, DPMIFn_0_12, DPMIFn_0_13
        DPMIRow 1, DPMIFn_1_0, DPMIFn_1_1, DPMIFn_1_2
        DPMIRow 2, DPMIFn_2_0, DPMIFn_2_1, DPMIFn_2_2, DPMIFn_2_3, DPMIFn_2_4, DPMIFn_2_5
        DPMIRow 3, DPMIFn_3_0, DPMIFn_3_1, DPMIFn_3_2, DPMIFn_3_3, DPMIFn_3_4, DPMIFn_3_5, DPMIFn_3_6
        DPMIRow 4, DPMIFn_4_0
        DPMIRow 5, DPMIFn_5_0, DPMIFn_5_1, DPMIFn_5_2, DPMIFn_5_3
        DPMIRow 6, DPMIFn_6_0, DPMIFn_6_1, DPMIFn_6_2, DPMIFn_6_3, DPMIFn_6_4
        DPMIRow 7, InvalidFunction, InvalidFunction, DPMIFn_7_2, DPMIFn_7_3
        DPMIRow 8, DPMIFn_8_0
        DPMIRow 9, DPMIFn_9_0, DPMIFn_9_1, DPMIFn_9_2
        DPMIRow A, DPMIFn_A_0
        DPMIRow B, InvalidFunction, InvalidFunction, InvalidFunction, InvalidFunction
        LDWord DPMITable2
        dd OffTable20, OffTable21, OffTable22, OffTable23, OffTable24, OffTable25
        dd OffTable26, OffTable27, OffTable28, OffTable29, OffTable2A, OffTable2B
        ESEG Text
