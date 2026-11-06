;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; PE import graph loading and binding (PEMAX only).
; PEBind loads every missing dependency of the registered
; modules (iteratively, so cycles and imports from the main EXE
; need no recursion) and then fills the import address tables.
; It runs on the application stack. See docs/PEABI.MD.

;Frame of PEBind, ebp = its start (the PELoadCtx comes first).
FDir equ PECtxSize
FPath equ FDir + 264
FNames equ FPath + 300
FMark equ FNames + 2 * NameMax
FIdx equ FMark + 4
FDesc equ FIdx + 4
FCnt equ FDesc + 4
FThunk equ FCnt + 4
FLook equ FThunk + 4
FProv equ FLook + 4
FRec equ FProv + 4
FCs equ FRec + 4
FTramp equ FCs + 4
FTrampH equ FTramp + 4
FSeq equ FTrampH + 4
FCur equ FSeq + 4
FDepth equ FCur + 4
FRec2 equ FDepth + 4
FSeq0 equ FRec2 + 4
FStart equ FSeq0 + 4
FTarget equ FStart + 4
FUser equ FTarget + 4
FRsv equ FUser + 264
FFi equ FRsv + 4
FDllSp equ FFi + 4
FSize equ FDllSp + 4

;ss:edi = directory part of the program path (with the final
;separator), empty if unknown.
PEEnvDir:
        mov byte [ss:edi], 0
        push ds
        call PEEnvPtr
        jc .out
        xor edx, edx
        xor ebx, ebx
.l:     lodsb
        test al, al
        jz .end
        cmp edx, 262
        jae .no
        mov [ss:edi + edx], al
        inc edx
        cmp al, '\'
        je .s
        cmp al, '/'
        je .s
        cmp al, ':'
        jne .l
.s:     mov ebx, edx
        jmp .l
.end:   mov byte [ss:edi + ebx], 0
        jmp .out
.no:    mov byte [ss:edi], 0
.out:   pop ds
        ret

;ecx = index. Out: CF clear and edx = record, or CF set.
PERegNth:
        push fs
        call PELoadFS
        mov esi, [fs:OffPEModHead]
        pop fs
.c:     or esi, esi
        jz .no
        mov eax, [es:esi + 4]
        cmp ecx, eax
        jb .hit
        sub ecx, eax
        mov esi, [es:esi]
        jmp .c
.hit:   imul ecx, ecx, RecSz
        lea edx, [esi + ecx + 16]
        clc
        ret
.no:    stc
        ret

;eax = RVA of an ASCIIZ string, edx = record. Out: CF clear and
;esi = flat address, or CF set if outside the image or unended.
PEImgStr:
        mov ecx, [es:edx + RecImgSize]
        sub ecx, eax
        jbe .bad
        mov esi, [es:edx + RecBase]
        add esi, eax
        push esi
.s:     cmp byte [es:esi], 0
        je .z
        inc esi
        dec ecx
        jnz .s
        pop esi
.bad:   stc
        ret
.z:     pop esi
        clc
        ret

;eax = RVA of the DLL name, [ebp + FRec] = record. Normalizes it
;into the first name of the frame. CF set if invalid.
PEDescName:
        mov edx, [ebp + FRec]
        call PEImgStr
        jc .out
        push ds
        push es
        pop ds
        lea edi, [ebp + FNames]
        call PENormName
        pop ds
.out:   ret

;edx = record: [ebp + FDesc] and [ebp + FCnt] = its import
;descriptors. CF clear and ZF set if the record has none.
PEImpInit:
        mov [ebp + FRec], edx
        mov eax, [es:edx + RecImpRVA]
        or eax, eax
        jz .none
        add eax, [es:edx + RecBase]
        mov [ebp + FDesc], eax
        mov eax, [es:edx + RecImpSize]
        xor edx, edx
        mov ebx, 20
        div ebx
        mov [ebp + FCnt], eax
        or eax, eax
        jz .none
        or esp, esp
        clc
        ret
.none:  xor eax, eax
        clc
        ret

;PEProcLookup that keeps ebp.
PEProc2:
        push ebp
        call PEProcLookup
        pop ebp
        ret

;Open the file ss:edx for reading (LFN first). Out: eax = handle.
PEOpenRead:
        push ds
        push esi
        push ss
        pop ds
        mov esi, edx
        mov bx, 20h
        xor ecx, ecx
        xor edx, edx
        inc edx
        mov ax, 716Ch
        stc
        int 21h
        jnc .ok
        mov edx, esi
        mov eax, 3D20h
        int 21h
.ok:    movzx eax, ax
        pop esi
        pop ds
        ret

;Load the DLL named [ebp + FNames] (upper case) and register it.
;Out: CF clear, or CF set and eax = 1 open, 2 bad image,
;5 name collision, 6 no memory.
PELoadDll:
        lea edi, [ebp + FPath]
        lea esi, [ebp + FDir]
.d:     mov al, [ss:esi]
        inc esi
        test al, al
        jz .n
        mov [ss:edi], al
        inc edi
        jmp .d
.n:     lea esi, [ebp + FNames]
.m:     mov al, [ss:esi]
        inc esi
        mov [ss:edi], al
        inc edi
        test al, al
        jnz .m
        cmp byte [ebp + FUser], 0
        je .std
        lea edx, [ebp + FUser]
        call PEOpenRead
        jnc .got
.std:   lea edx, [ebp + FPath]
        call PEOpenRead
        jnc .got
        lea edx, [ebp + FNames]
        call PEOpenRead
        jc .nofile
.got:   mov [ebp + CtxFile], eax
        mov dword [ebp + CtxKind], 1
        push ds
        push es
        pop ds
        push cs
        call PELoadImage
        pop ds
        pushfd
        push eax
        mov ebx, [ebp + CtxFile]
        mov ah, 3Eh
        int 21h
        pop eax
        popfd
        jnc .reg
        cmp al, 1
        je .ret1
        cmp al, 6
        je .ret6
        mov eax, 2
        stc
        ret
.ret1:  mov eax, 1
        stc
        ret
.ret6:  mov eax, 6
        stc
        ret
.nofile: mov eax, 1
        stc
        ret
.reg:   lea edi, [ebp + FNames + NameMax]
        call PEExpName
        lea esi, [ebp + FNames]
        call PERegFill
        jnc .ok
        cmp al, 9
        mov eax, 5
        je .bad
        mov eax, 6
.bad:   stc
.ok:    ret

;[ebp + FRec] = record, [ebp + FFi] = next export index. Out: CF
;clear and FNames = module of the next forwarder; CF set at the end.
PEFwdNext:
        mov esi, [ebp + FRec]
        mov edi, [es:esi + RecExpRVA]
        or edi, edi
        jz .end
        cmp dword [es:esi + RecExpSize], 40
        jb .end
        mov ecx, [es:esi + RecImgSize]
        mov ebx, [es:esi + RecBase]
        add edi, ebx
        mov eax, [es:edi + 20]
        mov edx, ecx
        shr edx, 2
        cmp eax, edx
        ja .end
        shl eax, 2
        add eax, [es:edi + 28]
        jc .end
        cmp eax, ecx
        ja .end
.n:     mov eax, [ebp + FFi]
        cmp eax, [es:edi + 20]
        jae .end
        inc dword [ebp + FFi]
        mov edx, [es:edi + 28]
        add edx, ebx
        mov eax, [es:edx + eax*4]
        mov edx, [es:esi + RecExpRVA]
        cmp eax, edx
        jb .n
        add edx, [es:esi + RecExpSize]
        cmp eax, edx
        jae .n
        mov ecx, [es:esi + RecImgSize]
        sub ecx, eax
        jbe .n
        add eax, ebx
        lea edi, [ebp + FNames]
        call PEFwdModName
        jc .n
        clc
        ret
.end:   stc
        ret

;edx = record: load the DLLs it imports. CF set, eax = code.
PELoadDeps:
        call PEImpInit
        jz .fw
.d:     cmp dword [ebp + FCnt], 0
        je .done
        mov esi, [ebp + FDesc]
        mov eax, [es:esi + 12]
        or eax, eax
        jz .fw
        call PEDescName
        jc .bad
        lea edi, [ebp + FNames]
        call PERegFind
        jnc .have
        call PELoadDll
        jc .err
.have:  add dword [ebp + FDesc], 20
        dec dword [ebp + FCnt]
        jmp .d
.fw:    mov dword [ebp + FFi], 0
.fl:    call PEFwdNext
        jc .done
        lea edi, [ebp + FNames]
        call PERegFind
        jnc .fl
        call PELoadDll
        jc .err
        jmp .fl
.done:  clc
.err:   ret
.bad:   mov eax, 4
        stc
        ret

;edx = record: fill its import address tables.
;CF set, eax = 3 unresolved, 4 bad import table.
PEBindRec:
        call PEImpInit
        jz .done
.d:     cmp dword [ebp + FCnt], 0
        je .done
        mov esi, [ebp + FDesc]
        mov eax, [es:esi + 12]
        or eax, eax
        jz .done
        call PEDescName
        jc .bad
        lea edi, [ebp + FNames]
        call PERegFind
        jc .bad
        mov [ebp + FProv], esi
        mov esi, [ebp + FDesc]
        mov eax, [es:esi + 16]
        or eax, eax
        jz .bad
        mov ecx, [es:esi]
        or ecx, ecx
        jnz .o
        cmp dword [es:esi + 4], -1
        je .bad
        mov ecx, eax
.o:     mov edx, [ebp + FRec]
        mov ebx, [es:edx + RecBase]
        add eax, ebx
        add ecx, ebx
        mov [ebp + FThunk], eax
        mov [ebp + FLook], ecx
.t:     mov edx, [ebp + FRec]
        mov ebx, [es:edx + RecBase]
        mov ecx, [es:edx + RecImgSize]
        mov eax, [ebp + FThunk]
        sub eax, ebx
        jb .bad
        add eax, 4
        jc .bad
        cmp eax, ecx
        ja .bad
        mov eax, [ebp + FLook]
        sub eax, ebx
        jb .bad
        add eax, 4
        jc .bad
        cmp eax, ecx
        ja .bad
        mov eax, [ebp + FLook]
        mov eax, [es:eax]
        or eax, eax
        jz .next
        test eax, 80000000h
        jnz .ord
        add eax, 2
        jc .bad
        call PEImgStr
        jc .bad
        mov edx, esi
        jmp .look
.ord:   test eax, 7FFF0000h
        jnz .bad
        movzx edx, ax
.look:  mov esi, [ebp + FProv]
        call PEProc2
        jc .lerr
        mov ecx, [ebp + FThunk]
        mov [es:ecx], eax
        add dword [ebp + FThunk], 4
        add dword [ebp + FLook], 4
        jmp .t
.next:  add dword [ebp + FDesc], 20
        dec dword [ebp + FCnt]
        jmp .d
.done:  clc
        ret
.lerr:  cmp eax, 3
        je .bad
        mov eax, 3
        stc
        ret
.bad:   mov eax, 4
        stc
        ret

;edx = record, ecx = reason (1 attach, 0 detach). Calls the
;DllMain of a DLL on the flat code selector through a trampoline
;(jmp far PEDllRet) because DllMain returns with a near RET; it
;may pop its arguments (stdcall) or not (cdecl): PEDllRet reloads
;ESP from FDllSp and EBP must survive the call.
;Out: eax = its result (1 if the module has no entry point).
PEDllMain:
        mov ebx, [es:edx + RecEntry]
        cmp ebx, [es:edx + RecBase]
        jne .call
        mov eax, 1
        ret
.call:  mov [ebp + FDllSp], esp
        push dword [ebp + FRsv]
        push ecx
        push edx
        push dword [ebp + FTramp]
        push dword [ebp + FCs]
        push ebx
        retf
        DPROC PEDllRet
        mov esp, [ebp + FDllSp]         ;cdecl or stdcall DllMain
        cld
        ret

;edx = record: initialize its imports first, then the module.
;Modules being initialized or done are skipped (cycles).
;Out: CF set and eax = 7 if a DllMain failed, 6 if too deep.
PEAttach:
        cmp dword [es:edx + RecState], 0
        jne .skip
        inc dword [ebp + FDepth]
        cmp dword [ebp + FDepth], 200
        ja .deep
        mov dword [es:edx + RecState], 1
        push edx
        call PEImpInit
        jz .self
        push dword [ebp + FDesc]
        push dword [ebp + FCnt]
.d:     cmp dword [esp], 0
        je .done
        mov ecx, [esp + 4]
        mov eax, [es:ecx + 12]
        or eax, eax
        jz .done
        mov edx, [esp + 8]
        mov [ebp + FRec], edx
        call PEDescName
        jc .bad
        lea edi, [ebp + FNames]
        call PERegFind
        jc .bad
        mov edx, esi
        call PEAttach
        jc .fail
        add dword [esp + 4], 20
        dec dword [esp]
        jmp .d
.done:  add esp, 8
.self:  push dword 0
.f:     mov eax, [esp]
        mov [ebp + FFi], eax
        mov edx, [esp + 4]
        mov [ebp + FRec], edx
        call PEFwdNext
        jc .fdone
        mov eax, [ebp + FFi]
        mov [esp], eax
        lea edi, [ebp + FNames]
        call PERegFind
        jc .f
        mov edx, esi
        call PEAttach
        jc .ffail
        jmp .f
.fdone: add esp, 4
        pop edx
        mov ecx, 1
        cmp dword [es:edx + RecKind], 0
        je .mark
        mov [ebp + FRec2], edx
        call PEDllMain
        mov edx, [ebp + FRec2]
        or eax, eax
        jz .refused
.mark:  mov eax, [ebp + FSeq]
        mov [es:edx + RecSeq], eax
        inc dword [ebp + FSeq]
        mov dword [es:edx + RecState], 2
        dec dword [ebp + FDepth]
.skip:  clc
        ret
.refused:
        mov dword [es:edx + RecState], 0
        lea esi, [edx + RecName1]
        lea edi, [ebp + FNames]
        mov ecx, NameMax / 4
        rep movsd
        dec dword [ebp + FDepth]
        mov eax, 7
        stc
        ret
.ffail: lea esp, [esp + 8]
        dec dword [ebp + FDepth]
        stc
        ret
.bad:   mov eax, 4
.fail:  lea esp, [esp + 12]
        dec dword [ebp + FDepth]
        stc
        ret
.deep:  dec dword [ebp + FDepth]
        mov eax, 6
        stc
        ret

;Detach (DllMain reason 0) every initialized DLL, newest first.
PEDetachAll:
        mov eax, [ebp + FSeq]
        mov [ebp + FCur], eax
.s:     dec dword [ebp + FCur]
        mov eax, [ebp + FCur]
        cmp eax, [ebp + FSeq0]
        jb .done
        mov dword [ebp + FIdx], 0
.r:     mov ecx, [ebp + FIdx]
        call PERegNth
        jc .s
        inc dword [ebp + FIdx]
        mov eax, [ebp + FCur]
        cmp [es:edx + RecSeq], eax
        jne .r
        cmp dword [es:edx + RecState], 2
        jne .r
        mov [ebp + FRec2], edx
        xor ecx, ecx
        cmp dword [es:edx + RecKind], 0
        je .clr
        call PEDllMain
        mov edx, [ebp + FRec2]
.clr:   mov dword [es:edx + RecState], 0
        jmp .r
.done:  ret

;Trampoline block for PEDllMain (CF set and eax = 6 on failure).
PETrampOn:
%ifdef FAKELACK
        push fs                         ;test build only (task 169)
        call PELoadFS
        inc dword [fs:OffPEFakeCnt]
        cmp dword [fs:OffPEFakeCnt], FAKELACK
        jne .real
        mov dword [fs:OffPEHostLack], 1
        pop fs
        mov eax, 6
        stc
        ret
.real:  pop fs
%endif
        mov ebx, 4096
        mov ax, 501h
        call PEDpmi
        jc .no
        mov [ebp + FTramp], ebx
        mov [ebp + FTrampH], esi
        mov byte [es:ebx], 0EAh         ;jmp far PEDllRet
        mov dword [es:ebx + 1], OffPEDllRet
        mov [es:ebx + 5], cs
        clc
        ret
.no:    mov eax, 6
        ret

PETrampOff:
        mov esi, [ebp + FTrampH]
        mov ax, 502h
        call PEDpmi
        ret

;Shared by the program start and LoadLibrary. ebp = frame with
;FCs, FMark (registry mark), FStart (first new record), FTarget
;(record to initialize) and FUser set; es = ds = flat.
;Out: CF clear, or CF set and eax = 1 open, 2 bad image, 3 unresolved,
;4 bad table, 5 name conflict, 6 no memory, 7 DllMain refused.
PEGraph:
        push fs
        call PELoadFS
        mov eax, [fs:OffPESeq]
        pop fs
        mov [ebp + FSeq], eax
        mov [ebp + FSeq0], eax
        mov dword [ebp + FDepth], 0
        call PETrampOn
        jc .nomem
        lea edi, [ebp + FDir]
        call PEEnvDir
        mov eax, [ebp + FStart]
        mov [ebp + FIdx], eax
.scan:  mov ecx, [ebp + FIdx]
        call PERegNth
        jc .bind
        call PELoadDeps
        jc .fail
        inc dword [ebp + FIdx]
        jmp .scan
.bind:  mov eax, [ebp + FStart]
        mov [ebp + FIdx], eax
.b1:    mov ecx, [ebp + FIdx]
        call PERegNth
        jc .ok
        call PEBindRec
        jc .fail
        inc dword [ebp + FIdx]
        jmp .b1
.ok:    mov ecx, [ebp + FTarget]
        call PERegNth
        jc .done
        call PEAttach
        jnc .done
        push eax
        call PEDetachAll
        pop eax
        jmp .fail
.done:  push fs
        call PELoadFS
        mov eax, [ebp + FSeq]
        mov [fs:OffPESeq], eax
        pop fs
        xor eax, eax
        jmp .free
.nomem: mov eax, 6
        stc
        ret
.fail:  push eax
        mov ecx, [ebp + FMark]
        call PERegRollback
        pop eax
        stc
.free:  pushfd
        push eax
        call PETrampOff
        pop eax
        popfd
        ret

;Set (ecx = 1) or clear (0) the "loading" flag.
PEBusy:
        push fs
        call PELoadFS
        mov [fs:OffPEBusy], ecx
        pop fs
        ret

;Far call from the loader: EAX = new ESP, EDX = application stack
;selector, ESI = flat code selector. Loads and binds the import
;graph, then runs the DllMain of every DLL (dependencies first).
;Out: CF clear, or CF set and AL = the PEGraph code.
        DPROC PEBind
        push ds
        push es
        push fs
        pushad
        mov ebx, ss
        mov ecx, esp
        push edx
        push eax
        lss esp, [esp]
        push ebx
        push ecx
        sub esp, FSize
        mov ebp, esp
        call PELoadES
        push es
        pop ds
        cld
        mov [ebp + FCs], esi
        mov byte [ebp + FUser], 0
        mov dword [ebp + FRsv], 0
        mov dword [ebp + FStart], 0
        mov dword [ebp + FTarget], 0
        mov byte [ebp + FSize + 28], '$'
        push fs
        call PELoadFS
        mov dword [fs:OffPEHostLack], 0
        pop fs
        mov ecx, 1
        call PEBusy
        call PERegMark
        mov [ebp + FMark], ecx
        call PEGraph
        pushfd
        push eax
        jnc .noname
        cmp eax, 6
        jne .nl
        push fs
        call PELoadFS
        cmp dword [fs:OffPEHostLack], 0
        pop fs
        je .nl
        mov dword [esp], 8              ;host lacks a function
        mov eax, 8
.nl:
        mov ecx, 0AEh                   ;codes 1 2 3 5 7 name a DLL
        bt ecx, eax
        jnc .noname
        lea esi, [ebp + FNames]
        lea edi, [ebp + FSize + 28]
        mov ax, ': '
        stosw
.cp:    lodsb
        stosb
        test al, al
        jnz .cp
        mov byte [edi - 1], '$'
.noname:
        xor ecx, ecx
        call PEBusy
        pop eax
        popfd
        lea esp, [ebp + FSize]
        pop ecx
        pop ebx
        push ebx
        push ecx
        lss esp, [esp]
        mov [esp + 28], eax
        popad
        pop fs
        pop es
        pop ds
        retf

;LoadLibrary: edx = client path (ds). Out: CF clear and eax =
;handle, or CF set and eax = ABI error. The frame is on the stack
;of the caller; [esp + 44] is its flat code selector.
PELoadLib:
        push fs
        call PELoadFS
        mov eax, [fs:OffPEBusy]
        pop fs
        or eax, eax
        jz .go
        mov eax, 11
        stc
        ret
.go:    push fs
        call PELoadFS
        mov dword [fs:OffPEHostLack], 0
        pop fs
        mov ecx, 1
        call PEBusy
        sub esp, FSize
        mov ebp, esp
        mov eax, [esp + FSize + 4 + 4 + 32 + 4 + 4]
        mov [ebp + FCs], eax
        mov dword [ebp + FRsv], 0
        xor ecx, ecx
.cp:    mov al, [ds:edx + ecx]
        mov [ebp + FUser + ecx], al
        inc ecx
        test al, al
        jz .copied
        cmp ecx, 260
        jb .cp
        mov eax, 2
        jmp .err
.copied:
        mov esi, edx
        lea edi, [ebp + FNames]
        call PENormName
        jc .badname
        call PERegFind
        jc .load
        inc dword [es:esi + RecRef]
        mov eax, esi
        jmp .ok
.badname:
        mov eax, 2
        jmp .err
.load:  lea edi, [ebp + FDir]
        call PEEnvDir
        call PERegMark
        mov [ebp + FMark], ecx
        mov [ebp + FStart], ecx
        mov [ebp + FTarget], ecx
        lea edi, [ebp + FNames]
        call PELoadDll
        mov byte [ebp + FUser], 0
        jnc .graph
        jmp .map
.graph: call PEGraph
        jnc .got
.map:   cmp eax, 1
        jne .m2
        mov eax, 2
        jmp .back
.m2:    cmp eax, 3
        jne .m3
        mov eax, 12
        jmp .back
.m3:    cmp eax, 5
        jne .m4
        mov eax, 9
        jmp .back
.m4:    cmp eax, 6
        jne .m5
        mov eax, 4
        push fs
        call PELoadFS
        cmp dword [fs:OffPEHostLack], 0
        pop fs
        je .back
        mov eax, 14
        jmp .back
.m5:    cmp eax, 7
        jne .m6
        mov eax, 13
        jmp .back
.m6:    mov eax, 3
.back:  push eax
        mov ecx, [ebp + FMark]
        call PERegRollback
        pop eax
.err:   push eax
        xor ecx, ecx
        call PEBusy
        pop eax
        lea esp, [ebp + FSize]
        stc
        ret
.got:   mov ecx, [ebp + FMark]
        call PERegNth
        mov dword [es:edx + RecRef], 1
        mov eax, edx
.ok:    push eax
        xor ecx, ecx
        call PEBusy
        pop eax
        lea esp, [ebp + FSize]
        clc
        ret

;edx = record: mark it and everything it imports (RfMark).
;CF set if the import chain is too deep.
PEMarkRec:
        test dword [es:edx + RecFlags], RfMark
        jnz .skip
        or dword [es:edx + RecFlags], RfMark
        inc dword [ebp + FDepth]
        cmp dword [ebp + FDepth], 200
        ja .deep
        push edx
        call PEImpInit
        jz .self
        push dword [ebp + FDesc]
        push dword [ebp + FCnt]
.d:     cmp dword [esp], 0
        je .done
        mov ecx, [esp + 4]
        mov eax, [es:ecx + 12]
        or eax, eax
        jz .done
        mov edx, [esp + 8]
        mov [ebp + FRec], edx
        call PEDescName
        jc .next
        lea edi, [ebp + FNames]
        call PERegFind
        jc .next
        mov edx, esi
        call PEMarkRec
        jc .fail
.next:  add dword [esp + 4], 20
        dec dword [esp]
        jmp .d
.done:  add esp, 8
.self:  push dword 0
.f:     mov eax, [esp]
        mov [ebp + FFi], eax
        mov edx, [esp + 4]
        mov [ebp + FRec], edx
        call PEFwdNext
        jc .fdone
        mov eax, [ebp + FFi]
        mov [esp], eax
        lea edi, [ebp + FNames]
        call PERegFind
        jc .f
        mov edx, esi
        call PEMarkRec
        jc .ffail
        jmp .f
.fdone: add esp, 4
        pop edx
        dec dword [ebp + FDepth]
.skip:  clc
        ret
.ffail: lea esp, [esp + 8]
        dec dword [ebp + FDepth]
        stc
        ret
.fail:  lea esp, [esp + 12]
        dec dword [ebp + FDepth]
        stc
        ret
.deep:  dec dword [ebp + FDepth]
        stc
        ret

;edx = record: free its image and clear it.
PEFreeRec:
        mov esi, [es:edx + RecHandle]
        or esi, esi
        jz .z
        push edx
        mov ax, 502h
        call PEDpmi
        pop edx
.z:     mov edi, edx
        mov ecx, RecSz / 4
        xor eax, eax
        cld
        rep stosd
        ret

;Drop empty records at the end of the registry and the empty
;chunks behind the first one.
PERegTrim:
.again: push fs
        call PELoadFS
        mov esi, [fs:OffPEModHead]
        pop fs
        xor edi, edi
.walk:  mov eax, [es:esi]
        or eax, eax
        jz .last
        mov edi, esi
        mov esi, eax
        jmp .walk
.last:  mov ecx, [es:esi + 4]
        jecxz .empty
        dec ecx
        imul edx, ecx, RecSz
        lea edx, [esi + edx + 16]
        cmp dword [es:edx + RecFlags], 0
        jne .ret
        mov [es:esi + 4], ecx
        jmp .last
.empty: or edi, edi
        jz .ret
        mov dword [es:edi], 0
        push edi
        mov esi, [es:esi + 8]
        mov ax, 502h
        call PEDpmi
        pop edi
        jmp .again
.ret:   ret

;Detach (reason 0, reserved FRsv) every unmarked DLL, newest first,
;then free their images. FSeq is the next sequence number.
PEUnloadUnmarked:
        mov eax, [ebp + FSeq]
        mov [ebp + FCur], eax
.s:     dec dword [ebp + FCur]
        jz .free
        mov dword [ebp + FIdx], 0
.r:     mov ecx, [ebp + FIdx]
        call PERegNth
        jc .s
        inc dword [ebp + FIdx]
        mov eax, [ebp + FCur]
        cmp [es:edx + RecSeq], eax
        jne .r
        cmp dword [es:edx + RecState], 2
        jne .r
        test dword [es:edx + RecFlags], RfMark
        jnz .r
        cmp dword [es:edx + RecKind], 0
        je .r
        mov [ebp + FRec2], edx
        xor ecx, ecx
        call PEDllMain
        mov edx, [ebp + FRec2]
        mov dword [es:edx + RecState], 3
        jmp .r
.free:  mov dword [ebp + FIdx], 0
.f:     mov ecx, [ebp + FIdx]
        call PERegNth
        jc .done
        inc dword [ebp + FIdx]
        test dword [es:edx + RecFlags], RfUsed
        jz .f
        test dword [es:edx + RecFlags], RfMark
        jnz .f
        cmp dword [es:edx + RecKind], 0
        je .f
        call PEFreeRec
        jmp .f
.done:  call PERegTrim
        ret

;Unload every DLL that cannot be reached from a root (the main
;module and modules with explicit handles; with FRsv = 1, at exit,
;there are no roots). Out: eax = 0, or CF set and eax = 6.
PESweep:
        call PETrampOn
        jc .ret
        push fs
        call PELoadFS
        mov eax, [fs:OffPESeq]
        pop fs
        mov [ebp + FSeq], eax
        mov dword [ebp + FDepth], 0
        mov dword [ebp + FIdx], 0
.c:     mov ecx, [ebp + FIdx]
        call PERegNth
        jc .roots
        and dword [es:edx + RecFlags], ~RfMark
        inc dword [ebp + FIdx]
        jmp .c
.roots: cmp dword [ebp + FRsv], 0
        jne .un
        mov dword [ebp + FIdx], 0
.r:     mov ecx, [ebp + FIdx]
        call PERegNth
        jc .un
        inc dword [ebp + FIdx]
        mov eax, [es:edx + RecFlags]
        test eax, RfUsed
        jz .r
        test eax, RfPinned
        jnz .mk
        cmp dword [es:edx + RecRef], 0
        je .r
.mk:    call PEMarkRec
        jnc .r
        mov eax, 6
        stc
        jmp .off
.un:    call PEUnloadUnmarked
        xor eax, eax
.off:   pushfd
        push eax
        call PETrampOff
        pop eax
        popfd
.ret:   ret

;FreeLibrary: esi = handle. CF clear and eax = 0, or CF set and
;eax = 5 bad handle, 8 main module, 11 busy, 6 too deep.
PEFreeLib:
        push fs
        call PELoadFS
        mov eax, [fs:OffPEBusy]
        pop fs
        or eax, eax
        jz .go
        mov eax, 11
        stc
        ret
.go:    call PERegCheckH
        jc .bad
        test dword [es:esi + RecFlags], RfPinned
        jnz .main
        cmp dword [es:esi + RecRef], 0
        je .bad
        dec dword [es:esi + RecRef]
        mov ecx, 1
        call PEBusy
        sub esp, FSize
        mov ebp, esp
        mov eax, [esp + FSize + 4 + 4 + 32 + 4 + 4]
        mov [ebp + FCs], eax
        mov dword [ebp + FRsv], 0
        call PESweep
        pushfd
        push eax
        xor ecx, ecx
        call PEBusy
        pop eax
        popfd
        lea esp, [ebp + FSize]
        ret
.bad:   mov eax, 5
        stc
        ret
.main:  mov eax, 8
        stc
        ret

;Detach and free every DLL (DllMain reason 0, reserved 1).
;eax = flat code selector of the program, ds = es = flat.
PEExitSweep:
        sub esp, FSize
        mov ebp, esp
        mov [ebp + FCs], eax
        mov dword [ebp + FRsv], 1
        mov ecx, 1
        call PEBusy
        call PESweep
        lea esp, [ebp + FSize]
        ret

;Far call from the exit stub on the application stack: detach and
;free every DLL, then return.
        DPROC PEExit
        pushad
        push ds
        push es
        cld
        call PELoadES
        push es
        pop ds
        mov eax, [esp + 4 + 4 + 32 + 4]
        call PEExitSweep
        pop es
        pop ds
        popad
        retf
