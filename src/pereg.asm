;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; PE module registry and the ring 3 vendor entry (PEMAX only).
; Records live in chunks of 8192 bytes (DPMI blocks): a 16-byte
; header (next, count, handle) and 63 records of 128 bytes.
; Only the chunk list head sits in EData. Names are upper case
; base names, at most 31 characters. See docs/PEABI.MD.

RegChunk equ 8192
RegPerChunk equ 63
RecSz equ 128
RecFlags equ 0
RecRef equ 4
RecHandle equ 8
RecBase equ 12
RecImgSize equ 16
RecEntry equ 20
RecExpRVA equ 24
RecExpSize equ 28
RecKind equ 32
RecState equ 36
RecSeq equ 112
RecTlsRVA equ 116
RecTlsIdx equ 120
RecImpRVA equ 104
RecImpSize equ 108
RecName1 equ 40
RecName2 equ 72
NameMax equ 32
RfUsed equ 1
RfPinned equ 2
RfMain equ 4
RfMark equ 8

;Selector loads use immediates patched by realinit: the QEMU TCG
;ignores the CS base of "cs:" data reads when DS, ES and SS are
;flat.
PELoadFS:
        push 0FFFFh
        LLabel PESelD
        pop fs
        ret
PELoadES:
        push 0FFFFh
        LLabel PESelE
        pop es
        ret

        SEGM EData
        LDWord PEModHead
        dd 0
        LDWord PELastErr
        dd 0
        LDWord PEBusy
        dd 0
        LDWord PESeq
        dd 1
        LDWord PEFwd
        dd 0
        LDWord PEHostLack
        dd 0
        LDWord PEEntryFS
        dd 0
        LDWord PETlsTab
        dd 0
        LDWord PETlsTabH
        dd 0
%ifdef FAKELACK
        LDWord PEFakeCnt
        dd 0
%endif
        LDWord PEProgPath
        times 128 db 0
        ESEG EData

;ds:esi ASCIIZ path -> ss:edi upper case base name (NameMax),
;".DLL" added if it has no extension. CF set if empty or too
;long. Clobbers eax, ebx, ecx, esi.
PENormName:
        mov ebx, esi
.scan:  lodsb
        test al, al
        jz .copy
        cmp al, '\'
        je .sep
        cmp al, '/'
        je .sep
        cmp al, ':'
        jne .scan
.sep:   mov ebx, esi
        jmp .scan
.copy:  mov esi, ebx
        xor ecx, ecx                    ;api-ms-win-crt-* is
.az:    cmp byte [ds:esi + ecx], 0      ;UCRTBASE.DLL (task 200)
        je .noapi
        inc ecx
        cmp ecx, 15
        jb .az
        mov eax, [ds:esi]
        or eax, 20202020h
        cmp eax, 'api-'
        jne .noapi
        mov eax, [ds:esi + 4]
        or eax, 20202020h
        cmp eax, 'ms-w'
        jne .noapi
        mov eax, [ds:esi + 8]
        or eax, 20202020h
        cmp eax, 'in-c'
        jne .noapi
        mov eax, [ds:esi + 11]
        or eax, 20202020h
        cmp eax, 'crt-'
        jne .noapi
        mov dword [ss:edi], 'UCRT'
        mov dword [ss:edi + 4], 'BASE'
        mov dword [ss:edi + 8], '.DLL'
        mov byte [ss:edi + 12], 0
        clc
        ret
.noapi: xor ecx, ecx
.cp:    lodsb
        test al, al
        jz .end
        cmp ecx, NameMax - 1
        jae .bad
        cmp al, 'a'
        jb .store
        cmp al, 'z'
        ja .store
        sub al, 20h
.store: mov [ss:edi + ecx], al
        inc ecx
        jmp .cp
.end:   or ecx, ecx
        jz .bad
        xor edx, edx
.dot:   cmp edx, ecx
        jae .noext
        cmp byte [ss:edi + edx], '.'
        je .done
        inc edx
        jmp .dot
.noext: cmp ecx, NameMax - 5            ;no ext: add ".DLL"
        ja .bad                         ;(Win32 loader)
        mov dword [ss:edi + ecx], '.DLL'
        add ecx, 4
.done:  mov byte [ss:edi + ecx], 0
        clc
        ret
.bad:   mov byte [ss:edi], 0
        stc
        ret

;es:ebx alias against ss:edi name; ZF set if equal.
PENameEq:
        push ecx
        xor ecx, ecx
.l:     mov al, [es:ebx + ecx]
        cmp al, [ss:edi + ecx]
        jne .out
        inc ecx
        or al, al
        jnz .l
.out:   pop ecx
        ret

;ss:edi name, es flat. Out: CF clear and esi = record.
;Clobbers eax, ebx, ecx, edx.
PERegFind:
        push fs
        call PELoadFS
        mov esi, [fs:OffPEModHead]
        pop fs
.chunk: or esi, esi
        jz .none
        mov ecx, [es:esi + 4]
        lea edx, [esi + 16]
.rec:   jecxz .next
        lea ebx, [edx + RecName1]
        call PENameEq
        je .hit
        lea ebx, [edx + RecName2]
        call PENameEq
        je .hit
        add edx, RecSz
        dec ecx
        jmp .rec
.next:  mov esi, [es:esi]
        jmp .chunk
.hit:   mov esi, edx
        clc
        ret
.none:  stc
        ret

;es flat. Out: CF clear and edx = new zeroed record (used);
;CF set and eax = 4 if no memory. Clobbers eax, ebx, ecx, esi,
;edi.
PERegAdd:
        push fs
        call PELoadFS
        mov esi, [fs:OffPEModHead]
        pop fs
        xor edx, edx
.walk:  or esi, esi
        jz .tail
        mov edx, esi
        mov esi, [es:esi]
        jmp .walk
.tail:  or edx, edx
        jz .new
        cmp dword [es:edx + 4], RegPerChunk
        jb .have
.new:   push edx
        mov ebx, RegChunk
        mov ax, 501h
        call PEDpmi
        pop edx
        jc .nomem
        mov edi, ebx
        mov ecx, RegChunk / 4
        xor eax, eax
        cld
        rep stosd
        mov [es:ebx + 8], esi
        or edx, edx
        jz .first
        mov [es:edx], ebx
        jmp .link
.first: push fs
        call PELoadFS
        mov [fs:OffPEModHead], ebx
        pop fs
.link:  mov edx, ebx
.have:  mov ecx, [es:edx + 4]
        inc dword [es:edx + 4]
        imul ecx, ecx, RecSz
        lea edx, [edx + ecx + 16]
        mov dword [es:edx + RecFlags], RfUsed
        clc
        ret
.nomem: mov eax, 4
        stc
        ret

;es flat. Out: ecx = number of records (transaction mark).
PERegMark:
        push fs
        call PELoadFS
        mov esi, [fs:OffPEModHead]
        pop fs
        xor ecx, ecx
.l:     or esi, esi
        jz .done
        add ecx, [es:esi + 4]
        mov esi, [es:esi]
        jmp .l
.done:  ret

;es flat, ecx = mark. Frees the images of the records added
;after the mark and forgets them (chunks are kept for reuse).
PERegRollback:
        push ebp
        mov ebp, ecx
        push fs
        call PELoadFS
        mov esi, [fs:OffPEModHead]
        pop fs
.chunk: or esi, esi
        jz .done
        mov edx, [es:esi + 4]
        cmp ebp, edx
        jae .whole
        mov edi, ebp
        mov [es:esi + 4], ebp
        xor ebp, ebp
.free:  cmp edi, edx
        jae .next
        imul ebx, edi, RecSz
        lea ebx, [esi + ebx + 16]
        push esi
        push edx
        push edi
        mov esi, [es:ebx + RecHandle]
        or esi, esi
        jz .z
        push ebx
        mov ax, 502h
        call PEDpmi
        pop ebx
.z:     mov edi, ebx
        mov ecx, RecSz / 4
        xor eax, eax
        cld
        rep stosd
        pop edi
        pop edx
        pop esi
        inc edi
        jmp .free
.whole: sub ebp, edx
.next:  mov esi, [es:esi]
        jmp .chunk
.done:  pop ebp
        ret

;Out: ds:esi = path of the program, CF set if unavailable. The
;path is the environment path of the stub file, or, when the
;environment holds ZRDXROLE (unbound run, task 170) or the
;command tail
;starts with the marker /#R (task 172), the first token after it,
;kept in PEProgPath and cut off the
;tail. Clobbers eax, ebx, ecx, edx (edi is kept). The caller
;saves
;ds.
PEEnvPtr:
        push fs
        call PELoadFS
        cmp byte [fs:OffPEProgPath], 0
        jne .have
        mov ah, 62h
        int 21h
        call PELoadFS
        push ebx
        mov ds, bx
        movzx eax, word [ds:2Ch]
        mov ds, ax
        lsl ecx, eax
        jnz .no
        xor esi, esi
.rs:    lea eax, [esi + 9]
        cmp eax, ecx
        jae .plain
        cmp byte [ds:esi], 0
        je .plain
        cmp dword [ds:esi], 'ZRDX'
        jne .nx
        cmp dword [ds:esi + 4], 'ROLE'
        jne .nx
        cmp byte [ds:esi + 8], '='
        je .role
.nx:    inc esi
        cmp byte [ds:esi - 1], 0
        jne .nx
        jmp .rs
.plain: mov ebx, [esp]
        push ds
        mov ds, bx
        mov esi, 81h
.pt:    cmp byte [ds:esi], ' '
        jne .pm
        inc esi
        jmp .pt
.pm:    cmp word [ds:esi], '/#'
        jne .pn
        cmp byte [ds:esi + 2], 'R'
        jne .pn
        cmp byte [ds:esi + 3], ' '
        jne .pn
        pop ds
        jmp .role
.pn:    pop ds
        xor esi, esi
.find:  lea eax, [esi + 4]
        cmp eax, ecx
        jae .no
        cmp word [ds:esi], 0
        je .path
        inc esi
        jmp .find
.path:  add esi, 4
        pop ebx
        pop fs
        clc
        ret
.role:  pop ebx
        mov ds, bx
        mov esi, 81h
.sp:    cmp byte [ds:esi], ' '
        jne .mk
        inc esi
        jmp .sp
.mk:    cmp word [ds:esi], '/#'
        jne .tok
        cmp byte [ds:esi + 2], 'R'
        jne .tok
        cmp byte [ds:esi + 3], ' '
        jne .tok
        add esi, 3
        jmp .sp
.tok:   xor ecx, ecx
.cp:    mov al, [ds:esi + ecx]
        cmp al, 21h
        jb .end
        cmp ecx, 126
        jae .end
        mov [fs:OffPEProgPath + ecx], al
        inc ecx
        jmp .cp
.end:   mov byte [fs:OffPEProgPath + ecx], 0
        or ecx, ecx
        jz .no2
        lea edx, [esi + ecx]
.sk:    cmp byte [ds:edx], ' '
        jne .mv0
        inc edx
        jmp .sk
.mv0:   mov esi, edx
        mov edx, 81h
.mv:    mov al, [ds:esi]
        mov [ds:edx], al
        inc esi
        inc edx
        cmp al, 0Dh
        je .mvd
        cmp edx, 0FFh
        jb .mv
        mov byte [ds:edx - 1], 0Dh
.mvd:   lea eax, [edx - 82h]
        mov [ds:80h], al
.have:  push fs
        pop ds
        mov esi, OffPEProgPath
        pop fs
        clc
        ret
.no2:   mov byte [fs:OffPEProgPath], 0
        pop fs
        stc
        ret
.no:    pop ebx
        pop fs
        stc
        ret

;ss:edi buffer (NameMax) = base name of the program path
;taken from the environment; empty if unavailable.
PEEnvName:
        mov byte [ss:edi], 0
        push ds
        call PEEnvPtr
        jc .out
        call PENormName
.out:   pop ds
        ret

;ss:edi buffer (NameMax) = base name from the export directory
;of the PELoadCtx at EBP; empty if none. es flat.
PEExpName:
        mov byte [ss:edi], 0
        cmp dword [ebp + PENumDirs], 1
        jb .out
        mov eax, [ebp + PEDirExport]
        or eax, eax
        jz .out
        add eax, [ebp + CtxBase]
        mov esi, [es:eax + 12]
        mov ecx, [ebp + PEImageSize]
        sub ecx, esi                    ;bytes to the end
        jbe .out
        cmp ecx, NameMax
        jbe .lim
        mov ecx, NameMax
.lim:   add esi, [ebp + CtxBase]
        xor ebx, ebx
.z:     cmp byte [es:esi + ebx], 0      ;the name must end inside
        je .ended
        inc ebx
        cmp ebx, ecx
        jb .z
        jmp .out
.ended: push ds
        push es
        pop ds
        call PENormName
        pop ds
.out:   ret

;EBP = PELoadCtx, es flat, ss:esi = two names (NameMax each).
;Registers the module. Out: CF clear and edx = record, or CF
;set and eax = 9 name collision / 4 no memory.
PERegFill:
        push esi
        mov edi, esi
        cmp byte [ss:edi], 0
        je .n2
        call PERegFind
        jnc .coll
.n2:    mov esi, [esp]
        lea edi, [esi + NameMax]
        cmp byte [ss:edi], 0
        je .add
        call PERegFind
        jnc .coll
.add:   call PERegAdd
        pop esi
        jc .ret
        mov eax, [ebp + CtxHandle]
        mov [es:edx + RecHandle], eax
        mov eax, [ebp + CtxBase]
        mov [es:edx + RecBase], eax
        mov eax, [ebp + CtxSize]
        mov [es:edx + RecImgSize], eax
        mov eax, [ebp + CtxEntry]
        mov [es:edx + RecEntry], eax
        mov eax, [ebp + CtxKind]
        mov [es:edx + RecKind], eax
        mov dword [es:edx + RecRef], 0
        cmp dword [ebp + PENumDirs], 1
        jb .imp
        mov eax, [ebp + PEDirExport]
        mov [es:edx + RecExpRVA], eax
        mov eax, [ebp + PEDirExportSize]
        mov [es:edx + RecExpSize], eax
.imp:   cmp dword [ebp + PENumDirs], 2
        jb .copy
        mov eax, [ebp + PEDirImport]
        mov [es:edx + RecImpRVA], eax
        mov eax, [ebp + PEDirImportSize]
        mov [es:edx + RecImpSize], eax
.copy:  xor ecx, ecx
.cn:    mov al, [ss:esi + ecx]
        mov [es:edx + RecName1 + ecx], al
        mov al, [ss:esi + NameMax + ecx]
        mov [es:edx + RecName2 + ecx], al
        inc ecx
        cmp ecx, NameMax
        jb .cn
        call PETlsSetup
.ret:   ret
.coll:  pop esi
        mov eax, 9
        stc
        ret

;The TLS table (task 208, 209): a block of 8192 bytes, 1024 block
;pointers (the array behind FS:[2Ch]) and the 1024 DPMI handles
;of the blocks. Out: CF clear and esi = table, or CF set (no
;memory).
PETlsTable:
        push fs
        call PELoadFS
        mov esi, [fs:OffPETlsTab]
        pop fs
        or esi, esi
        jnz .ok
        mov ebx, 8192
        mov ax, 501h
        call PEDpmi
        jc .ret
        push fs
        call PELoadFS
        mov [fs:OffPETlsTab], ebx
        mov [fs:OffPETlsTabH], esi
        pop fs
        mov edi, ebx
        mov ecx, 8192 / 4
        xor eax, eax
        cld
        rep stosd
        mov esi, ebx
.ok:    clc
.ret:   ret

;edx = record, ebp = PELoadCtx (es flat). A module with a TLS
;directory gets a block (raw data and zero fill), a free index,
;written to the directory's index variable, and a table slot.
;Out: CF clear, or CF set and eax = 4 no memory / 10 bad
;directory (the record is cleared; the caller frees the image).
PETlsSetup:
        push edx                        ;rec
        cmp dword [ebp + PENumDirs], 10
        jb .none
        mov eax, [ebp + PEDirTLS]
        or eax, eax
        jz .none
        mov [es:edx + RecTlsRVA], eax
        cmp dword [ebp + PEDirTLSSize], 24
        jb .bad
        add eax, 24
        jc .bad
        cmp eax, [ebp + PEImageSize]
        ja .bad
        mov ebx, [ebp + CtxBase]
        mov esi, [ebp + PEDirTLS]
        add esi, ebx                    ;esi = directory
        mov ecx, [es:esi + 4]
        sub ecx, [es:esi]               ;ecx = raw data size
        jb .bad
        cmp ecx, 100000h
        ja .bad
        mov eax, [es:esi]
        sub eax, ebx
        jb .bad
        add eax, ecx
        jc .bad
        cmp eax, [ebp + PEImageSize]
        ja .bad
        mov eax, [es:esi + 8]           ;the index variable
        sub eax, ebx
        jb .bad
        add eax, 4
        cmp eax, [ebp + PEImageSize]
        ja .bad
        mov eax, [es:esi + 12]          ;callbacks array, if any
        or eax, eax
        jz .cbok
        sub eax, ebx
        jb .bad
        add eax, 4
        cmp eax, [ebp + PEImageSize]
        ja .bad
.cbok:  mov eax, [es:esi + 16]          ;zero fill
        cmp eax, 100000h
        ja .bad
        add eax, ecx
        add eax, 0FFFh
        and eax, ~0FFFh                 ;block size, whole pages
        jnz .size
        mov eax, 1000h
.size:  push eax                        ;size
        push esi                        ;dir
        push ecx                        ;raw: [esp] = raw,
        call PETlsTable                 ;+4 dir, +8 size, +12 rec
        jc .nomem
        xor ebx, ebx
.slot:  cmp dword [es:esi + ebx * 4], 0
        je .got
        inc ebx
        cmp ebx, 1024
        jb .slot
        jmp .nomem
.got:   push ebx                        ;slot
        push esi                        ;table
        mov ebx, [esp + 16]
        mov ax, 501h
        call PEDpmi                     ;ebx = block
        pop edi
        pop eax
        jc .nomem
        mov [es:edi + eax * 4], ebx
        mov [es:edi + eax * 4 + 4096], esi
        push eax                        ;slot
        mov edi, ebx
        mov ecx, [esp + 12]
        shr ecx, 2
        xor eax, eax
        cld
.zero:  mov [es:edi], eax
        add edi, 4
        loop .zero
        mov esi, [esp + 8]
        mov esi, [es:esi]               ;raw data (start address)
        mov edi, ebx
        mov ecx, [esp + 4]
.cp:    jecxz .cpd
        mov al, [es:esi]
        mov [es:edi], al
        inc esi
        inc edi
        dec ecx
        jmp .cp
.cpd:   pop eax                         ;slot
        mov esi, [esp + 4]
        mov esi, [es:esi + 8]
        mov [es:esi], eax               ;the TLS index
        mov edx, [esp + 12]
        inc eax
        mov [es:edx + RecTlsIdx], eax
        add esp, 12
.none:  pop edx
        clc
        ret
.nomem: add esp, 12
        mov eax, 4
        jmp .fail
.bad:   mov eax, 10
.fail:  pop edx
        push eax
        mov edi, edx
        mov ecx, RecSz / 4
        xor eax, eax
        cld
        rep stosd
        pop eax
        push eax
        call PERegTrim
        pop eax
        stc
        ret

;Free the block of the record in edx (kept) and its table slot.
PETlsRelease:
        mov eax, [es:edx + RecTlsIdx]
        or eax, eax
        jz .ret
        dec eax
        push fs
        call PELoadFS
        mov esi, [fs:OffPETlsTab]
        pop fs
        mov dword [es:esi + eax * 4], 0
        mov esi, [es:esi + eax * 4 + 4096]
        push edx
        mov ax, 502h
        call PEDpmi
        pop edx
.ret:   ret

;Free every block and the table (at exit).
PETlsFreeAll:
        push fs
        call PELoadFS
        mov ebx, [fs:OffPETlsTab]
        pop fs
        or ebx, ebx
        jz .ret
        xor ecx, ecx
.l:     cmp dword [es:ebx + ecx * 4], 0
        je .n
        push ecx
        push ebx
        mov esi, [es:ebx + ecx * 4 + 4096]
        mov ax, 502h
        call PEDpmi
        pop ebx
        pop ecx
.n:     inc ecx
        cmp ecx, 1024
        jb .l
        push fs
        call PELoadFS
        mov esi, [fs:OffPETlsTabH]
        mov dword [fs:OffPETlsTab], 0
        pop fs
        mov ax, 502h
        call PEDpmi
.ret:   ret

;Far call from the loader, EBP = PELoadCtx of the main EXE.
;Registers it (pinned) under its file name and export name.
;CF set if out of memory.
        DPROC PERegMain
        pushad
        push ds
        push es
        call PELoadES
        sub esp, 2 * NameMax
        mov edi, esp
        call PEEnvName
        lea edi, [esp + NameMax]
        call PEExpName
        mov esi, esp
        call PERegFill
        jc .err
        or dword [es:edx + RecFlags], RfPinned + RfMain
        mov dword [es:edx + RecRef], 1
        clc
        jmp .fail
.err:   mov [esp + 2 * NameMax + 8 + 28], eax
.fail:  lea esp, [esp + 2 * NameMax]
        pop es
        pop ds
        popad
        retf

;Ring 3 vendor entry (INT 2Fh 168Ah), far call, AX = function:
;0 GetVersion, 1 LoadLibrary (DS:EDX path), 2 GetModuleHandle
;(DS:EDX name), 3 GetProcAddress (EBX handle, EDX ordinal <
;10000h
;or DS:EDX name), 4 FreeLibrary (EBX), 5 GetLastError.
;6 ProgramInfo (DS:EDX buffer of 128 bytes gets the main program
;path; EAX = its image base, EBX = its module handle), 7 Exit
;(EDX = code: detach all DLLs like the exit stub, end the
;program),
;8 SetEntryFS (EDX = selector the program starts with in FS),
;9 TlsTable (EAX = the array for FS:[2Ch], one block per module).
;Other functions: CF set, EAX = 1. See docs/PEABI.MD.
        DPROC PEVendor
        pushad
        push ds
        push es
        cld
        call PELoadES
        cmp ax, 0
        je .ver
        cmp ax, 2
        je .gmh
        cmp ax, 5
        je .gle
        cmp ax, 3
        je .gpa
        cmp ax, 1
        je .lib
        cmp ax, 4
        je .free
        cmp ax, 6
        je .pinfo
        cmp ax, 7
        je .exit
        cmp ax, 9
        je .tls
        cmp ax, 8
        je .sfs
        mov eax, 1
        jmp .err
.exit:  mov eax, [esp + 8 + 32 + 4]     ;7: detach all and end
        mov ebx, [esp + 8 + 20]         ;the program, code = edx
        push ebx
        push es
        pop ds
        call PEExitSweep
        pop eax
        mov ah, 4Ch
        int 21h
.tls:   call PETlsTable                 ;9: TLS table
        mov eax, esi
        jnc .ok
        mov eax, 4
        jmp .err
.sfs:   mov eax, [esp + 8 + 20]         ;8: FS of the program
        push fs
        call PELoadFS
        mov [fs:OffPEEntryFS], eax
        pop fs
        jmp .ok
.pinfo: mov edi, edx                    ;6: path to ds:edx
        xor ecx, ecx
.pn:    push ecx                        ;PERegNth clobbers ecx
        call PERegNth
        pop ecx
        jc .nomod
        test dword [es:edx + RecFlags], RfMain
        jnz .pm
        inc ecx
        jmp .pn
.pm:    mov eax, [es:edx + RecBase]
        mov [esp + 8 + 16], edx         ;ebx = module handle
        push ds                         ;the path: ds:esi
        push eax
        call PEEnvPtr
        mov ecx, 0
        jc .pe
.pc:    mov bl, [ds:esi + ecx]
        mov [es:edi + ecx], bl
        inc ecx
        test bl, bl
        jz .pd
        cmp ecx, 127
        jb .pc
.pe:    mov byte [es:edi + ecx], 0
.pd:    pop eax
        pop ds
        jmp .ok
.lib:   call PELoadLib
        jc .err
        jmp .ok
.free:  mov esi, ebx
        call PEFreeLib
        jc .err
        jmp .ok
.gpa:   mov esi, ebx
        call PERegCheckH
        jc .nomod
        call PEProcLookup
        jc .err
        jmp .ok
.ver:   mov eax, VendorVersion
        jmp .ok
.gle:   push fs
        call PELoadFS
        mov eax, [fs:OffPELastErr]
        pop fs
        jmp .ok
.gmh:   sub esp, NameMax
        mov esi, edx
        mov edi, esp
        call PENormName
        jc .badnm
        call PERegFind
        lea esp, [esp + NameMax]
        jc .nomod
        mov eax, esi
        jmp .ok
.badnm: lea esp, [esp + NameMax]
        mov eax, 2
        jmp .err
.nomod: mov eax, 5
.err:   push fs
        call PELoadFS
        mov [fs:OffPELastErr], eax
        pop fs
        mov [esp + 8 + 28], eax
        pop es
        pop ds
        popad
        stc
        retf
.ok:    mov [esp + 8 + 28], eax
        pop es
        pop ds
        popad
        clc
        retf
