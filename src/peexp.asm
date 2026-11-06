;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; PE export lookup by name or ordinal (PEMAX only). Names are
; case sensitive. The export arrays are checked on every lookup.
; Errors (EAX, CF set): 3 bad image, 5 bad handle, 6 symbol not
; found, 10 forwarder loop. See docs/PEABI.MD.

;es flat, ecx = image size, edi = export directory (flat).
;Checks that array [dir + %2] of dir[%1] elements of 1 << %3
;bytes lies inside the image. Jumps to .bad, clobbers eax, ebp.
%macro PEARR 3
        mov eax, [es:edi + %1]
        mov ebp, ecx
        shr ebp, 2
        cmp eax, ebp
        ja .bad
        shl eax, %3
        add eax, [es:edi + %2]
        jc .bad
        cmp eax, ecx
        ja .bad
%endmacro

;ds:edx client name, es:eax image string, ecx = bytes left in
;the image; ZF set if equal. Preserves all registers.
PEStrEq:
        push ebx
        push ecx
        push edx
        push eax
.l:     or ecx, ecx
        jz .no
        mov bl, [ds:edx]
        cmp bl, [es:eax]
        jne .no
        or bl, bl
        jz .yes
        inc edx
        inc eax
        dec ecx
        jmp .l
.no:    or esp, esp
        jmp .out
.yes:   xor ebx, ebx
.out:   pop eax
        pop edx
        pop ecx
        pop ebx
        ret

;es flat, esi = handle. CF set unless it is a record address.
;Clobbers eax, ebx, ecx.
PERegCheckH:
        push fs
        call PELoadFS
        mov ebx, [fs:OffPEModHead]
        pop fs
.c:     or ebx, ebx
        jz .no
        mov ecx, [es:ebx + 4]
        imul ecx, ecx, RecSz
        mov eax, esi
        sub eax, ebx
        jb .nx
        sub eax, 16
        jb .nx
        cmp eax, ecx
        jae .nx
        test al, RecSz - 1
        jnz .nx
        test dword [es:esi + RecFlags], RfUsed
        jz .no
        clc
        ret
.nx:    mov ebx, [es:ebx]
        jmp .c
.no:    stc
        ret

;es flat, esi = record, edx = ordinal (< 10000h) or ds:edx name.
;Out: CF clear and eax = address, or CF set and eax = error.
;Clobbers all but esi, edx, ds, es.
PEProcLookup:
        mov ebx, [es:esi + RecBase]
        mov ecx, [es:esi + RecImgSize]
        mov edi, [es:esi + RecExpRVA]
        or edi, edi
        jz .nosym
        cmp dword [es:esi + RecExpSize], 40
        jb .bad
        add edi, ebx
        PEARR 20, 28, 2
        PEARR 24, 32, 2
        PEARR 24, 36, 1
        cmp edx, 10000h
        jae .byname
        mov eax, edx
        sub eax, [es:edi + 16]
        jb .nosym
        cmp eax, [es:edi + 20]
        jae .nosym
        jmp .entry
.byname:
        xor eax, eax
.nl:    cmp eax, [es:edi + 24]
        jae .nosym
        mov ebp, [es:edi + 32]
        add ebp, ebx
        mov ecx, [es:ebp + eax*4]
        push eax
        mov eax, ecx
        mov ecx, [es:esi + RecImgSize]
        sub ecx, eax
        jbe .badp
        add eax, ebx
        call PEStrEq
        pop eax
        je .found
        inc eax
        jmp .nl
.badp:  pop eax
        jmp .bad
.found: mov ebp, [es:edi + 36]
        add ebp, ebx
        movzx eax, word [es:ebp + eax*2]
        cmp eax, [es:edi + 20]
        jae .bad
.entry: mov ebp, [es:edi + 28]
        add ebp, ebx
        mov eax, [es:ebp + eax*4]
        or eax, eax
        jz .nosym
        mov ecx, [es:esi + RecExpRVA]
        cmp eax, ecx
        jb .inside
        add ecx, [es:esi + RecExpSize]
        cmp eax, ecx
        jb .fwd
.inside:
        cmp eax, [es:esi + RecImgSize]
        jae .bad
        add eax, ebx
        clc
        ret
.fwd:   mov ecx, [es:esi + RecImgSize]
        sub ecx, eax
        jbe .bad
        add eax, ebx
        jmp PEFwd
.nosym: mov eax, 6
        stc
        ret
.bad:   mov eax, 3
        stc
        ret

;es:eax forwarder text "MODULE.name" or "MODULE.#n", ecx = bytes
;left, ss:edi buffer (NameMax). Writes MODULE.DLL (upper case) and
;leaves eax and ecx after the dot. CF set if malformed.
PEFwdModName:
        xor ebx, ebx
.m:     jecxz .bad
        mov dl, [es:eax]
        inc eax
        dec ecx
        cmp dl, '.'
        je .dot
        test dl, dl
        jz .bad
        cmp ebx, NameMax - 5
        jae .bad
        cmp dl, 'a'
        jb .st
        cmp dl, 'z'
        ja .st
        sub dl, 20h
.st:    mov [ss:edi + ebx], dl
        inc ebx
        jmp .m
.dot:   or ebx, ebx
        jz .bad
        mov dword [ss:edi + ebx], '.DLL'
        mov byte [ss:edi + ebx + 4], 0
        clc
        ret
.bad:   stc
        ret

;Resolve a forwarder: es:eax = text, ecx = bytes left (see above).
;The target module must be loaded. Chains are limited to 8 steps.
;Out as PEProcLookup (3 bad text, 6 not found, 10 loop).
PEFwd:
        sub esp, NameMax
        mov edi, esp
        call PEFwdModName
        jc .bad
        push eax
        push ecx
.z:     cmp byte [es:eax], 0
        je .zo
        inc eax
        dec ecx
        jnz .z
        add esp, 8
        jmp .bad
.zo:    pop ecx
        pop eax
        push eax
        call PERegFind
        pop eax
        jc .nosym
        push fs
        call PELoadFS
        inc dword [fs:OffPEFwd]
        mov ebx, [fs:OffPEFwd]
        pop fs
        cmp ebx, 8
        ja .loop
        cmp byte [es:eax], '#'
        je .ord
        mov edx, eax
        jmp .call
.ord:   inc eax
        xor edx, edx
.d:     movzx ebx, byte [es:eax]
        test ebx, ebx
        jz .call
        sub ebx, '0'
        cmp ebx, 9
        ja .bad2
        imul edx, edx, 10
        add edx, ebx
        cmp edx, 0FFFFh
        ja .bad2
        inc eax
        jmp .d
.call:  call PEProcLookup
        pushfd
        push fs
        call PELoadFS
        dec dword [fs:OffPEFwd]
        pop fs
        popfd
        jmp .out
.loop:  push fs
        call PELoadFS
        dec dword [fs:OffPEFwd]
        pop fs
        mov eax, 10
        jmp .err
.nosym: mov eax, 6
        jmp .err
.bad2:  push fs
        call PELoadFS
        dec dword [fs:OffPEFwd]
        pop fs
.bad:   mov eax, 3
.err:   stc
.out:   lea esp, [esp + NameMax]
        ret
