;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; Optional swap paging for RAW and XMS (docs/SWAPDES.MD).
; Built only with %assign SWAP 1 in segdefs.asm.
;
; PTE bits: 200h = pageable client page, 400h = swapped (P=0,
; bits 31..12 hold the slot). Slot n is the 4 KB at file offset
; n * 4096; SwapIdx is a page holding the slot bitmap. Pages go
; through the 4 KB DOS buffer at SwapBufLin (SwapDisk).

%define PWIN(w) (OffPage2 + (((w) - KernelBase) >> 10))
SwapPageable equ 200h
SwapSwapped equ 400h
SwapFirstPage equ 0

%macro SwapSaveWin 0
        push dword [PWIN(OffPageTableWin)]
        push dword [PWIN(OffFreePageWin)]
        push dword [PWIN(OffSDir0Win)]
        push dword [PWIN(OffSDir1Win)]
%endmacro
%macro SwapLoadWin 0
        pop dword [PWIN(OffSDir1Win)]
        pop dword [PWIN(OffSDir0Win)]
        pop dword [PWIN(OffFreePageWin)]
        pop dword [PWIN(OffPageTableWin)]
        InvalidateTLB
%endmacro

        SEGM Text
;in: edi -> PTEs, ecx = count; marks them pageable
        DPROC SwapMarkPages
        push ecx
.l:     or dword [edi + ecx * 4 - 4], SwapPageable
        loop .l
        pop ecx
        ret


;Text16 variables are reached through a relocated base
;(SwapHandle, SwapPSP, SwapInDos); swap stays off without
;a file
;reserve the bitmap page
        DPROC SwapInit
        pushad
        cmp dword [OffTotalVCPIPages], 0
        jne .out                       ;VCPI stays nonpageable
        mov esi, OffSwapHandle
        RRT
        cmp byte [esi + 8], 0          ;size 0 = off
        je .out
        cmp dword [esi + 4], 0
        je .out
        mov eax, [esi + 4]
        movzx eax, byte [eax]          ;InDOS at start: a host run by
        mov [esi + 9], al              ;a DOS EXEC of a client starts at 1
        movzx eax, byte [esi + 8]
        cmp al, 80h                    ;one bitmap page
        jbe .sz
        mov al, 80h
.sz:    shl eax, 8
        mov [OffSwapMax], eax
        call SwapTake
        jz .out
        and eax, ~0FFFh
        mov [OffSwapIdx], eax
        or eax, 67h
        mov [PWIN(OffSDir1Win)], eax
        InvalidateTLB
        mov edi, OffSDir1Win
        xor eax, eax
        mov ecx, 1024
        cld
        rep stosd
.out:   popad
        ret

;out: ZF=1 no frame, else eax = frame entry
SwapTake:
        push 0
        mov edi, esp
        mov ecx, 1
        call XAllocPages
        pop eax
        ret

;free the slot in ebp
SwapFreeSlot:
        push dword [PWIN(OffSDir1Win)]
        mov eax, [OffSwapIdx]
        or eax, 67h
        mov [PWIN(OffSDir1Win)], eax
        InvalidateTLB
        btr dword [OffSDir1Win], ebp
        pop dword [PWIN(OffSDir1Win)]
        InvalidateTLB
        ret

;SDir1Win maps the bitmap; out: CF=0 ebp = slot taken, CF=1 full
SwapSlotAlloc:
        xor ebp, ebp
.s:     bts dword [OffSDir1Win], ebp
        jnc .got
        inc ebp
        cmp ebp, [OffSwapMax]
        jb .s
        stc
.got:   ret

;first page out: make the swap file (edi = &SwapHandle, host PSP
;current). The name lives in a DOS block (SwapNameSeg): %TEMP%,
;else %TMP%, else the current directory, plus ZRDXnnnn.TMP. Each
;call is tried as LFN (CF set) and repeated as plain DOS.
;out: CF=1 on failure; the handle is stored; edi, ebp are kept
SwapCreate:
        push edi
        push ebp
        mov ebx, 11h                   ;272 bytes: 260 path + name
        mov ah, 48h
        call Dos1Call
        jc .out
        mov ebp, edi
        movzx ebx, word [ebp + 10]     ;environment (saved at init)
        mov [ebp + 10], ax             ;SwapNameSeg
        movzx esi, ax
        shl esi, 4                     ;esi = name linear
        shl ebx, 4
        xor edx, edx
.scan:  cmp byte [ebx], 0
        je .none
        cmp dword [ebx], 'TMP='
        jne .tmp
        lea edx, [ebx + 4]
        jmp short .skip
.tmp:   cmp dword [ebx], 'TEMP'
        jne .skip
        cmp byte [ebx + 4], '='
        jne .skip
        lea ebx, [ebx + 5]
        jmp short .copy
.skip:  inc ebx
        cmp byte [ebx - 1], 0
        jne .skip
        jmp short .scan
.none:  mov ebx, edx
        xor ecx, ecx
        test ebx, ebx
        jz .tail
.copy:  xor ecx, ecx
.cp:    mov al, [ebx]
        test al, al
        jz .cpend
        cmp ecx, 247                   ;leaves room for the name
        jae .nodir
        mov [esi + ecx], al
        inc ecx
        inc ebx
        jmp short .cp
.cpend: jecxz .tail
        cmp byte [esi + ecx - 1], '\'
        je .tail
        mov byte [esi + ecx], '\'
        inc ecx
        jmp short .tail
.nodir: xor ecx, ecx
.tail:  push ecx                       ;length of the directory part
        movzx eax, word [46Ch]         ;timer ticks
        movzx edx, word [ebp + 2]
        xor eax, edx
        lea ebx, [esi + ecx]
        mov dword [ebx], 'ZRDX'
        add ebx, 4
        mov ecx, 4
.hex:   rol ax, 4
        mov dl, al
        and dl, 0Fh
        add dl, '0'
        cmp dl, '9'
        jbe .dig
        add dl, 7
.dig:   mov [ebx], dl
        inc ebx
        loop .hex
        mov dword [ebx], '.TMP'
        mov byte [ebx + 4], 0
        shr esi, 4                     ;ES:DS = the name block
        mov ebx, 2                     ;read/write
        mov ecx, 2                     ;hidden
        mov edx, 10h                   ;create new, fail if exists
        mov ax, 716Ch                  ;DS:SI = name (SI = 0)
        call Dos1Call
        pop ecx
        jnc .made
        mov ebx, esi
        shl ebx, 4
        mov byte [ebx + ecx], 0        ;plain call: path + temp name
        xor edx, edx
        mov ecx, 2
        mov ah, 5Ah
        call Dos1Call
        jnc .made
        mov word [ebx], 5C2Eh          ;".\"
        mov byte [ebx + 2], 0
        mov ah, 5Ah
        call Dos1Call
        jc .out
.made:  mov [ebp], ax                  ;SwapHandle
        clc
.out:   pop ebp
        pop edi
        ret

%ifdef SWAPFAIL
;test builds only: the size byte (zrdxconf /S) selects a disk
;error on the 3rd write (71h) or on the 3rd read (72h)
SwapInject:
        mov al, [edi + 8]
        mov ah, [esp + 9]              ;3Fh read / 40h write
        cmp al, 71h
        jne .rd
        cmp ah, 40h
        jmp short .chk
.rd:    cmp al, 72h
        jne .no
        cmp ah, 3Fh
.chk:   jne .no
        inc dword [OffSwapFailCnt]
        cmp dword [OffSwapFailCnt], 3
        jne .no
        stc
        ret
.no:    clc
        ret
%endif
;in: ah = 3Fh read / 40h write of slot ebp; the page is
;the frame mapped at OffFreePageWin, moved through a 4 KB DOS
;block (allocated on first use under the host PSP, freed by
;DOS at exit). out: CF=1 on failure (DOS busy, nested, PL0,
;no DOS memory, I/O error, disk full); the host PSP owns the
;handle
SwapDisk:
        pushad
        mov ecx, cs
        test cl, 3
        jz .err
        mov edi, OffSwapHandle
        RRT
        mov esi, [edi + 4]
        mov al, [esi]
        cmp al, [edi + 9]
        jne .err
        cmp byte [esi - 1], 0
        jne .err
        cmp byte [OffSwapIO], 0
        jne .err
        mov byte [OffSwapIO], 1
        push eax
        mov ah, 51h
        call Dos1Call
        push ebx
        movzx ebx, word [edi + 2]
        mov ah, 50h
        call Dos1Call
        cmp word [edi], 0
        jne .made
        call SwapCreate
        jc .fail
.made:
%ifdef SWAPFAIL
        call SwapInject
        jc .fail
%endif
        cmp dword [OffSwapBufLin], 0
        jne .have
        mov ebx, 100h
        mov ah, 48h
        call Dos1Call
        jc .fail
        movzx eax, ax
        shl eax, 4
        mov [OffSwapBufLin], eax
.have:  mov esi, [OffSwapBufLin]
        cmp byte [esp + 5], 40h
        jne .seek
        push esi
        mov edi, esi
        mov esi, OffFreePageWin
        mov ecx, 1024
        cld
        rep movsd
        pop esi
        mov edi, OffSwapHandle
        RRT
.seek:  shr esi, 4
        movzx ebx, word [edi]
        mov edx, ebp
        shl edx, 12
        mov ecx, edx
        shr ecx, 16
        mov ax, 4200h
        call Dos1Call
        jc .fail
        movzx ebx, word [edi]
        mov ecx, 4096
        xor edx, edx
        mov ah, [esp + 5]
        call Dos1Call
        jc .fail
        cmp ax, 4096
        je .copy
        stc
        jmp short .fail
.copy:  cmp byte [esp + 5], 3Fh
        clc
        jne .fail
        mov esi, [OffSwapBufLin]
        mov edi, OffFreePageWin
        mov ecx, 1024
        cld
        rep movsd
        clc
.fail:  pushfd
        pop edx
        pop ebx
        mov ah, 50h
        call Dos1Call
        push edx
        popfd
        mov byte [OffSwapIO], 0
        pop eax
        popad
        ret
.err:   stc
        popad
        ret

        DPROC SwapXFree
        pushad
        mov ebx, ecx
.l:     mov ebp, [esi]
        mov eax, ebp
        and eax, SwapPageable | SwapSwapped | 1
        cmp eax, SwapPageable | SwapSwapped
        jne .plain
        shr ebp, 12
        call SwapFreeSlot
        mov dword [esi], 0
        jmp short .next
.plain: and dword [esi], ~0E00h
        push esi
        mov ecx, 1
        call XFreePages
        pop esi
.next:  add esi, 4
        dec ebx
        jnz .l
        popad
        ret

;frees one frame into the pool; CF=1 if nothing to evict
        DPROC SwapEvict
        pushad
        SwapSaveWin
        movzx eax, byte [OffSwapBusy]
        push eax
        mov byte [OffSwapBusy], 1
        xor eax, eax
        mov ebx, [OffSwapIdx]
        test ebx, ebx
        jz .done
        lea edx, [ebx + 67h]
        mov [PWIN(OffSDir1Win)], edx
        InvalidateTLB
.scan:  mov ecx, [OffSwapHand]
        cmp ecx, SwapFirstPage
        jae .hand
        mov ecx, SwapFirstPage
.hand:  mov esi, 2049
.pde:   mov eax, ecx
        shr eax, 10
        cmp eax, 1024
        jb .inrng
        mov ecx, SwapFirstPage
        mov eax, ecx
        shr eax, 10
.inrng: cmp dword [OffnEntriesInTable + eax * 4], 0
        je .skip
        mov edx, [OffPageDir + eax * 4]
        test dl, 1
        jz .skip
        mov [PWIN(OffPageTableWin)], edx
        InvalidateTLB
.pte:   mov edi, ecx
        and edi, 3FFh
        mov edx, [OffPageTableWin + edi * 4]
        mov eax, edx
        and eax, SwapPageable | 1
        cmp eax, SwapPageable | 1
        jne .next
        test dl, 20h
        jz .victim
        and dword [OffPageTableWin + edi * 4], ~20h
.next:  inc ecx
        test ecx, 3FFh
        jnz .pte
        jmp short .adv
.skip:  or ecx, 3FFh
        inc ecx
.adv:   dec esi
        jnz .pde
        xor eax, eax
        jmp .done
.victim:
        call SwapSlotAlloc
        jc .full
        push ecx
        push edi
        and edx, ~0FFFh
        push edx
        or edx, 67h
        mov [PWIN(OffFreePageWin)], edx
        InvalidateTLB
        mov ah, 40h
        call SwapDisk
        pop edx
        pop edi
        pop ecx
        jc .wfail
        mov eax, ebp
        shl eax, 12
        or eax, SwapPageable | SwapSwapped
        mov [OffPageTableWin + edi * 4], eax
        inc ecx
        mov [OffSwapHand], ecx
        InvalidateTLB
        or edx, 67h
        push edx
        mov esi, esp
        mov ecx, 1
        call XFreePages
        add esp, 4
        mov eax, 1
        jmp short .done
.wfail: btr dword [OffSDir1Win], ebp
.full:  xor eax, eax
.done:  pop edx
        mov [OffSwapBusy], dl
        SwapLoadWin
        cmp eax, 1
        popad
        ret

;in: esi = page number; out: CF=1 no page table, else
;edi = PTE index, eax = PTE (PTE window mapped)
SwapPte:
        mov eax, esi
        shr eax, 10
        cmp dword [OffnEntriesInTable + eax * 4], 0
        je .no
        mov edx, [OffPageDir + eax * 4]
        test dl, 1
        jz .no
        mov [PWIN(OffPageTableWin)], edx
        InvalidateTLB
        mov edi, esi
        and edi, 3FFh
        mov eax, [OffPageTableWin + edi * 4]
        clc
        ret
.no:    stc
        ret

;in: [OffSwapCR2]; out: ZF=1 if the page is swapped and
;pageable (esi = page number, PTE window mapped)
SwapLookup:
        mov esi, [OffSwapCR2]
        shr esi, 12
        call SwapPte
        jc .no
        and eax, SwapPageable | SwapSwapped | 1
        cmp eax, SwapPageable | SwapSwapped
        ret
.no:    test esp, esp
        ret

;in: esi = page, bl = 0 lock / 1 unlock, bh = 0 check only /
;1 apply. A locked page is resident, loses SwapPageable and
;keeps a count (0..3) in PTE bits 11:10; a swapped page is
;touched in first. out: CF=1, al = DPMI error code
;(ecx and edi are kept)
SwapLockPage:
        push ecx
        push edi
.again: pushfd
        cli
        call SwapPte
        jc .inv
        test al, 1
        jnz .res
        and eax, SwapPageable | SwapSwapped
        cmp eax, SwapPageable | SwapSwapped
        jne .inv
        test bl, bl
        jnz .nlk
        test bh, bh
        jz .ok
        popfd
        mov eax, esi
        shl eax, 12
        mov al, [eax]
        jmp .again
.res:   mov ecx, eax
        and ecx, 0C00h
        test bl, bl
        jnz .unl
        test ah, 2
        jnz .first
        test ecx, ecx
        jz .ok
        cmp ecx, 0C00h
        je .full
        jmp short .inc
.first: and ah, ~2
.inc:   add eax, 400h
        jmp short .store
.unl:   test ah, 2
        jnz .nlk
        test ecx, ecx
        jz .ok
        sub eax, 400h
        test eax, 0C00h
        jnz .store
        or ah, 2
.store: test bh, bh
        jz .ok
        mov [OffPageTableWin + edi * 4], eax
.ok:    popfd
        pop edi
        pop ecx
        clc
        ret
.inv:   mov al, 23h
        jmp short .e
.full:  mov al, 13h
        jmp short .e
.nlk:   mov al, 2
.e:     popfd
        pop edi
        pop ecx
        stc
        ret

;0600h/0601h at PL1. in: esi = linear start, edi = bytes,
;dl = 0 lock / 1 unlock; two passes (check, apply). out:
;CF=1, al = DPMI error (21h value, 23h linear address,
;13h count full, 02h not locked); other registers kept
        DPROC SwapLockRange
        pushad
        SwapSaveWin
        mov bl, dl
        mov bh, 0
        mov al, 21h
        test edi, edi
        jz .err
        mov al, 23h
        add edi, esi
        jc .err
        dec edi
        shr esi, 12
        shr edi, 12
        sub edi, esi
        inc edi
        mov ebp, esi
.pass:  mov esi, ebp
        mov ecx, edi
.page:  call SwapLockPage
        jc .err
        inc esi
        loop .page
        xor bh, 1
        jnz .pass
        SwapLoadWin
        popad
        clc
        ret
.err:   mov [esp + 10h + 1Ch], al
        SwapLoadWin
        popad
        stc
        ret

;PL0: CF=0 if CR2 is a swapped page this module can service
        DPROC SwapCheck
        cmp byte [OffSwapBusy], 0
        jne .no
        cmp dword [OffSwapIdx], 0
        je .no
        cli
        pushad
        mov eax, cr2
        mov [OffSwapCR2], eax
        SwapSaveWin
        call SwapLookup
        setz al
        SwapLoadWin
        cmp al, 1
        popad
        ret
.no:    stc
        ret

;PL0: abandons the exception frame (stack: ebp, number,
;error code, EIP, CS, EFLAGS, ESP, SS) and continues at PL1
;on a copy of it: a gate-style frame (3 dwords if the fault
;came from PL1, else 5 dwords below TSS_ESP1) and all
;registers. The client stack is never touched.
        DPROC SwapToPL1
        push eax
        mov eax, [ss:OffTSS + TSS_ESP1]
        test byte [esp + 20], 2
        jnz .outer
        mov eax, [esp + 28]
        mov ebp, [esp + 16]
        mov [ss:eax - 12], ebp
        mov ebp, [esp + 20]
        mov [ss:eax - 8], ebp
        mov ebp, [esp + 24]
        mov [ss:eax - 4], ebp
        sub eax, 12
        jmp short .regs
.outer: mov ebp, [esp + 16]
        mov [ss:eax - 20], ebp
        mov ebp, [esp + 20]
        mov [ss:eax - 16], ebp
        mov ebp, [esp + 24]
        mov [ss:eax - 12], ebp
        mov ebp, [esp + 28]
        mov [ss:eax - 8], ebp
        mov ebp, [esp + 32]
        mov [ss:eax - 4], ebp
        sub eax, 20
.regs:  mov ebp, [esp]
        mov [ss:eax - 4], ebp
        mov ebp, [esp + 4]
        mov [ss:eax - 8], ebp
        mov [ss:eax - 12], ecx
        mov [ss:eax - 16], edx
        mov [ss:eax - 20], ebx
        mov [ss:eax - 24], esi
        mov [ss:eax - 28], edi
        mov ebp, ds
        mov [ss:eax - 32], ebp
        mov ebp, es
        mov [ss:eax - 36], ebp
        mov ebp, fs
        mov [ss:eax - 40], ebp
        mov ebp, gs
        mov [ss:eax - 44], ebp
        sub eax, 44
        push Data1Selector
        push eax
        push Code1Selector
        push OffSwapPL1
        retf

;PL1 continuation of SwapToPL1: services the fault, then
;returns to the faulting instruction by iretd. A failure
;(no frame even after eviction) is fatal.
        DPROC SwapPL1
        push ss
        pop ds
        push ss
        pop es
        call SwapService
        jc .fatal
        pop gs
        pop fs
        pop es
        pop ds
        pop edi
        pop esi
        pop ebx
        pop edx
        pop ecx
        pop ebp
        pop eax
        iretd
.fatal: ud2

;in: [OffSwapCR2]; out: CF=0 page brought back, CF=1 failed
        DPROC SwapService
        pushad
        SwapSaveWin
        mov byte [OffSwapBusy], 1
        xor eax, eax
        call SwapLookup
        jnz .out0
        push esi
        push 0
        mov edi, esp
        mov ecx, 1
        call XAllocPages
        pop ebx
        jz .fail
        and ebx, ~0FFFh
        mov esi, [esp]
        mov edi, esi
        and edi, 3FFh
        mov ebp, [OffPageTableWin + edi * 4]
        shr ebp, 12
        lea eax, [ebx + 67h]
        mov [PWIN(OffFreePageWin)], eax
        InvalidateTLB
        push ebx
        mov ah, 3Fh
        call SwapDisk
        pop ebx
        jc .fail
        mov eax, [OffSwapIdx]
        or eax, 67h
        mov [PWIN(OffSDir1Win)], eax
        InvalidateTLB
        btr dword [OffSDir1Win], ebp
        pop esi
        mov edi, esi
        and edi, 3FFh
        lea eax, [ebx + 67h + SwapPageable]
        mov [OffPageTableWin + edi * 4], eax
        InvalidateTLB
        mov eax, 1
        jmp short .out
.fail:  add esp, 4
.out0:  xor eax, eax
.out:   mov byte [OffSwapBusy], 0
        SwapLoadWin
        cmp eax, 1
        popad
        ret

        ESEG Text
