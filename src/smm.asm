;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

Segm  Text
;assume cs:Text, ds:Data, ss:Data
DPROC AllocPages
PageSize = 1000h
@@Pos   equ edx
@@Size  equ eax
@@PageSubdirIndex equ ebx
@@XDir  equ edi
@@XSize equ ecx
@@T     equ esi
@@XDir1 equ esi
@@nFreePagesOnDir equ ebp
        pushad
;uses eax, ebx, ecx, edx, esi, edi, ebp
        mov  @@Pos,  [esp+8 + 8*4]
        mov  @@Size, [esp+12 + 8*4]
        add  @@Pos,  PageSize-1
        add  @@Size, @@Pos
        shr  @@Pos,  12  ;Calculating number of first page
        shr  @@Size, 12
        cld
        push @@Pos
        sub  @@Size, @@Pos ;calculating number of pages
        $ifnot jbe
        $do
          cmp byte ptr [esp+4+8*4+4], 0
          $ifnot je
            IFDEF Release
            sti
            ENDIF
            jmp short $+2
            cli
          $endif
          mov  @@PageSubdirIndex, @@Pos
          shr  @@PageSubdirIndex, 10
          cmp  nEntriesInTable[@@PageSubdirIndex*4], 0
          $ifnot jne
            ;allocate page for table
            lea  @@XDir, PageDir[@@PageSubdirIndex*4]
            mov  @@XSize, 1
            call XAllocPages
            jz @@Fail
          $endif
          mov  @@T, PageDir[@@PageSubdirIndex*4]
          mov  PageTableEntry, @@T
          mov  @@XDir, @@Pos
          and  @@XDir, 3FFh
          mov  @@XSize, 400h
          sub  @@XSize, @@XDir
          cmp  @@XSize, @@Size
          $ifnot jb
            mov  @@XSize, @@Size
          $endif
          lea  @@XDir, PageTableWin[@@XDir*4]
          call XAllocPages
          $ifnot jnz
            cmp nEntriesInTable[@@PageSubdirIndex*4], 0
            $ifnot jne
              ;free previsionaly allocated subdirectory, if main allocation failed
              lea  @@XDir1, PageDir[@@PageSubdirIndex*4]
              mov  @@XSize, 1
              call XFreePages
            $endif
@@Fail:
            pop  eax
            push dword ptr [esp+4+8*4]
            call FreePagesN
            stc
            jmp  @@ret
          $endif
          add  nEntriesInTable[@@PageSubdirIndex*4], @@XSize
          add  @@Pos,  @@XSize
          sub  @@Size, @@XSize
        $enddo ja
        $endif
        pop  eax                   ;add esp, 4
        clc
@@ret:
        popad
        ret 12

;destroy @@XDir(edi), return @@XSize(ecx)
;destroy ebp, esi
XAllocPages:
        InvalidateTLB
        cmp nFreePages, 0
        $ifnot je
          mov @@nFreePagesOnDir, nEntriesInFplist ; nFreePagesOnDir
          or  @@nFreePagesOnDir, @@nFreePagesOnDir
          $ifnot jz
            cmp @@XSize, @@nFreePagesOnDir
            $ifnot jbe
              mov @@XSize, @@nFreePagesOnDir
            $endif
            sub  @@nFreePagesOnDir, @@XSize
            lea  esi, fplist[@@nFreePagesOnDir*4+4]
            push ecx
            rep  movsd
            pop  ecx
            ;jmp  @@L0
          $else jmp
            mov  @@T, fplist[0]
            lea  @@XSize, [@@nFreePagesOnDir+1]
            xchg FpListEntry, @@T
            mov  [@@XDir], @@T
            mov  @@nFreePagesOnDir, 1023
            ;@@L0:
          $endif
          mov  nEntriesInFplist, @@nFreePagesOnDir
          sub  nFreePages, @@XSize
          ;jmp @@Ret2
        $else jmp
;VCPIAlloc:
          push eax edx
          xor  @@XSize, @@XSize
          call Alloc1Page
          $ifnot jnc
            mov  [@@XDir], edx
            inc  @@XSize
          $endif
          pop  edx eax
        $endif
;@@Ret2:
        InvalidateTLB
        or   @@XSize, @@XSize
        retn
        ENDP

;@@Pos, @@Size
DPROC FreePages
@@Pos   equ eax
@@Size  equ edx
;uses eax, edx
        push eax edx
        mov @@Pos, [esp+8+2*4]
        mov @@Size, [esp+12+2*4]
        add @@Pos, PageSize-1
        add @@Size, @@Pos
        shr @@Pos, 12
        shr @@Size, 12
        push dword ptr [esp+4+2*4]
        call FreePagesN
        pop edx eax
        ret 12
        ENDP
DPROC FreePagesN
@@FirstPage   equ eax
@@LastPage    equ edx
@@XDir        equ esi
@@XSize       equ ecx
@@nd          equ @@LastPage
@@SubdirIndex equ ebx
@@T           equ edi
        ;Log
        pushad
        cld
        sub @@LastPage, @@FirstPage  ;@@nd
        $ifnot jbe
        $do
          cmp byte ptr [esp+4+8*4], 0
          $ifnot je
            IFDEF Release
            sti
            ENDIF
            jmp short $+2
            cli
          $endif
          mov  @@SubdirIndex, @@FirstPage
          shr  @@SubdirIndex, 10
          mov  @@XSize, PageDir[@@SubdirIndex*4]
          mov  PageTableEntry, @@XSize
          ;InvalidateTLB
          mov  @@XSize, 400h
          mov  @@XDir, @@FirstPage
          and  @@XDir, 3FFh
          sub  @@XSize, @@XDir
          cmp  @@XSize, @@nd
          $ifnot jb
            mov @@XSize, @@nd
          $endif
          ;Log
          lea  @@XDir, PageTableWin[@@XDir*4]
          call XFreePages
          add  @@FirstPage, @@XSize
          sub  @@nd, @@XSize
          sub  nEntriesInTable[@@SubdirIndex*4], @@XSize
          $ifnot jnz
            lea  @@XDir, PageDir[@@SubdirIndex*4]
            mov  @@XSize, 1
            call XFreePages
          $endif
          or @@nd, @@nd
        $enddo jnz
        $endif
        popad
        ret 4
;@@XDir, @@XSize
;destroy @@XDir, edi, eax
XFreePages:
@@T equ edi
        InvalidateTLB
        mov  @@T, 1023
        sub  @@T, nEntriesInFplist
        $ifnot jz
          cmp @@XSize, @@T
          $ifnot jb
            mov @@XSize, @@T
          $endif
          mov  @@T, nEntriesInFplist; nFreePagesOnDir
          add  nEntriesInFplist, @@XSize
          lea  edi, fplist[@@T*4+4]
          push ecx
          push ecx
          push esi
          rep  movsd
          pop  edi
          pop  ecx
          push eax
          xor  eax, eax
          rep  stosd
          pop  eax
          pop  ecx
          InvalidateTLB
        $else jmp
          mov  nEntriesInFplist, @@T    ; @@T already zero
          lea  @@XSize, [@@T+1]
          xchg @@T, [@@XDir]
          xchg FpListEntry, @@T
          InvalidateTLB
          mov  fplist[0], @@T
        $endif
        add  nFreePages, @@XSize
        retn
        ENDP
DPROC ReturnFreePool
        pushad
        mov  ecx, FpListEntry         ;FreePagesRef
        cmp  nFreePages, 0
        $ifnot jz
        mov  ecx, nEntriesInFplist  ;nFreePagesOnDir
        jcxz @@L
        $do
        $do

        sti
        jmp  short $+2
        cli
        mov  edx, fplist[ecx*4]
        call @@VCPIFree
        dec  nFreePages
        $enddo loop
        @@L:
        mov  edx, fplist[0]
        xchg FpListEntry, edx
        call @@VCPIFree
        InvalidateTLB
        mov  ch, 1024 shr 8
        dec  nFreePages
        $enddo loopnz
        $endif
        @@L0:
@@E:
@@E1:
        ;mov nFreePagesOnDir, 1023
        popad
        ret

@@VCPIFree:
        test dh, VCPIPageBit
        $ifnot jz
        and  dx, 0F000h
        mov  ax, 0DE05h
        VCPITrap
        $endif
        retn
        ENDP
Eseg Text
