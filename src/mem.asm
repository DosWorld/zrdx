;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

Segm Text
malloc  PROC
DPMIFn 5 01
        @@NewMCB  equ EAX
        @@NewMCBW equ AX
        @@Offset  equ EDI
        @@OffsetW equ DI
        @@NewSize equ EBX
        pushad
        push ds
        pop  es
        shl  ebx, 16
        mov  bx, cx
        add  @@NewSize, 1000h-1
        and  @@NewSize, not 0FFFh
        call FindLinWindow
        call MakeNewMCB
        push @@NewSize
        push @@Offset
        push 1
        call AllocPages
        $ifnot jnc
          call FreeMCB
          jmp MemError
        $endif
ReturnMalloc:
        mov  word ptr [esp].PA_ECX, @@OffsetW
        shr  @@Offset, 16
        mov  word ptr [esp].PA_EBX, @@OffsetW
        mov  word ptr [esp].PA_EDI, @@NewMCBW
        shr  @@NewMCB, 16
        mov  word ptr [esp+32].DPMIFrame_ESI, @@NewMCBW
        popad
        retn
malloc  ENDP

free    PROC
DPMIFn 5 02
        @@NewMCB equ eax
        pushad
        push ds
        pop  es
        mov  esi, [esp+32].DPMIFrame_ESI
        shl  esi, 16
        mov  si, di
        cmp  esi, OffMCBVector
        jb   @@Err
        cmp  esi,  MCBVectorEnd
        jae  @@Err
        test esi, 0Fh
        jnz  @@Err
        cmp  [esi].MCB_StartOffset, 0
        je   @@Err
        mov  ecx, [esi].MCB_EndOffset
        sub  ecx, [esi].MCB_StartOffset
        push ecx
        push [esi].MCB_StartOffset
        push 1
        call FreePages
        xchg @@NewMCB, esi
        call FreeMCB
        popad
        ret
@@Err:  mov al, 0
        jmp MemError
free    ENDP
MemError3: pop ebp
MemError2: pop ebp
MemError1: pop ebp
MemError:
        ;mov  byte ptr [esp].PA_EAX, al
        popad
        jmp  DPMIError1

realloc PROC
DPMIFn 5 03
        @@NewMCB  equ EAX
        @@NewSize equ EBX
        @@OptSize equ EBP
        @@CurMCB  equ ESI
        @@OptMCB  equ ECX
        @@Err EQU MemError
        pushad
        push ds
        pop  es
        shl  ebx, 16
        mov  esi, [esp+32].DPMIFrame_ESI
        shl  esi, 16
        mov  bx, cx
        mov  si, di
        add  @@NewSize, 1000h-1
        and  @@NewSize, not 0FFFh
        cmp  esi, OffMCBVector
        jb   @@Err
        cmp  esi, MCBVectorEnd
        jae  @@Err
        test esi, 0Fh
        jne  @@Err
        cmp  [esi].MCB_StartOffset, 0
        je   @@Err
        mov  @@OptMCB, [@@CurMCB].MCB_Next
        mov  @@OptSize, [@@OptMCB].MCB_StartOffset
        sub  @@OptSize, [@@CurMCB].MCB_StartOffset
        mov  @@OptMCB, @@CurMCB
        cmp  @@OptSize, @@NewSize
        $ifnot jb
          call FindLinWindowX
        $else jmp
          call FindLinWindow
        $endif
        cmp @@CurMCB, @@OptMCB
        $ifnot je
          call MakeNewMCB
          ;alloc 2 new pages;
          push @@NewMCB
          push @@CurMCB
          ;must preserve @@CurMCB, @@NewMCB, @@NewSize (esi, ebx, eax)
          push eax
          push eax        ;reserve space for @@GCount
          lea  edx, [esp-8]
          $do
          IFNDEF VMM
            push edi             ;allocate space for phisical address
            mov  edi, esp        ;set pointer to it
            xor  ecx, ecx
            inc  ecx             ;mov 1 to ecx
            push esi             ;preserve esi
            call XAllocPages     ;allocate 1 page
            pop  esi
            jz   @@FreeAndErr
          ELSE
            call alloc_page
            jc @@FreeAndErr
            push eax
          ENDIF
          cmp  esp, edx        ;test for end
          $enddo jne
          @@MSize equ edi
          @@T     equ ebp
          @@T1    equ ecx
          mov  @@MSize, [@@CurMCB].MCB_EndOffset
          sub  @@MSize, [@@CurMCB].MCB_StartOffset ;calculate size of the current block
          mov  @@T, @@NewSize
          sub  @@T, @@MSize
          $ifnot jb
            mov  @@T1, [@@NewMCB].MCB_EndOffset
            sub  @@T1, @@T
            push @@T
            push @@T1
            push 1
            call AllocPages
            $toendif jnc
@@FreeAndErr:
            add  edx, 8
            $do  jmp
            IFNDEF VMM
              mov  esi, esp
              xor  ecx,ecx
              inc  ecx
              call XFreePages
              pop  esi
            ELSE
              call free_page
            ENDIF
            $while
            cmp  esp, edx
            $enddo  jne
            pop  esi
            jmp  MemError
          $else; jmp
            neg  @@T
            mov  @@T1, [@@CurMCB].MCB_EndOffset
            sub  @@T1, @@T
            push @@T
            push @@T1
            push 1
            call FreePages
            mov @@MSize, @@NewSize
          $endif
          add  edx, 8          ;restore pointer to @@GCount and stack base
          ;all OK, move pages now
          @@PAddr0      equ ebp
          @@PAddr1      equ ebx
          @@DirIndex    equ eax
          @@SDirIndex0  equ esi
          @@SDirIndex1  equ edi
          @@LCount      equ ecx
          mov  @@PAddr0, [@@CurMCB].MCB_StartOffset
          mov  @@PAddr1, [@@NewMCB].MCB_StartOffset
          shr  @@Paddr0, 12
          shr  @@Paddr1, 12
          shr  @@MSize, 12
          mov  [edx], @@MSize
          cld
          $do
          mov  @@LCount, 3FFh
          mov  @@SDirIndex0, @@PAddr0
          and  @@SDirIndex0, @@LCount
          and  @@LCount, @@PAddr1
          lea  @@SDirIndex1, SDir1Win[@@LCount*4]
          cmp  @@LCount, @@SDirIndex0
          $ifnot ja
            mov @@LCount, @@SDirIndex0
          $endif
          lea  @@SDirIndex0, SDir0Win[@@SDirIndex0*4]
          sub  @@LCount, 400h
          neg  @@LCount
          cmp  @@LCount, [edx] ; @@GCount
          $ifnot jb
            mov  @@LCount, [edx] ; @@GCount
          $endif
          mov  @@DirIndex, @@PAddr1
          shr  @@DirIndex, 10
          lea  @@DirIndex, PageDir[@@DirIndex*4]
          cmp  dword ptr [nEntriesInTable-PageDir][@@DirIndex], 0
          $ifnot jnz
            pop dword ptr [@@DirIndex]
          $endif
          add  [nEntriesInTable-PageDir][@@DirIndex], @@LCount
          mov  eax, [@@DirIndex]
          mov  SDir1Entry, eax
          mov  @@DirIndex, @@PAddr0
          shr  @@DirIndex, 10
          lea  @@DirIndex, PageDir[@@DirIndex*4]
          push dword ptr[@@DirIndex]
          pop  SDir0Entry
          sub  [nEntriesInTable-PageDir][@@DirIndex], @@LCount
          $ifnot jnz
            push dword ptr [@@DirIndex]
            and  dword ptr [@@DirIndex], 0
          $endif
          add  @@PAddr0, @@LCount
          add  @@PAddr1, @@LCount
          InvalidateTLB
          xor  eax, eax
          sub  [edx], @@LCount          ;[esp] = @@GCount
          push ecx esi
          rep  movsd
          pop  edi ecx
          rep  stosd
          ;mov  @@GCount, [edx]
          $enddo jnz

          ;add  edx, 8
          $do  jmp
          IFNDEF VMM
            mov  esi, esp
            xor  ecx,ecx
            inc  ecx
            call XFreePages
            pop  esi
          ELSE
            call free_page
          ENDIF
          $while
          cmp  esp, edx
          $enddo  jne
          pop  esi                 ;add esp, 4
          pop  @@NewMCB                 ;free space for @@GCount
          pop  @@NewMCB
          call FreeMCB
          pop  eax
@@ReturnMalloc:
          mov  edi, [eax].MCB_StartOffset      ;@@Offset
          jmp  ReturnMalloc
        $else
          mov  @@MSize, [@@CurMCB].MCB_EndOffset
          sub  @@MSize, [@@CurMCB].MCB_StartOffset ;calculate size of the current block
          sub  @@MSize, @@NewSize
          $ifnot jae
            neg  @@MSize
            push @@MSize
            push [@@CurMCB].MCB_EndOffset
            push 1
            call AllocPages
            jc   @@Err
            add  [@@CurMCB].MCB_EndOffset, @@MSize
          $else jmp
            sub  [@@CurMCB].MCB_EndOffset, @@MSize
            push @@MSize
            push [@@CurMCB].MCB_EndOffset
            push 1
            call FreePages
          $endif
          xchg eax, @@CurMCB
          jmp @@ReturnMalloc
        $endif
;@@Err:  jmp MemError
realloc ENDP
FindLinWindow PROC
        @@NewMCB   equ EAX
        @@NewSize  equ EBX
        @@OptSize  equ EBP
        @@CurMCB   equ ESI
        @@OptMCB   equ ECX
        @@MaxCount equ EDX
        @@Count    equ EDI
        @@c        equ @@NewMCB
        @@t_size   equ @@CurMCB
        mov  @@OptMCB, RootMCB.MCB_Prev
        mov  @@OptSize, RootMCB.MCB_StartOffset; 0; 0FFF00000h ;LastMappedPage   !!!!!!!
        sub  @@OptSize, [@@OptMCB].MCB_EndOffset
        cmp  @@OptSize, @@NewSize
        $ifnot jae
          or  @@OptSize, -1
        $endif
FindLinWindowX:
        push @@t_size
        push @@OptMCB
        mov  @@MaxCount, nmemblocks
        cmp  @@MaxCount, 10000
        $ifnot jb
          mov @@MaxCount, 10000
        $endif
        mov  @@Count, 50
        cmp  @@Count, @@MaxCount
        $ifnot jb
          mov @@Count, @@MaxCount
        $endif
        sub  @@MaxCount, @@Count
        mov  @@c, MemRover
        inc  @@MaxCount
        $do  ;jmp
        dec  @@Count
        jz   @@CheckEnd
        @@Continue:
        mov  @@t_size, [@@c].MCB_StartOffset
        mov  @@c, [@@c].MCB_Prev
        sub  @@t_size, [@@c].MCB_EndOffset
        cmp  @@t_size, @@NewSize
        $loop jb
        cmp  @@t_size, @@OptSize
        $loop jae
        mov  @@OptSize, @@t_size
        mov  @@OptMCB, @@c
        or   @@MaxCount, @@MaxCount
        $enddo jnz
@@Exit:
        mov  MemRover, @@c
        pop  @@t_size        ;add esp, 4
        pop  @@t_size        ;restore value, destroyed by @@t_size
        retn
@@CheckEnd:
        cmp  @@MaxCount, 1
        je   @@L1
        cmp  @@OptMCB, [esp]
        jne  @@Exit
        @@L1:
        xchg @@Count, @@MaxCount
        or   @@Count, @@Count
        jnz  @@Continue
        cmp  @@OptSize, -1
        jne  @@Exit
        jmp  MemError3
FindLinWindow ENDP

MakeNewMCB PROC
        @@NewMCB  equ EAX
        @@NewSize equ EBX
        @@c       equ EDX
        @@count   equ EDI
        @@OptMCB  equ ECX
        @@NextMCB equ @@count
        @@PrevMCB equ @@c
        @@Offset  equ EDI          ;@@count & @@NextMCB
        mov  @@c, FMemRover
        or   @@c, @@c
        jz   @@AllocMCB
        mov  @@NewMCB, @@c
        mov  @@count, 14
        $do
          dec  @@count
          jz   @@Exit1
          @@L0:
          mov  @@c, [@@c].MCB_Next
          cmp  @@c, @@NewMCB
          $loop ja
          mov  @@NewMCB, @@c
          dec  @@count
          jnz  @@L0
        $enddo
@@Exit1:
        mov  @@NextMCB, [@@NewMCB].MCB_Next  ;delete MCB from free list
        mov  @@PrevMCB, [@@NewMCB].MCB_Prev
        mov  [@@NextMCB].MCB_Prev, @@PrevMCB
        mov  [@@PrevMCB].MCB_Next, @@NextMCB
        cmp  @@NewMCB, FMemRover
        $ifnot jne
          cmp  @@NextMCB, @@NewMCB
          $ifnot jne
            xor @@NextMCB, @@NextMCB
          $endif
          mov  FMemRover, @@NextMCB
        $endif
@@Exit:
;new MCB succefully allocated, now setup it
;insert new MCB in the allocated list
        mov  @@NextMCB, [@@OptMCB].MCB_Next
        mov  [@@NewMCB].MCB_Next,  @@NextMCB
        mov  [@@NewMCB].MCB_Prev,  @@OptMCB
        mov  [@@OptMCB].MCB_Next,  @@NewMCB
        mov  [@@NextMCB].MCB_Prev, @@NewMCB
;set start & end locations for new MCB
        mov  @@Offset, [@@OptMCB].MCB_EndOffset
        mov  [@@NewMCB].MCB_StartOffset, @@Offset
        lea  @@c, [@@Offset + @@NewSize]
        mov  [@@NewMCB].MCB_EndOffset, @@c
        inc  nmemblocks
        retn
@@AllocMCB:
        mov  @@NewMCB, MCBVectorEnd
        cmp  @@NewMCB, 800000h; OffMCBVector+
        jae  @@Error
        push size MCBStruct
        push @@NewMCB
        push 0
        call AllocPages
        jc   @@Error
        add  MCBVectorEnd, size MCBStruct
        jmp  @@Exit
@@Error:
        jmp  MemError1
MakeNewMCB ENDP

FreeMCB PROC
        @@MCB     equ eax
        @@MCB1    equ ecx
        @@NextMCB equ edx
        @@PrevMCB equ edi
        @@NextMCB1 equ @@NextMCB
        @@PrevMCB1 equ @@PrevMCB
        @@rover equ @@NextMCB
        @@next  equ @@PrevMCB
        @@size  equ @@PrevMCB
        dec nmemblocks
        mov @@NextMCB, [@@MCB].MCB_Next     ;exclude from allocated list
        mov @@PrevMCB, [@@MCB].MCB_Prev
        mov [@@NextMCB].MCB_Prev, @@PrevMCB
        mov [@@PrevMCB].MCB_Next, @@NextMCB
        cmp @@MCB, MemRover
        $ifnot jne
          mov MemRover, @@PrevMCB
        $endif
        mov @@rover, FMemRover
        or  @@rover, @@rover
        $ifnot jne
          mov  @@rover, @@MCB
        $else jmp
          mov ecx, 10
          $do
          mov @@rover, [@@rover].MCB_Next
          $enddo loop
          mov  @@next, [@@rover].MCB_Next
          mov  [@@MCB].MCB_Next, @@next
          mov  [@@next].MCB_Prev, @@MCB
        $endif
        mov  [@@MCB].MCB_Prev, @@rover
        mov  [@@rover].MCB_Next, @@MCB
        ;insert index in the free indexes list
        and  [@@MCB].MCB_StartOffset, 0
        mov  @@MCB1, MCBVectorEnd
        $do jmp
          cmp  [@@MCB1-size MCBStruct].MCB_StartOffset, 0
          $break jne
          sub  @@MCB1, size MCBStruct
          mov  @@NextMCB1, [@@MCB1].MCB_Next
          mov  @@PrevMCB1, [@@MCB1].MCB_Prev
          mov  [@@NextMCB1].MCB_Prev, @@PrevMCB1
          mov  [@@PrevMCB1].MCB_Next, @@NextMCB1
          cmp  @@MCB, @@MCB1
          $ifnot jne
            mov @@MCB, @@PrevMCB1
            cmp @@NextMCB1, @@MCB1
            $ifnot jne
              xor @@MCB, @@MCB
            $endif
          $endif
        $while
          cmp  @@MCB1, OffMCBVector
        $enddo ja
        mov  @@size, MCBVectorEnd
        sub  @@size, @@MCB1
        $ifnot je
          push @@size
          push @@MCB1
          push 0
          call FreePages
          mov  MCBVectorEnd, @@MCB1
        $endif
        mov  FMemRover, @@MCB
        retn
FreeMCB ENDP

DPROC CleanMemVector
        uses ebx, esi, eax
        mov ebx, OffMCBVector
        mov esi, ebx
        $do jmp

        cmp [ebx].MCB_StartOffset, 0
        $ifnot je
        mov  eax, [ebx].MCB_EndOffset
        sub  eax, [ebx].MCB_StartOffset
        push eax
        push [ebx].MCB_StartOffset
        push 1
        call FreePages
        $endif
        add ebx, size MCBStruct
        $while
        cmp ebx, MCBVectorEnd
        $enddo jb
        sub  ebx, esi
        push ebx
        push esi
        push 1
        call FreePages
        ret
        endp
IFNDEF Release
DPROC  Random
        mov eax, Seed
        imul eax, 015a4e35h
        inc eax
        mov Seed, eax
        ret
        ENDP
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
        $do
        call Random
        xor edx, edx
        mov esi, Vl
        div esi
        lea ebp, TVector[edx*4]
        cmp dword ptr [ebp], 0
        $ifnot jne
          call Random
          xor edx, edx
          mov esi, 200000*10;400000
          div esi
          push ecx
          lea ecx,  [edx+10]
          mov ebx,  ecx
          shr ebx,  16        ;bx:cx - linear address
          mov ax, 501h
          int 31h
          pop ecx
          jc  @@Exit1
          shl esi, 16
          mov si, di
          mov [ebp], esi
        $else jmp
          mov edi, [ebp]
          mov dword ptr [ebp], 0
          mov esi, edi
          shr esi, 16
          mov ax, 502h
          int 31h
          jc  @@Exit2
        $endif
        mov ah, 1
        sti
        int 16h
        cli
        $break jnz
        inc ecx
        mov ax, 0DE03h
        VCPITrap
        push edi
        push edx
        push ecx
        push MCBVectorEnd
        push nFreePages

        ;push esp 5 17
        ;call MemDump
        add esp, 5*4
        $enddo jmp
@@ret:
        sti
        mov ah, 0
        int 16h
        cli
        popad
        ret
@@Exit1:; ;mov word ptr ds:[0B8000h+160*6+50], 1F31h
        jmp @@ret
@@Exit2:; ;mov word ptr ds:[0B8000h+160*6+50], 1F32h
        jmp @@ret
        ENDP
ENDIF
ESeg Text
;end
