;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

Segm Text
assume cs:Text
DPROC Alloc1Page
        cmp  VCPIMemAvailable, 1
        jne  @@TryXMS
        mov  ax, 0DE04h
        VCPITrap
        and  dh, 0F0h
        or   dh, VCPIPageBit  ;mark this page as allocated via VCPI
                              ;(must be freed via VCPI on termination)
        or   ah, ah
        jz   @@Success
        shr  VCPIMemAvailable, 1
@@TryXMS:
        shr  XMSBlockNotAllocated, 1    ;check and clear flag
        $ifnot jnc
        ;try to allocate and lock xms block
          pushad
          DB PushWCode
          DW ROffAllocXMSBlock, seg DGROUP16
          call DosPCall
          popad
        $endif
        mov  edx, OffFreeXMSCount
        RRT
        cmp  dword ptr ds:[edx], 0
        je   @@FailAlloc
        dec  dword ptr ds:[edx]
        inc  dword ptr ds:[edx+4]  ;FirstFreeXMS
        mov  edx, ds:[edx+4]
        dec  edx
        shl  edx, 12               ;convert pages to bytes
@@Success:
        stc
        mov  dl, 67h               ;set default access rights
@@FailAlloc:                ;carry flag is clear on error
        retn

        ENDP
comment &
DPROC VCPIPMEmulator
;VCPIGetPage, FreePage, GetPCount
        cmp al, 4
        $ifnot jne
;-------------------------------- GetPage -----------------------------------
          cmp  VFreePagesCount, 0
          $ifnot jne
            cmp  FreeXMSCount, 0
            je   @@FailAlloc
            mov  edx, FirstFreeXMS
            inc  FirstFreeXMS
            dec  FreeXMSCount
            shl  edx, 12
            jmp  @@Success
          $else
            mov  edx, VFreePage[0]
            mov  VFreePageRef, edx
            InvalidateTLB
            dec  VFreePagesCount
            jmp  @@Success
          $endif
        $endif
        cmp al, 5
        $ifnot jne
;------------------------------- FreePage -----------------------------------
          push edx
          or   dl, 67h
          xchg VFreePageRef, edx
          InvalidateTLB
          mov  VFreePage[0], edx
          pop  edx
          inc  VFreePagesCount
          jmp  @@Success
        $endif
        cmp al, 3
        $ifnot jne
;----------------------------- GetNFreePages ------------------------------
          mov edx, FreeXMSCount
          add edx, VFreePagesCount
        $endif
@@Success:
        mov ah, 0
@@FailAlloc:
        retf
;        mov ah, 80h
;        retf
ENDP              ;&

Eseg Text

;page allocated via VCPI
;page locked
