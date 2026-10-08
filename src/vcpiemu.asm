;             This file is part of the ZRDX 0.50OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

        SEGM Text
;assume cs:Text
        DPROC Alloc1Page
        cmp byte [OffVCPIMemAvailable], 1
        jne TryXMS@8
        mov ax, 0DE04h
        VCPITrap
        and dh, 0F0h
        or dh, VCPIPageBit  ;mark this page as allocated via VCPI
                              ;(must be freed via VCPI on termination)
        or ah, ah
        jz Success@8
        shr byte [OffVCPIMemAvailable], 1
TryXMS@8:
        shr byte [OffXMSBlockNotAllocated], 1    ;check and clear flag
        _ifnot jnc
        ;try to allocate and lock xms block
          pushad
          db PushWCode
          dw ROffAllocXMSBlock, DGROUP16
          call DosPCall
          popad
        _endif
        mov edx, OffFreeXMSCount
        RRT
        cmp dword [edx], 0
        je FailAlloc@8
        dec dword [edx]
        inc dword [edx + 4]  ;FirstFreeXMS
        mov edx, [edx + 4]
        dec edx
        shl edx, 12               ;convert pages to bytes
Success@8:
        stc
        mov dl, 67h               ;set default access rights
FailAlloc@8: ;carry flag is clear on error
        ret



        ESEG Text

;page allocated via VCPI
;page locked
