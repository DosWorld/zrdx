;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; Second half of realinit.asm (split by size, task 150):
; it is included at the end of realinit.asm.

; ----------------------------- Fill  client IDT
; ------------------------------
        mov di, ROffClientIDT + PSP
        mov dx, OffDefIntTrap3
        _do
        mov [di], dx
        mov word [di + 4], Trap3Selector
        mov [di + ROffIDT - ROffClientIDT + 0], dx
        mov word [di + ROffIDT - ROffClientIDT + 2], \
                Trap3Selector
        mov dword [di + ROffIDT - ROffClientIDT + 4], (0E0h \
                + SS_GATE_TRAP3) << 8
        add di, 8
        add dx, 4
        dec cl
        _enddo jnz
; ------------------------ fill default exceptions table
; ---------------------
        mov cl, 16
        mov eax, OffDefaultExcTrap3
        mov di, ROffClientExc + PSP
        _do
        mov word [di + 4], Trap3Selector
        mov [di], eax
        add ax, 4
        add di, 8
        _enddo loop
; ------------------------ Init hardware interrupts
; ---------------------------
        mov ax, 0DE0Ah
        call VCPIRICall
        mov bh, cl
        ;mov HardwareIntMapR[PSP], bx
        mov esi, OffHIntHandlers & 0FFFFh
        ;mov dx, JmpShortCode + ((23*4) shl 8)
        mov ax, 8
        movzx di, bl
        call Init8Handlers
        mov al, 70h
        movzx di, bh
        call Init8Handlers
        xor di, di
        mov ax, di
        call Init8HandlersAbs
        call Init8HandlersAbs
; --------------------- generate code for real mode hooks
; ----------------
        mov di, OffFirstSwitchCode + PSP
        mov ax, ROffRMS_RMHandler - ROffFirstSwitchCode - 3
        mov cx, (MaxSystemSwitchCode + nMaxCallbacks * 4) / 4
        _do
        mov byte [di], NearCallCode
        mov [di + 1], ax
        add di, 4
        sub ax, 4
        _enddo loop
; ------- calculate top of dos memory area, may be used by dpmi
; host --------
        mov ah, 48h
        mov bx, -(1)
        int 21h
        mov ax, [ROffTransferBufferPSize + PSP]
        add ax, [ROffMemReserve + PSP]
;ax - total size to reserve
        mov cx, [2]       ;top of task memory
        cmp bx, ax
        _ifnot jae
          ; decrase top if no another memory for transfer buffer
          sub cx, ax
          _ifnot ja
            xor cx, cx
          _endif
        _endif
        push cx
;--------------------- hook autopassup interrupts ---------------
        mov di, ROffAutoPassupRJmps
        xor si, si
        mov cx, 100h
        _do
          bt word [ROffPassupIntMap + PSP], si
          _ifnot jnc
            shl si, 2
            cli
            mov eax, [fs:si]
            mov byte [cs:di], JmpFarCode
            mov [cs:di + 1], eax
            mov [fs:si + 2], cs
            mov [fs:si], di
            add di, 5
            sti
            shr si, 2
          _endif
          inc si
        _enddo loop
; ---------------------- install terminate handler
; --------------------------
        db 66h, 0B8h
        dw TerminateRHandler
        dw DGROUP16
        xchg [0Ah], eax
        mov [ROffOldInt22 + PSP], eax
        pop cx
        jmp CriticalInitVCPI@167

TryRAW@167:
        mov ah, 88h
        int 15h
        movzx ebp, ax
        or ax, ax
        _ifnot jz, near
        cli
        call IsA20Enabled
        _ifnot je
PS2@167:
        mov word [ROffXMC + PSP], ROffA20Port
        mov [ROffXMC + 2 + PSP], cs
        mov byte [ROffA20State + PSP], 5
        mov cx, 5000
        in al, 92h
        test al, 2
        jnz NotPS2@167
        or al, 2
        jmp $ + 2
        jmp $ + 2
        out 92h, al
        _do
        jmp $ + 2
        jmp $ + 2
        in al, 92h
        test al, 2
        _enddo loopz
        ;jz   @@NotPS2
        call IsA20Enabled
        je A20OK@167
NotPS2@167:
        mov word [ROffXMC + PSP], ROffA20KBC
        mov si, ROffA20EnableSq + PSP
        _do jmp
        movzx dx, al
        jmp $ + 2
        outsb
        _while
        xor cx, cx
        _do
        jmp $ + 2
        jmp $ + 2
        in al, 64h
        test al, 2
        _enddo loopnz
        jnz A20Err@167
        lodsb
        or al, al
        _enddo jnz
        mov dx, [gs:46Ch]    ;timer counter
        _do                   ;wait for A20 about 2 timer ticks
          call IsA20Enabled
          je A20OK@167
          sti
          mov ax, [gs:46Ch]
          sub ax, dx
          cli
          cmp ax, 2
        _enddo jb
A20Err@167:
        sti
        xor ebp, ebp
        jmp RawPatch@167
        _endif
A20OK@167:
        sti
        mov di, 1
        ; default value for top of the extended memory
        mov edx, 100000h
        call CheckVDisk
        _ifnot jnz
          mov dx, [es:di + 2Eh - 1] ;vdisk top in k
          shl edx, 10          ;convert to bytes
        _endif
        les di, [gs:di - 1 + (19h * 4)]
        ;add  di, 12h
        call CheckVDisk
        _ifnot jnz
          mov eax, [es:di + 2Ch]
          and eax, 0FFFFFFh
          cmp eax, edx
          _ifnot jbe
            xchg eax, edx
          _endif
        _endif
        _endif
;ebp - size of mem window, <= 0FFFFh
;edx - top of mem window
        mov eax, [ROffMaxXMSAllocate + PSP]
        ; shr  eax, 10                        ;convert to
        ; kilobytes
        cmp eax, ebp
        _ifnot jb
          ; now hi word of eax is undefined!
          mov eax, ebp
        _endif
        sub bp, ax                         ;bp = leaved_mem_size
        ; move allocation size to ebp
        xchg ax, bp
        ; set new extended size, returned by my int 15 handler
        mov [ROffPatchPoint_int15 + 1 + PSP], ax
        shl eax, 10
        ; shift my memory base to bottom of the window
        add edx, eax
RawPatch@167:
        ; mov eax, ??????
        mov word [PatchPoint7 + PSP], 0B866h
        mov bx, ROffFreeXMSCount + PSP
        call TranslateMemLimits
        jmp InitRX
CheckVDisk:
        cmp dword [es:di + 12h], 'VDIS'
        _ifnot jne
          cmp byte [es:di + 12h + 4], 'K'
        _endif
        ret
        LLabel A20EnableSq
        db 64h, 0D1h, 60h, 0DFh, 64h, 0FFh, 0

IsA20Enabled:
        xor bx, bx
        mov gs, bx
        dec bx
        mov es, bx
        inc bx
        mov al, [es:bx + 10h]
        mov ah, al
        inc al
        xchg al, [gs:bx]
        cmp ah, [es:bx + 10h]
        mov [gs:bx], al
        ret


;al - unmapped real mode interrup number
;di - mapped interrupt number
Init8Handlers:
        test di, 111b
        jne near HIntMapError@169
        push di
        shr di, 3
        mov byte [di + ROffPassupIntPMap + PSP], 0FFh
        pop di
        cmp di, 18h
        _ifnot jae
          cmp di, 8
          jne HIntMapError@169
          ret
        _endif
Init8HandlersAbs:
        _do
        mov byte [si + ROffHIntHandlers + PSP \
                - (OffHIntHandlers & 0FFFFh)], PushBCode
        mov [si + ROffHIntHandlers + PSP - (OffHIntHandlers \
                & 0FFFFh) + 1], al

        ; only interrupt
        mov ecx, (OffInterruptH & 0FFFFh) - 7
        cmp di, 10h
        _ifnot jae
          mov cx, (OffException0DHandler & 0FFFFh) - 7
          cmp di, 0Dh
          _toendif je
          mov cx, (OffExceptionOrInterruptH & 0FFFFh) - 7
          bt word [ROffPassupIntPMap + PSP], di
          _ifnot jc
            mov cx, (OffExceptionWithCodeH & 0FFFFh) - 7
            cmp di, 8
            _ifnot jae
              cmp di, 2
              je NotExc@169
              mov cx, (OffExceptionWOCodeH & 0FFFFh) - 7
            _endif
          _endif
        _endif
        mov byte [si + ROffHIntHandlers + PSP \
                - (OffHIntHandlers & 0FFFFh) + 2], JmpNearCode
        sub ecx, esi
        mov [si + ROffHIntHandlers + PSP - (OffHIntHandlers \
                & 0FFFFh) + 3], ecx
        shl di, 2
        ; set new redirected real mode number
        mov [di + ROffFirstTrap3 + PSP + 3], al
        shl di, 1
        mov [di + ROffIDT + PSP], si
        mov word [di + ROffIDT + PSP + 2], Code0Selector
        mov word [di + ROffIDT + PSP + 4], (0E0h \
                + SS_GATE_INT3) << 8
        mov word [di + ROffIDT + PSP + 6], (OffHIntHandlers \
                >> 16)
        add si, 7
        shr di, 3
NotExc@169:
        inc di
        inc ax
        test di, 111b
        _enddo jnz
        ret
HIntMapError@169:
        mov si, ROffIntMapEM
        jmp RInitError


        ESEG IText16
