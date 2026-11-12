;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; Second half of extender.asm (split by size, task 150):
; it is included at the end of extender.asm.

; --------------------------- interrupt vectors get/set
; ------------------------
w_vect:
        mov al, [ebp + DC_EAX]
        movzx ecx, word [ebp + DC_DS0]
        or al, al
        _ifnot jne
          mov [OffClientInt0Vector + 4], ecx
          mov [OffClientInt0Vector + 0], edx
          ret
        _endif
DPMIVectCall:
        xchg eax, ebx
        and byte [ebp + DC_Flags], ~(1)  ;clear carry
        int 31h
        _ifnot jnc
          or byte [ebp + DC_Flags], 1      ;set carry
        _endif
        ret
r_vect:
        mov al, [ebp + DC_EAX]
        or al, al
        _ifnot jne
          mov ecx, [OffClientInt0Vector + 4]
          mov edx, [OffClientInt0Vector + 0]
        _else jmp
          call DPMIVectCall
        _endif
        mov [ebp + DC_ES0], cx
        mov [ebp + DC_EBX], edx
        ret
; ----------------------------- get and set DTA
; ------------------------------
GetDTA:
        mov eax, [TaskDTA]
        mov [ebp + DC_EBX], eax
        mov eax, [TaskDTA + 4]
        mov [ebp + DC_ES0], ax
        ret
SetDTA:
        mov eax, [OffTransferDTA]
        mov [TaskDTA], edx
        mov [ebp + DC_EDX], eax
        mov eax, [OffTransferSegment]
        mov [ebp + DC_DS], ax
        mov eax, [ebp + DC_DS0]
        mov [TaskDTA + 4], ax
        call DosCall
        mov [ebp + DC_EDX], edx
        ret
; ---------------------------------- read and write
; ---------------------------
rw_file:
;Source pointer - esi, source length - SavedSize
;Length read - edx
;Old buffer pointer - ebp
ReadWriteFile:
_minreserved equ 1000h
_mincluster equ 512
%define _bytesleft edx
        mov eax, [OffTransferSegment]
        mov [ebp + DC_DS], ax
        mov eax, [OffTransferStack]
        push edx                ;save original edx
        push eax                ;save TransferStack
        mov esi, edx
        mov _bytesleft, ecx   ;DataPtr.DC_ECX
        mov edi, eax
        push _bytesleft
        sub eax, _minreserved     ;eax is signed now!
        cmp eax, _mincluster
        _ifnot jg
          mov eax, _mincluster
        _endif
        and eax, -(_mincluster)
        cmp eax, _bytesleft
        _ifnot jb
          mov eax, _bytesleft
        _endif
        sub edi, eax
        jb near TransferStackOverflow
        and edi, ~(3)
        push eax               ;@@buffersize
;        @@buffersize equ dword ptr ss:[ebp-20]
        mov [OffTransferStack], edi
        mov [ebp + DC_EDX], edi
        _do
        mov ecx, [ebp - 20]
        cmp ecx, _bytesleft
        _ifnot jb
          mov ecx, _bytesleft
        _endif
        mov [ebp + DC_ECX], ecx
        mov eax, ecx
        or bl, bl
        _ifnot je
          les edi, [OffTransferStack]
          mov fs, [ebp + DC_DS0]
          call MoveStr
        _endif
        mov [ebp + DC_EAX + 1], bh
        call DosCall
        jnz Err@153
        movzx ecx, word [ebp + DC_EAX]
        or bl, bl
        _ifnot jne
          push ecx
          mov edi, esi
          mov es, [ebp + DC_DS0]
          lfs esi, [OffTransferStack]
          call MoveStr
          mov esi, edi
          pop ecx
        _endif
        sub _bytesleft, ecx
        jbe Exit@153
        cmp eax, ecx
        _enddo jbe
Exit@153:
        pop eax      ;drop buffer size
        pop eax      ;@@read_rq
        mov esi, eax
        sub eax, _bytesleft
ret@153:
        mov [ebp + DC_EAX], eax
        mov [ebp + DC_ECX], esi    ;restore read_rq
        pop dword [OffTransferStack]
        pop dword [ebp + DC_EDX]
        ret
Err@153:
        pop eax        ;drop buffer size
        pop esi        ;read_rq
        mov eax, 4201h
        xchg [ebp + DC_EAX], ax
        sub _bytesleft, esi
        jz ret@153
        mov dword [ebp + DC_EDX], _bytesleft
        shr _bytesleft, 16
        mov dword [ebp + DC_ECX], _bytesleft
        call DosCall
        jmp ret@153
;es:edi - destination, fs:esi - source, ecx - num
MoveStr:
        push ecx
        shr ecx, 2
        rep fs movsd
        pop ecx
        and ecx, 3
        rep fs movsb
        ret


; ---------------------------------- exec
; ------------------------------------
DosExecute:
        cmp byte [ebp + DC_EAX], 0
        jne ToOldInt1
        call GetAsciizLenPush        ;push command name on ds:edx
        ; save last transfer stack and original edx
        push edx
        ; save last transfer stack and original edx
        push esi
        push gs
        add eax, (12h + 20 + 8) & (~(3))  ;shift TransferStack
        sub edx, eax
        jb near TransferStackOverflow@155 ;overflow
        mov [OffTransferStack], edx      ;reserve space for CB
        lea edi, [edx + 12h]
        mov [es:edx + 6], edi         ;offset of first fcb
        mov [es:edx + 10], edi         ;offset of second fcb
        mov al, 0
        mov cl, 20
        rep stosb
        mov gs, [ebp + DC_ES0]
        ; load pointer to PM parameter block
        mov ebx, [ebp + DC_EBX]
        lfs esi, [gs:ebx + 6]
        mov cl, [fs:esi]   ;load size of command line
        inc ecx
        inc ecx
        call PushStr@155
        mov [es:edx + 2], ax             ;store to cb it address
        lfs esi, [gs:ebx + 0]  ;load environment pointer
        xor ecx, ecx
        _do
        cmp word [fs:esi + ecx], 0
        _break je
        inc cx
        _loop jns
        ;too long or bad environment
        mov al, 10
        jmp RetErrorCode@155
        _enddo
        inc ecx
        inc ecx
        push ecx                    ;save environment size
        mov ax, 100h
        lea ebx, [ecx + 15]
        shr ebx, 4
        mov ecx, edx
        int 31h
        ; no DOS memory for environment
        jc TransferStackOverflow1@155
        ;shrd e
        ;call @@PushStr             ;move environment
        mov [es:ecx], ax           ;save it to CB

        ; shr  eax, 4                 ;convert offset to segment
        ; disp
        mov ax, [OffTransferSegment]   ;TransferSegment
        mov [ebp + DC_ES], ax
        ; set segment address of transfer stack
        mov [es:ecx + 4], ax
        ; set segment address of transfer stack
        mov [es:ecx + 8], ax
        ; set segment address of transfer stack
        mov [es:ecx + 12], ax
        mov eax, [ebp + DC_EBX]
        mov [ebp + DC_EBX], ecx
        mov es, edx
        pop ecx
        xor edi, edi
        rep fs movsb
        call DosCallErr
        mov [ebp + DC_EBX], eax
        mov ax, 101h
        int 31h                     ;free environment segment
        _ifnot jnc
          mov ebx, edx              ;if error
          mov ax, 0001h
          ; try to free environment selector
          int 31h
        _endif
Rest@155:
        pop gs                      ;original gs
        pop dword [ebp + DC_EDX]          ;original edx
        pop edx                     ;TransferStack
        ; restore only Transfer stack form edx
        jmp Rest1
TransferStackOverflow1@155:
        pop eax
TransferStackOverflow@155:
        mov al, 8                    ;insufficient memory
RetErrorCode@155:
        and eax, 7Fh
        mov [ebp + DC_EAX], eax      ;write error code
        or word [ebp + DC_Flags], 1      ;stc
        jmp Rest@155

PushStr@155:
        mov edi, [OffTransferStack]
        sub edi, ecx
        jb TransferStackOverflow1@155
        and edi, ~(0Fh)     ;align on paragraph
        mov [OffTransferStack], edi
        mov eax, edi
        rep fs movsb
        ret


; --------------------------- rename file
; ------------------------------------
;don't needed with short jumps
w_asciiz_dsdx_esdi: ;for rename file
        call GetAsciizLenPush
        push esi
        push edx
        ; word ptr DataPtr.DC_OffsetIndex, 0    ;es:edi internal
        ; index
        mov bx, p_esdi
        call w_asciiz_de
        pop dword [OffTransferStack]
        pop dword [ebp + DC_EDX]
        ret

; --------------------------- mouse set handler
; ------------------------------
MouseSetHandler:
        mov ax, 900h
        int 31h                      ;disable virtual interrupts
        ; save old virtual interrupts state
        push eax
        movzx eax, word [ebp + DC_ES0]
        mov ebx, edx
        mov esi, eax
        xchg [OffMouseHookProc + 0], ebx
        xchg [OffMouseHookProc + 4], esi
        mov edi, cs
        cmp di, ax
        mov ecx, [HookFromMouse32]
        _ifnot jne
          cmp edx, DefMouseHookProc
          je DosCall@157
        _endif
        or eax, edx
        db 0B9h
        LWord MouseCallBackX
        dw DGROUP16
        dw ROffMouseRHandlerEntry
        jne DosCall@157
        xor ecx, ecx
DosCall@157:
        mov [ebp + DC_ES], cx
        shr ecx, 16
        mov [ebp + DC_EDX], cx
        mov al, [ebp + DC_EAX + 0]
        call DosCall
        xchg [ebp + DC_EDX], edx
        cmp al, 14h            ;exchange function ?
        _ifnot jne
          mov [ebp + DC_EDX], ebx
          mov [ebp + DC_ES], si
          cmp ebx, DefMouseHookProc
          _ifnot jne
            mov edi, cs
            cmp di, si
            _ifnot jne
              shl edx, 16
              mov dx, [ebp + DC_ES]
              mov [HookFromMouse32], edx
            _endif
          _endif
        _endif
        pop eax
        ; restore old virtual interrupts state
        int 31h
        ret

DefMouseHookProc:
        retf
; ------------------------------ Mouse callback
; ------------------------------
;assume es:EGroup
MouseCallbackHandler:
        ;cld
        push es
        ;lodsd                           ;load return address
        pop ds
        ;mov  ax, TransferSegment
        ;add  ax, TransferBufferSize/16
        ;shl  eax, 16
        ;mov  ax, OffMouseCallbackPlace+4-OffMouseRHandler
        db 0C7h, 47h, DC_IP
        dw OffMouseCallbackPlace + 4 - OffMouseRHandler
;LWord MouseRHandlerSRef
        dw DGROUP16
        ;add  word ptr ds:[edi].DC_SP, 4           ;simulate retf
        ;check for nested callback
        ;shr  byte ptr ds:InMouseFlag, 1
        ; shr  byte ptr
        ; ds:[edi][InMouseFlag-MouseCallbackStruct], 1
        ; $ifnot jnc          ;ignore nested call(this is an
        ; unexpected event)
        ;mov  esi, edi
        ;add  edi, 34h
        push edi
        push es
        ;mov  ecx, 34h/4
        mov eax, ss
        mov [edi + OffMouseHandlerESP \
                - OffMouseCallbackStruct], esp
        ;rep  movs dword ptr es:[edi], dword ptr ds:[esi]
        lar eax, eax
        shr eax, 23     ;copy segment-32 bit to carry;  S32Bit
        _ifnot jc
        movzx esp, sp
        _endif
        movzx eax, word [edi + DC_EAX]
        movzx ebx, word [edi + DC_EBX]
        movsx ecx, word [edi + DC_ECX]
        movsx edx, word [edi + DC_EDX]
        movsx esi, word [edi + DC_ESI]
        movsx edi, word [edi + DC_EDI]
        mov es, [OffFlatSelector]
        push es
        pop ds
        ; for compatibility with buggy hookers
        pushfd
        call far [cs:OffMouseHookProc]
        ;mov ax, 900h
        ;int 31h
        ;cli        ;hooker may enable interrupts, but callback
                   ; must disable interrupts before setting
                   ; reenterancy flag
        mov esp, [cs:OffMouseHandlerESP]
        pop es
        pop edi
        ; mov  byte ptr
        ; es:[edi][InMouseFlag-MouseCallbackStruct1], 1
        ;mov  byte ptr es:InMouseFlag, 1
        ;$endif
        iretd
;assume  es:nothing

; --------------------- Macros for translation definitions
; -------------------
        SEGM EData
%assign FnIdx 1
%macro RegFnIndex 2
        zequ zcat3(%1,%2,Idx), FnIdx*2
        %assign FnIdx FnIdx+1
        dw %2
        dw %1 wrt EGroup
%endmacro

%assign ttpend 0
%assign ttpos 0
%assign ttmax 0
%define ttdef 0
%macro TransEntry 3
        times (%1) - ttpos db ttdef
        db zcat3(%2,%3,Idx)
        %assign ttpos (%1) + 1
%endmacro
%macro TransTableEnd 0
        times ttmax + 1 - ttpos db ttdef
        %assign ttpend 0
        %define ttdef 0
%endmacro
%macro DefTransTable 2
%if ttpend
        TransTableEnd
%endif
        %assign ttpend 1
        %assign ttmax %2
        %assign ttpos 0
%1:
        db ttmax
%endmacro
%macro TTTX 0
%endmacro


InternalIntNum equ 0


; ---------------------------indexes for translation
; tables-------------------
MethodTable0:
        RegFnIndex w_ascii$, p_dsdx
        RegFnIndex wr_pas, p_dsdx
        RegFnIndex SetDTA, p_dsdx
        RegFnIndex r_ptr, p_dsbx
        RegFnIndex r_ptr, p_esbx
        RegFnIndex w_vect, 205h
        RegFnIndex GetDTA, p_esdx
        RegFnIndex r_vect, 204h
        RegFnIndex w_asciiz_de, p_dsdx
        RegFnIndex w_asciizc, p_dsdx
        RegFnIndex w_asciizc_zc, p_dsdx
        RegFnIndex rw_file, 4001h
        RegFnIndex rw_file, 3f00h
      ;read or write file
;RegFnIndex wr_64,        p_dssi      ;get cur dir
        RegFnIndex allocmem, 100h        ;realloc mem
        RegFnIndex freemem, 101h        ;realloc mem
        RegFnIndex reallocmem, 102h        ;realloc mem
        RegFnIndex w_asciiz_r_dta, p_dsdx     ;find first
        RegFnIndex DTACall, 0           ;find next
        RegFnIndex w_asciiz_dsdx_esdi, p_dsdx ;rename file
        RegFnIndex r_createunic, p_dsdx      ;create unic
        RegFnIndex MouseSetHandler, p_esdx
        RegFnIndex DosExecute, p_dsdx      ;exec
        RegFnIndex ExitDPMI, 0
        RegFnIndex wr_64, p_esdx
        RegFnIndex wr_bx, p_esdx
;RegFnIndex GetExtVer,
        RegFnIndex wr_17, p_esdx
        RegFnIndex wr_cx3, p_esdx
        RegFnIndex r_psp, 0
        RegFnIndex wr_256, p_esdi
        RegFnIndex r_ptr_zc, p_esdi
        RegFnIndex VBE4F00, p_esdi
        RegFnIndex wr_ecx4, p_esdi
        RegFnIndex DosCallErr, 0
        RegFnIndex r_ptr_zcd, p_dsbx
        RegFnIndex onalnff_r_ptr_zcd, p_dsbx
        RegFnIndex onal0_r_ptr, p_dsbx
        RegFnIndex dc_zc, 0
        RegFnIndex onaxnff_zabcd, 0
        RegFnIndex r_country, p_dsdx
        RegFnIndex lseek_, 0
        RegFnIndex file_attrs, p_dsdx
        RegFnIndex wr_64_de, p_dssi
        RegFnIndex zd_de, 0
        RegFnIndex r_cx_za, p_dsdx
        RegFnIndex w_cx_za, p_dsdx
        RegFnIndex dc_za, 0
        RegFnIndex clip_plain, 0
        RegFnIndex clip_set, p_esbx
        RegFnIndex clip_get, p_esbx
        RegFnIndex lfn_name, 0
        RegFnIndex lfn_curdir, 0
        RegFnIndex lfn_find1, 0
        RegFnIndex lfn_find2, 0
        RegFnIndex lfn_rename, 0
        RegFnIndex lfn_true, 0
        RegFnIndex lfn_open, 0
        RegFnIndex lfn_vol, 0
        RegFnIndex lfn_info, 0
        RegFnIndex lfn_time, 0
        RegFnIndex lfn_short, 0
        RegFnIndex lfn_subst, 0

; ----------------------------translation
; tables------------------------------
;DefTransTable int21Table, 21h, 09h, 0FFh
        DefTransTable int21Table, 6Ch
;@@eee=1
        TransEntry 09h, w_ascii$, p_dsdx
        TransEntry 0Ah, wr_pas, p_dsdx
        TransEntry 1Ah, SetDTA, p_dsdx     ;set dta
        TransEntry 1Bh, r_ptr_zcd, p_dsbx  ;get FAT info
        ; get FAT info for spec drive
        TransEntry 1Ch, onalnff_r_ptr_zcd, p_dsbx
        TransEntry 1Fh, onal0_r_ptr, p_dsbx ;get DPB
        TransEntry 25h, w_vect, 205h       ;set int vector
        TransEntry 2Ah, dc_zc, 0           ;get system date
        TransEntry 2Fh, GetDTA, p_esdx
        TransEntry 32h, onal0_r_ptr, p_dsbx ;get DPB
        TransEntry 34h, r_ptr, p_esbx      ;get InDos flag
        TransEntry 35h, r_vect, 204h       ;get interrupt vector
        TransEntry 36h, onaxnff_zabcd, 0   ;get disk info
        TransEntry 38h, r_country, p_dsdx  ;get/set country code
        TransEntry 39h, w_asciiz_de, p_dsdx ;Create subdir
        TransEntry 3Ah, w_asciiz_de, p_dsdx ;remove subdir
        TransEntry 3Bh, w_asciizc, p_dsdx  ;set directory
        TransEntry 3Ch, w_asciizc, p_dsdx  ;create file
        TransEntry 3Dh, w_asciizc, p_dsdx  ;open file
        TransEntry 3Eh, DosCallErr, 0      ;close file
        TransEntry 3Fh, rw_file, 3f00h     ;read from file
        TransEntry 40h, rw_file, 4001h     ;write to file
        TransEntry 41h, w_asciiz_de, p_dsdx ;delete file
        TransEntry 42h, lseek_, 0          ;lseek
        TransEntry 43h, file_attrs, p_dsdx ;get or set file attr
        TransEntry 45h, dc_za, 0           ;dup file handle
        TransEntry 47h, wr_64_de, p_dssi   ;get cur dir
        TransEntry 48h, allocmem, 100h     ;alloc mem
        TransEntry 49h, freemem, 101h      ;free mem
        TransEntry 4Ah, reallocmem, 102h   ;realloc mem
        TransEntry 4Bh, DosExecute, p_dsdx ;exec
        TransEntry 4Ch, ExitDPMI, 0        ;exit
        TransEntry 4Eh, w_asciiz_r_dta, p_dsdx    ;find first
        TransEntry 4Fh, DTACall, 0                ;find next
;TransEntry 51h, r_psp, 0                  ;get PSP segment
        ; get list of list
        TransEntry 52h, r_ptr, p_esbx
        TransEntry 56h, w_asciiz_dsdx_esdi, p_dsdx ;rename file
; TransEntry 57h, xxx                      ;get last write
; date/time
        ; create unicue file
        TransEntry 5Ah, r_createunic, p_dsdx
        ; create new file
        TransEntry 5Bh, w_asciizc, p_dsdx
;TransEntry 5Ch,                          ;lock region
        ; Get PSP selector
        TransEntry 62h, r_psp, 0
        ; Set handle count
        TransEntry 67h, DosCallErr, 0
        ; Flush file handle
        TransEntry 68h, DosCallErr, 0
        ; Flush file handle
        TransEntry 6Ah, DosCallErr, 0
        ; open file extended
        TransEntry 6Ch, w_asciizc_zc, p_dsdx

;@@eee=0
        DefTransTable int2144Table, 0Fh
        TransEntry 0, zd_de, 0                ;get device info
        TransEntry 1, DosCallErr, 0           ;set device info
        TransEntry 2, r_cx_za, p_dsdx         ;char ioctl read
        TransEntry 3, w_cx_za, p_dsdx         ;char ioctl write
        TransEntry 4, r_cx_za, p_dsdx         ;block ioctl read
        TransEntry 5, w_cx_za, p_dsdx         ;block ioctl write
        TransEntry 6, DosCallErr, 0           ;get input status
        TransEntry 7, DosCallErr, 0           ;get output status
        ; is device removable?
        TransEntry 8, dc_za, 0
        TransEntry 9, zd_de, 0                ;is device remote?
        TransEntry 0Ah, zd_de, 0              ;is handle remote?
        ; set sharing retry count
        TransEntry 0Bh, DosCallErr, 0
        TransEntry 0Eh, DosCallErr, 0         ;logical drive map
        TransEntry 0Fh, DosCallErr, 0         ;-----
        TransTableEnd



        DefTransTable int2171Table, 0FFh
        %define ttdef clip_plain0Idx
        TransEntry 0Dh, clip_plain, 0
        TransEntry 39h, lfn_name, 0
        TransEntry 3Ah, lfn_name, 0
        TransEntry 3Bh, lfn_name, 0
        TransEntry 41h, lfn_name, 0
        TransEntry 43h, lfn_name, 0
        TransEntry 47h, lfn_curdir, 0
        TransEntry 4Eh, lfn_find1, 0
        TransEntry 4Fh, lfn_find2, 0
        TransEntry 56h, lfn_rename, 0
        TransEntry 60h, lfn_true, 0
        TransEntry 6Ch, lfn_open, 0
        TransEntry 0A0h, lfn_vol, 0
        TransEntry 0A1h, clip_plain, 0
        TransEntry 0A2h, lfn_find2, 0
        TransEntry 0A6h, lfn_info, 0
        TransEntry 0A7h, lfn_time, 0
        TransEntry 0A8h, lfn_short, 0
        TransEntry 0A9h, lfn_open, 0
        TransEntry 0AAh, lfn_subst, 0
        TransTableEnd

        DefTransTable int2F17Table, 0FFh
        %define ttdef clip_plain0Idx
        TransEntry 0, clip_plain, 0
        TransEntry 1, clip_plain, 0
        TransEntry 2, clip_plain, 0
        TransEntry 3, clip_set, p_esbx
        TransEntry 4, clip_plain, 0
        TransEntry 5, clip_get, p_esbx
        TransEntry 8, clip_plain, 0
        TransEntry 9, clip_plain, 0
        TransEntry 0Ah, clip_plain, 0
        TransTableEnd

        DefTransTable int33Table, 17h
        ; set graphics cursor
        TransEntry 09h, wr_64, p_esdx
        ; set mouse callback
        TransEntry 0Ch, MouseSetHandler, p_esdx
        ; exchange mouse callback
        TransEntry 14h, MouseSetHandler, p_esdx
        TransEntry 16h, wr_bx, p_esdx
        TransEntry 17h, wr_bx, p_esdx

        DefTransTable int1010Table, 17h
        TransEntry 2, wr_17, p_esdx   ;set all palette
        TransEntry 9, wr_17, p_esdx   ;get all palette
        TransEntry 12h, wr_cx3, p_esdx  ;set DAC block
        TransEntry 17h, wr_cx3, p_esdx  ;get DAC block

;DefTransTable int101030Table, 0
;TransEntry 0, r_ptr, p_bpdi     ;get font pointer

        DefTransTable int104FTable, 0Ah
        TransEntry 0, VBE4F00, p_esdi     ;get
        TransEntry 1, wr_256, p_esdi      ;get vmode info
        TransEntry 9, wr_ecx4, p_esdi     ;
        TransEntry 0Ah, r_ptr_zc, p_esdi
        TransTableEnd




; -------------------------- Exception handlers
; ------------------------------
TermMsg: db 13, 10
  db 'Unhandled exception #', 2, ', error code ', 4, ' at ', 4, \
          ':', 8, 13, 10
  db 'eax=', 8, ' ebx=', 8, ' ecx=', 8, ' edx=', 8, 13, 10
  db 'esp=', 8, ' ebp=', 8, ' esi=', 8, ' edi=', 8, 13, 10
  db 'eflags=', 8, '  unrelocated eip=', 8, 13, 10, 0
Term1Msg: db '=', 4, ' base=', 8, ' limit=', 8, ' acc=', 4, 13, \
        10, 0
Term2Msg: db 'cs', 0, 'ds', 0, 'es', 0, 'ss', 0, 'fs', 0, 'gs', 0
TermStkcMsg: db '[ss:esp]: ', 8, ' ', 8, ' ', 8, ' ', 8, ' ', \
        8, ' ', 8, ' ', 8, ' ', 13, 10, 0
TermInvSelMsg: db '=', 4, ' invalid selector', 13, 10, 0
TermEmpSelMsg: db '=', 4, ' null selector', 13, 10, 0
        LByte ZeroDivMsg
  db 'ZRDX runtime error: divide overflow', 13, 10, '$'
        LByte TSOverflowMsg
  db 'ZRDX runtime error: transfer buffer overflow', 13, 10, '$'
        ESEG EData
; ---------------------------exception 0
; handler------------------------------
        LLabel Exc0handler
        push eax
        push edi
        push es
%assign F 3 * 4
        mov edi, [cs:OffClientInt0Vector + 4]
        mov eax, cs
        cmp ax, di
        mov eax, [cs:OffClientInt0Vector + 0]
        _ifnot jne
        cmp eax, OffDefaultInt0Handler
        je GoDefaultExcHandler
        _endif
        xchg [esp + F + EXC_CS], edi
        xchg [esp + F + EXC_EIP], eax
        push edi
        sub dword [esp + F + 4 + EXC_ESP], 3 * 4
        les edi, [esp + F + 4 + EXC_ESP]
        cld
        stosd
        pop eax
        stosd
        mov eax, [esp + F + EXC_EFlags]
        stosd
        pop es
        pop edi
        pop eax
        LLabel Exc3Handler
        retf
;LLabel Exc1Handler
;        push 1
;        jmp  GlobalExceptionEntry
;        and  byte ptr [esp][1].EXC_EFlags, not 1    ;clear TF
;        retf
        ;exc 0 start here
; ------------------------entries for all
; exceptions--------------------------
ExceptionHandler:
%assign ExcptNum 0
%rep 16
%if (ExcptNum > 7) || (ExcptNum = 6)
            push ExcptNum
            jmp short GlobalExceptionEntry
%endif
%assign ExcptNum ExcptNum + 1
%endrep
GoDefaultExcHandler:
        pop es
        pop edi
        pop eax
        push 0
        ;jmp GlobalExceptionEntry
; ------------------------- common exceptin handler
; --------------------------
GlobalExceptionEntry:
        push gs
        push fs
        push dword [esp + (3 * 4) + EXC_SS]     ;ss
        push es
        push ds
        ; reserve space for unrelocated eip
        push eax
        push dword [esp + (7 * 4) + EXC_EFlags]    ;eflags
        push edi
        push esi
        push ebp
        ; and use [ebp] later for dec size of code
        mov ebp, esp
%assign F 11 * 4
        push dword [ebp + F + EXC_ESP]         ;esp
        push edx
        push ecx
        push ebx
        push eax
        mov ax, 0 ;selector
        LLabel SelReference1
               ;ra:2, ec, rip, rf, rsp
        mov ds, eax

        ;push dword ptr [ebp+F].EXC_EIP ;eip
        mov eax, [ebp + F + EXC_EIP] ;eip
        push eax
        sub eax, [OffImageBaseAddr]
        mov [ebp + (4 * 4)], eax
        push dword [ebp + F + EXC_CS]        ;cs
        push dword [ebp + F + EXC_Errcode]   ;err code
        push dword [ebp + F - 4]             ;exception num
        ;movzx eax, OrigVideoMode  ;
        ;and  al, 7Fh
        ; int  10h                        ;clear screen and
        ; restore initial video mode
        ;move stack content to new location
        push ds
        or edx, -(1)
        mov [ebp + F + EXC_CS], cs
        mov eax, [OffExcReturnAddr]
        mov [ebp + F + EXC_EIP], eax
        ;mov  ss:[ebp+F].EXC_EIP, offset EGroup:ExcHandler2Entry
        cmp eax, ExcHandler2Entry
        _ifnot jne
        mov esi, esp
        mov es, [OffFlatSelector]     ;TransferSelector
        movzx edi, word [OffTransferSegment]
        shl edi, 4
        ; allocate space for stack in TransferBuffer
        add edi, 1000h - 200
        mov dword [OffTransferStack], 800h
        xor ecx, ecx
        ; number of dwords in exception state
        mov cl, 21
        cld
        mov [ebp + F + EXC_SS], es
        mov [ebp + F + EXC_ESP], edi
        pushfd
        ; replace client eflags with my own
        pop dword [ebp + F + EXC_EFlags]
        rep ss movsd
        ;mov  esp, esi
        _endif
        add esp, 21 * 4
        ; return to my secondary exception handler with my stack
        retf

ExcHandler2Entry:
        pop ds
        ; some dpmi hosts don't set eflags content correctly
        cld
        ; mov  ds, edx             ;reload my own ds, because
        ; some dpmi hosts
                                 ; destroy it during returning
                                 ; from exception
        mov esi, TermMsg
%ifndef Release
        mov ah, 0
        int 16h
        cmp al, 27
        je Term@159
%endif
        ;mov ax, 3
        ;int 10h
        call PrintFStr
        pop eax       ;num
        pop eax       ;err code
        pop edi       ;cs
        sub ecx, 7
        mov esi, [esp + (5 * 4)]     ;esp
        mov es, [esp + (13 * 4)]     ;ss
        mov dword [OffExcReturnAddr], ExcHandler3Entry
        _do
          mov edx, [es:esi]
ExcHandler3Entry:
          add esi, 4
          mov [esp + ecx*4 + (7 * 4)], edx
          inc ecx
        _enddo jnz
        mov esi, TermStkcMsg
        call PrintFStr
        add esp, 11 * 4

        push edi         ;push cs again
        mov ebp, 6
        mov edi, Term2Msg
        _do
        mov esi, edi
        add edi, 3
        call PrintFStr
        pop ebx      ;selector
        lar eax, ebx
        shr eax, 8
        push eax
        xor edx, edx
        xor ecx, ecx
        mov ax, 6
        int 31h
        shl ecx, 16
        mov cx, dx
        lsl edx, ebx
        mov esi, Term1Msg
        push edx
        push ecx
        push ebx
        _ifnot jz
          mov esi, TermInvSelMsg
          cmp bx, 3
          _ifnot ja
            mov esi, TermEmpSelMsg
          _endif
        _endif
        call PrintFStr
        add esp, 16
        dec ebp
        _enddo jnz
Term@159:
        mov ax, 4CFEh
        pushfd
        push cs
        call int21entry
        jmp $
CallInt10:
        pushfd
        call far [EID10 + EID_OldIntVect]
        ret
PrintFStr:
        pushad
        mov ebp, esp
        _do
        lodsb
        cmp al, 0
        _break je
        cmp al, 8
        _ifnot jbe
        mov ah, 0Eh
        mov bx, 07h
        ;int 10h
        call CallInt10
        _else jmp
        add ebp, 4
        push dword [ebp + 32]
        movzx eax, al
        push eax
        call PrintN
        _endif
        _enddo jmp
        popad
        ret

        LLabel Starter
        int 31h
%ifdef PEMAX
        ; Win32: ES = DS (not the PSP)
        push ds
        pop es
%endif
        mov eax, ebx
        mov esi, ebx
        mov edi, ebx
        retf
%ifndef Release
Print8:
        push dword [esp + 4]
        push 8
        call PrintN
        pushad
        mov ax, 0E00h + ' '
        call CallInt10
        ;int 10h
        popad
        ret 4

%endif
PrintN:
        push eax
        push ecx
;@@Digit EQU DWORD PTR ss:[esp+8].8
;@@N     EQU DWORD PTR ss:[esp+8].4
        mov eax, [esp + 8 + 8]
        mov cl, 8
        sub cl, [esp + 8 + 4]
        shl cl, 2
        rol eax, cl
        mov ecx, [esp + 8 + 4]
        _do
        rol eax, 4
        pushad
        and al, 1111b
        add al, '0'
        cmp al, '9'
        _ifnot jbe
        add al, 'A' - '9' - 1
        _endif
        mov ah, 0Eh
        mov bx, 07h
        call CallInt10
        ;int 10h
        popad
        _enddo loop
        pop ecx
        pop eax
        ret 8


int75handler: ;handler for numeric coprocessor interrupt
        push eax
        mov al, 0
        out 0F0h, al
        mov al, 20h
        out 0A0h, al
        out 020h, al
        pop eax
        int 2
        sti
        iretd
TransferStackOverflow:
        mov edx, OffTSOverflowMsg
        jmp FatalExtExit
        DPROC DefaultInt0Handler
        mov edx, OffZeroDivMsg
FatalExtExit:
        push cs
        pop ds
        mov ah, 9
        int 21h
        mov ax, 4CFFh
        int 21h


        ESEG EText
