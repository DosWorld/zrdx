%assign NameFull -(6)
%assign NameWOExt -(4)
%assign NamePath -(2)
%assign sf_w 2
%assign sf_r 4
%assign sf_c 8
%assign sf_y 16
%assign sf_n 32
%assign header_size 200h
;             This file is part of the ZRDX 0.51OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

bits 16
org 100h

%include "autolbl.inc"

section .text
        jmp start

section .data
ParamBase: dw SrcName

le_found: db 0
StubLeName: db 'stub-le.exe', 0
StubPeName: db 'stub-pe.exe', 0
StubRdfName: db 'stub-rdf.exe', 0
SuccMsg: db 'Binding complete', 13, 10, 0
UserTermMsg: db 13, 10, 'Terminated by user answer', 0
CBMsg: db 13, 10, 'Terminated by Control-Break', 0
NotAccessMsg: db 'File "', 1, '" is not accessable.', 0
OverwriteMsg: db 'File "', 1, '" already exist, do you want to overwrite it?(y/n)', 0
OpenErrMsg: db 'Can', "'", 't open file "', 1, '"', 0
CreatErrMsg: db 'Can', "'", 't create file "', 1, '"', 0
TCreatErrMsg: db 'Can', "'", 't create temporary file', 0
TWriteErrMsg: db 'Can', "'", 't write temporary file', 1, 0
WriteErrMsg: db 'Can', "'", 't write file "', 1, '"', 1, 0
TRenErrMsg: db 'Can', "'", 't rename temporary file to "', 1, '"', 0
RenErrMsg: db 'Can', "'", 't rename "', 1, '" to "', 1, '"', 0
DelErrMsg: db 'Can', "'", 't delete file "', 1, '"', 0
ReadErrMsg: db 'Can', "'", 't read file "', 1, '"', 0
BadStubMsg: db 'Invalid stub file "', 1, '"', 0
BadHeadErrMsg: db 'Bad or unsupported header in "', 1, '"', 0
DiskFullMsg: db ', disk full ?'
EmptyMsg: db 0
HelpMsg: db "Usage:", 13, 10
db "  zrxbind -r [-y][-n] <target file> [backup file]", 13, 10
db "     replace stub in the target file and make a backup copy to backup file", 13, 10
db "     (target name with .bak extention is default)", 13, 10
db "     -n option: don't make backup", 13, 10
db "     -y option: assume yes on all questions(never prompt for overwrite)", 13, 10
db "  zrxbind [-c] [-y] <old file> <new file>", 13, 10
db "     copy <old file> with new stub to <new file>", 13, 10
db "  zrxbind -w [-y] [target file]", 13, 10
db "     write zrdx stub to the target file(zrdx.exe is default)"
CrLfMsg: db 13, 10, 0
YesMsg: db 'Yes', 13, 10, 0
section .text
;dx - pointer to file name
CheckForOverwrite:
        push ax
        push si
        mov al, 2
        call DosOpen
        _ifnot jnc
          cmp ax, 2         ;file not found ?
          je Exit@1
          mov si, NotAccessMsg
          jmp ErrorF2
        _endif
        xchg ax, bx
        mov ah, 3Eh
        int 21h           ;close
        test byte [MSwitchFlags], sf_y
        _ifnot jnz
          push dx
          mov si, OverwriteMsg
          call DispMsg
          _do
            mov ah, 8
            int 21h
            cmp al, 'n'
            _ifnot jne
Term@1:
              mov si, UserTermMsg
              jmp ErrorF1
            _endif
            cmp al, 1bh
            je Term@1
            cmp al, 'y'
          _enddo jne
          mov si, YesMsg
          call DispMsg
          pop si
Exit@1:
        _endif
        pop si
        pop ax
        ret


DispMsg:
        push ax
        push cx
        push dx
        push di
        push bp
        mov bp, sp
        add bp, 5 * 2
        mov di, disp_buffer
        mov dx, di
        _do jmp
        stosb
        _while
        lodsb
        cmp al, 1
        _ifnot jne
        push si
        inc bp
        inc bp
        mov si, [bp]
        _do jmp
          stosb
        _while
          lodsb
          or al, al
        _enddo jnz
        pop si
        _towhile jmp
        _endif
        or al, al
        _enddo jnz
        mov cx, di
        sub cx, dx
        mov bx, 1             ;stdout
        mov ah, 40h           ;write
        int 21h
        pop bp
        pop di
        pop dx
        pop cx
        pop ax
        ret


charclass:
%assign _b 090h
%assign _e 0C0h
%assign _s 0D0h
%assign _c 1
section .data
cclass_table: db 0, 0FFh, ' ', 9, '-', '/', 13
cclass_table_end:
cclass_table1: db _b, _b, _b, _b, _s, _s, _e, _c
section .text
        push bx
        mov cx, cclass_table_end - cclass_table + 1
        mov bx, cclass_table - 1
        _do
        inc bx
        cmp al, [ds:bx]
        _enddo loopnz
        cmp byte [bx + cclass_table1 - cclass_table], 0C0h
        pop bx
        ret

        ;less  - blank
        ;equal - end of line
        ;greate and above - switch
        ;below and greate - character

;NameFull  = -6
;NameWOExt = -4
;NamePath  = -2
set_default_ext:
%define _T cx
        mov _T, [di + NameWOExt]
        cmp _T, [di + NameFull]
        _ifnot jne
          add di, _T
          movsw
          movsw
        _endif
        ret

section .text
Int23Handler:
        push cs
        pop ds
        push cs
        pop es
        mov si, CBMsg
        jmp ErrorF
start:
        mov sp, StackTop
        mov si, 81h
        xor ax, ax
        mov cx, (BSSEnd - BSSStart) / 2
        mov di, BSSStart
        cld
        rep stosw            ;initialize BSS to zero
        mov ah, 9
section .data
IntroMsg: db 'Zurenava DOS extender bind utility ver.0.51OSE. (C) 1998-1999, Sergey Belyakov', 13, 10, '(C) 2026, Viacheslav Komenda', 13, 10, '$'
CfgTrailer: db 'ZRXC'
        dw 0
        dw 400h
        dd 0FFFFFFFFh
        dw 0F000h
CfgTrailerSize equ $ - CfgTrailer
section .text
        mov dx, IntroMsg
        int 21h
        mov dx, Int23Handler
        mov ax, 2523h
        int 21h
%define param_count dl
%define SwitchFlags dh
        xor dx, dx               ;clear SwitchFlags & ParamCount
        _do
          lodsb                  ;get next symbol from command line
L0@8:
        call charclass         ;check it class
        _enddo jl
        je end_param           ;if EOL
        ja do_switch           ;if '-' or '/'
        cmp param_count, 2      ;more then 2 parameters are not allowed
        ja TooManyParms
        inc param_count
        mov di, [ParamBase]  ;get address of parameter buffer
        add word [ParamBase], 140 ;shift pointer to next parameter
        mov bx, di
        mov [bx + NamePath], di   ;inital size of the path
        mov [bx + NameWOExt], di  ;inital size of the name without extension
        cmp byte [si], ':'  ;parameter has a drive in pathname?
        _ifnot jne
          add word [bx + NamePath], 2
          stosb
          movsb
          lodsb
        _endif
        _do
          cmp al, '.'
          _ifnot jne
            mov [bx + NameWOExt], di
          _endif
          stosb
          cmp al, '\'
          _ifnot jne
            mov [bx + NamePath], di
          _endif
          lodsb
          cmp al, '/'
          _break je
          call charclass
        _enddo jg
        sub di, bx         ;calculate and store full size of the name
        mov [bx + NameFull], di
        sub [bx + NamePath], bx     ;convert offset to size
        sub [bx + NameWOExt], bx    ;---- // ------
        jnz L0@8                  ;ext not defined ?
        mov [bx + NameWOExt], di    ;set to total size
        jmp L0@8
do_switch:
        lodsb
        call charclass
        je ParamErr
do_switch1:
        mov cx, 5
section .data
SwitchTable: db 'wrcyn'
section .text
        mov bx, SwitchTable - 1
        mov ah, 1h
        _do
          shl ah, 1
          inc bx
          cmp al, [bx]
        _enddo loopnz
        jne ParamErr
        test SwitchFlags, ah    ;check for double switch
        jnz ParamErr           ;error if this
        or SwitchFlags, ah    ;set switch flag
        lodsb
        call charclass
        jle L0@8
        jb do_switch1
        jmp L0@8
ParamErr:
TooManyParms:
        mov si, HelpMsg
        jmp ErrorF1
end_param:
        test SwitchFlags, sf_w
        _ifnot je
        test SwitchFlags, sf_c | sf_r | sf_n
        jne ParamErr
        or param_count, param_count
        xchg ax, dx
        mov di, SrcName
        mov dx, di
        _ifnot jne
section .data
ZrdxTpl:
db 'zrdx'
ExeExtTpl: db '.exe'
BakExtTpl: db '.bak'
TempNameTpl: db '!z$bind!.tmp'
section .text
          mov si, ZrdxTpl
          movsw
          movsw
          movsw
          movsw
        _else jmp
          mov si, ExeExtTpl
          call set_default_ext
        _endif
        test ah, sf_n
        _ifnot jnz
          call CheckForOverwrite
        _endif
        mov si, CreatErrMsg
        call DosCreat
        xchg bx, ax
        call write_le_stub
        mov ax, 4C00h
        int 21h
        _endif
        or param_count, param_count
        jz ParamErr
        mov byte [MSwitchFlags], SwitchFlags
        mov di, SrcName
        mov si, ExeExtTpl
        call set_default_ext
        test SwitchFlags, sf_r
        _ifnot jz                     ;replace operation
          test SwitchFlags, sf_c
          jnz ParamErr
          cmp param_count, 2
          _ifnot jae
            mov di, DestName - 6
            mov si, SrcName - 4
            mov cx, 128 + 2
            lodsw
            stosw
            stosw
            rep movsb                ;copy fp[0] to fp[1] without extention len
          _else jmp
            test SwitchFlags, sf_n
ParamErr1:
            jnz ParamErr
          _endif
          mov di, DestName
          mov si, BakExtTpl
          call set_default_ext         ;.BAK to destination
          mov si, SrcName        ;get temporary directory from fp[0]
          test SwitchFlags, sf_n
          mov al, 2h
          jz OvrCheck@8
        _else jmp
          ;copy operation
          cmp param_count, 2
          jb ParamErr1             ;b and nz
          test SwitchFlags, sf_n
          jnz ParamErr1
          mov di, DestName    ;get temporary directory from fp[1]
          push di
          mov si, ExeExtTpl
          call set_default_ext
          pop si
          mov al, 40h
OvrCheck@8:
          mov dx, DestName
          call CheckForOverwrite
        _endif
Exit:
        mov dx, SrcName
        call DosOpen
        _ifnot jnc
          mov si, OpenErrMsg
          jmp ErrorF2
        _endif
        mov [in_handle], ax
        push si
        xchg bx, ax
        mov ax, 4202h
        mov cx, 0FFFFh
        mov dx, -CfgTrailerSize
        int 21h
        _ifnot jc
          mov ah, 3Fh
          mov cx, CfgTrailerSize
          mov dx, buffer
          int 21h
          _ifnot jc
            cmp ax, CfgTrailerSize
            _ifnot jne
              cmp word [buffer], 'ZR'
              _ifnot jne
                cmp word [buffer + 2], 'XC'
                _ifnot jne
                  mov byte [had_trailer], 1
                  mov si, buffer + 4
                  mov di, CfgTrailer + 4
                  mov cx, (CfgTrailerSize - 4) / 2
                  rep movsw
                _endif
              _endif
            _endif
          _endif
        _endif
        pop si
        mov di, TempName
        mov dx, di
        mov cx, [si + NamePath]
        rep movsb                          ;copy path only from dest name or srcname
        mov si, TempNameTpl
        mov cl, 12
        rep movsb                          ;append fixed temporary name
        mov si, TCreatErrMsg
        call DosCreat                       ;create temporary file
        mov [out_handle], ax
%define _newheader_pos_h si
%define _newheader_pos_l di
%define _msize_l ax
%define _msize_h cx
%define _t_l bx
%define _t_h dx
        xor _msize_l, _msize_l
        xor _msize_h, _msize_h
        xor _newheader_pos_h, _newheader_pos_h
        xor _newheader_pos_l, _newheader_pos_l
main_cicle:
        _do
        mov word [xmsize_l], _msize_l
        mov word [xmsize_h], _msize_h
        call seek_to_newheader
        mov cx, header_size
        mov dx, buffer
        call DosRead
        cmp ax, 0Eh
        jb BadHeader
        mov ax, [buffer + 0]  ;signature
        cmp ax, 'MZ'
        _ifnot jne
          mov _msize_l, word [buffer + 4]
          mov _msize_h, 8000h >> (9 - 1)
          _do
          shl _msize_l, 1
          rcl _msize_h, 1
          _enddo jnc
          mov _t_l, word [buffer + 2]
          neg _t_l
          and _t_l, 1FFh
          sub _msize_l, _t_l
          sbb _msize_h, 0
          cmp word [buffer + 24], 40h
          _ifnot jne
            cmp _msize_h, word [buffer + 40h - 2]
            _ifnot jb
              _toendif ja
              cmp _msize_l, word [buffer + 40h - 4]
              _toendif ja
            _else
              mov _msize_h, word [buffer + 40h - 2]
              mov _msize_l, word [buffer + 40h - 4]
            _endif
          _endif
          _toendif jmp
        _else
          cmp ax, 'BW'
        _toelse jne
          mov _msize_l, word [buffer + 32 + 0]
          mov _msize_h, word [buffer + 32 + 2]
          mov _t_l, _msize_l
          mov _t_h, _msize_h
          add _t_l, _newheader_pos_l
          adc _t_h, _newheader_pos_h
          cmp _t_l, word [buffer + 28 + 0]
          jne BadHeader
          cmp _t_h, word [buffer + 28 + 2]
          jne BadHeader
          _toendif jmp
BadHeader:
          cmp byte [le_found], 0
          _ifnot je
            mov _newheader_pos_l, word [le_pos_l]
            mov _newheader_pos_h, word [le_pos_h]
            call seek_to_newheader
            mov bx, [out_handle]
            call write_le_stub
            call lread
            mov _t_h, word [stub_size_h]
            mov _t_l, word [stub_size_l]
            sub _t_l, word [msize_l]
            sbb _t_h, word [msize_h]
            add word [buffer + 80h + 0], _t_l
            adc word [buffer + 80h + 2], _t_h
            jmp copy_file
          _endif

          mov dx, SrcName
          mov si, BadHeadErrMsg
          jmp ErrorF2
        _else
          cmp ax, 'LE'
        _toelse jne
          mov _msize_l, word [buffer + 14h]     ;number of pages in image
          mov _msize_h, word [buffer + 14h + 2]
          mov _t_h, 12
          _do
            shl _msize_l, 1
            rcl _msize_h, 1
            dec _t_h
          _enddo jnz
          add _msize_l, word [buffer + 80h]
          adc _msize_h, word [buffer + 80h + 2]
          mov _t_l, word [buffer + 2Ch]
          neg _t_l
          and _t_l, 0FFFh
          sub _msize_l, _t_l
          sbb _msize_h, _t_h
          mov _t_l, word [xmsize_l]
          mov _t_h, word [xmsize_h]
          sub _msize_l, _t_l
          sbb _msize_h, _t_h
          cmp byte [le_found], 0
          _ifnot jne
            mov byte [le_found], 1
            mov word [le_pos_l], _newheader_pos_l
            mov word [le_pos_h], _newheader_pos_h
            mov word [msize_l], _t_l
            mov word [msize_h], _t_h
          _endif
          _toendif jmp
        _else
          cmp ax, 'PM'
        _toelse jne
          cmp word [buffer + 2], 'W1'
          _ifnot je
            jmp BadHeader
          _endif
          mov _msize_l, word [buffer + 20h]
          mov _msize_h, word [buffer + 20h + 2]
          add _msize_l, word [buffer + 2Ch]
          adc _msize_h, word [buffer + 2Ch + 2]
          add _msize_l, word [buffer + 44h]
          adc _msize_h, word [buffer + 44h + 2]
          add _msize_l, word [buffer + 4Ch]
          adc _msize_h, word [buffer + 4Ch + 2]
          _toendif jmp
        _else
          cmp ax, 'PE'
          _ifnot jne
            cmp word [buffer + 2], 0
            _ifnot je
              jmp BadHeader
            _endif
            sub _newheader_pos_l, word [xmsize_l]
            sbb _newheader_pos_h, word [xmsize_h]
            call seek_to_newheader
            mov bx, [out_handle]
            call write_pe_stub
            call lread
            jmp copy_file
          _endif
          cmp ax, 'RD'
          _ifnot jne
            cmp word [buffer + 2], 'OF'
            _ifnot je
              jmp BadHeader
            _endif
            cmp word [buffer + 4], 'F2'
            _ifnot je
              jmp BadHeader
            _endif
            call seek_to_newheader
            mov bx, [out_handle]
            call write_rdf_stub
            call lread
            jmp copy_file
          _endif
          jmp BadHeader
        _endif
        add _newheader_pos_l, _msize_l
        adc _newheader_pos_h, _msize_h
        _ifnot jno
          jmp BadHeader
        _endif
        _enddo jmp

        _do ;jmp
          call lwrite
          call lread
copy_file:
        _while
          cmp si, cx
        _enddo je
        call lwrite
        mov bx, [out_handle]
        cmp byte [had_trailer], 0
        _ifnot je
          mov ax, 4202h
          mov cx, 0FFFFh
          mov dx, -CfgTrailerSize
          int 21h
        _endif
        mov dx, CfgTrailer
        mov cx, CfgTrailerSize
        mov ax, TWriteErrMsg
        call DosWrite
        xor bx, bx
        xchg bx, [out_handle]
        mov ah, 3Eh
        int 21h
        mov bx, [in_handle]
        mov ah, 3Eh
        int 21h
        mov al, [MSwitchFlags]
        test al, sf_r
        mov dx, DestName
        _ifnot jnz
          call remove
          mov di, dx
          mov dx, TempName
          call rename
        _else jmp
          test byte [ds:MSwitchFlags], sf_n
          _ifnot jnz
            call remove
            mov di, dx
            mov dx, SrcName
            call rename
          _else jmp
            mov dx, SrcName
            call remove
          _endif
          mov di, dx
          mov dx, TempName
          call rename
        _endif
        mov si, SuccMsg
        call DispMsg
        mov ax, 4C00h
        int 21h

remove:
        call DosDelete
        _ifnot jnc
          cmp ax, 2
          _ifnot je
            mov si, DelErrMsg
            jmp ErrorF2
          _endif
        _endif
        ret

rename:
        call DosRename
        _ifnot jnc
          push di
          mov si, TRenErrMsg
          cmp dx, TempName
          _ifnot je
          ;error with name offset in dx
            mov si, RenErrMsg
ErrorF2:
            push dx
          _endif
          jmp ErrorF1
        _endif
        ret

seek_to_newheader:
        mov bx, [in_handle]
        mov cx, _newheader_pos_h
        mov dx, _newheader_pos_l
        mov al, 0
        mov ah, 42h
        int 21h
        ret
lread:
        mov bx, [in_handle]
        mov di, buffer
        mov cl, 4
        shr di, cl
        mov ax, es
        add di, ax
        _do
          mov ax, [ds:2]
          sub ax, di
          cmp ax, 200h
          jb ExitN@8
          cmp ax, 0F00h
          sbb si, si
          _ifnot jb
            mov ax, 0F00h
          _endif
          and ax, ~(1Fh)
          mov cl, 4
          shl ax, cl
          xchg ax, cx
          xor dx, dx
          mov ds, di
          call DosRead
          push es
          pop ds
          xchg si, ax
          _break jc
          mov dx, di
          add di, 0F00h
          cmp si, cx
          _break jne
          sahf
        _enddo jnc
ExitN@8:
        mov di, dx
        ret
;di:si - counter
lwrite:
        mov bx, [out_handle]
        mov bp, buffer
        mov cl, 4
        shr bp, cl
        mov ax, es
        add bp, ax
        _do jmp
          add bp, 0F00h
        _while
          mov cx, si
          cmp bp, di
          _ifnot je
            mov cx, 0F000h
          _endif
          mov ds, bp
          xor dx, dx
          mov ax, TWriteErrMsg
          call DosWrite
          cmp ax, cx
          _break jne
          cmp bp, di
        _enddo jne
        ret
write_le_stub:
        mov dx, StubLeName
        jmp short write_stub_file
write_pe_stub:
        mov dx, StubPeName
        jmp short write_stub_file
write_rdf_stub:
        mov dx, StubRdfName
write_stub_file:
        push si
        push di
        mov [stub_out], bx
        call build_stub_path
        xor al, al
        call DosOpen
        _ifnot jnc
          mov si, OpenErrMsg
          jmp ErrorF2
        _endif
        mov [stub_handle], ax
        xor ax, ax
        mov [stub_size_l], ax
        mov [stub_size_h], ax
        _do
          mov bx, [stub_handle]
          mov cx, 4000h
          mov dx, buffer
          mov ah, 3Fh
          int 21h
          _ifnot jnc
            mov si, ReadErrMsg
            mov dx, StubPath
            jmp ErrorF2
          _endif
          or ax, ax
          _break jz
          mov dx, [stub_size_l]
          or dx, [stub_size_h]
          _ifnot jnz
            cmp word [buffer], 'MZ'
            jne BadStub
          _endif
          add [stub_size_l], ax
          adc word [stub_size_h], 0
          mov cx, ax
          mov dx, buffer
          mov bx, [stub_out]
          call write_stub
        _enddo jmp
        mov ax, [stub_size_l]
        or ax, [stub_size_h]
        jz BadStub
        test word [stub_size_l], 1FFh
        jnz BadStub
        mov bx, [stub_handle]
        mov ah, 3Eh
        int 21h
        mov bx, [stub_out]
        pop di
        pop si
        ret

BadStub:
        mov si, BadStubMsg
        mov dx, StubPath
        jmp ErrorF2

build_stub_path:
        push ax
        push cx
        push si
        push di
        push es
        mov bx, dx
        mov es, [2Ch]
        xor di, di
        xor ax, ax
        mov cx, 0FFFFh
        cld
bsp_scan:
        repne scasb
        jne bsp_nopath
        cmp byte [es:di], 0
        jne bsp_scan
        add di, 3
        mov si, StubPath
        mov dx, si
bsp_copy:
        mov al, [es:di]
        inc di
        or al, al
        jz bsp_name
        mov [si], al
        inc si
        cmp al, '\'
        je bsp_slash
        cmp al, ':'
        jne bsp_copy
bsp_slash:
        mov dx, si
        jmp bsp_copy
bsp_nopath:
        mov dx, StubPath
bsp_name:
        mov si, dx
bsp_cname:
        mov al, [bx]
        inc bx
        mov [si], al
        inc si
        or al, al
        jnz bsp_cname
        mov dx, StubPath
        pop es
        pop di
        pop si
        pop cx
        pop ax
        ret

write_stub:
        mov ax, WriteErrMsg
DosWrite:
        push ax
        mov ah, 40h
        int 21h
        push es
        pop ds
        _ifnot jc
          cmp ax, cx
          _ifnot jne
            add sp, 2
            ret
          _endif
          mov bx, DiskFullMsg
        _else jmp
          mov bx, EmptyMsg
        _endif
        pop si
        cmp si, TWriteErrMsg
        mov ax, DestName
        _ifnot jne
          xchg ax, bx
        _endif
ErrorF:
FileError:
        push bx
        push ax
ErrorF1:
        call DispMsg
        mov bx, [out_handle]
        or bx, bx
        _ifnot je
          mov ah, 3Eh
          int 21h              ;close temp file
          mov dx, TempName
          call DosDelete               ;delete temp file
        _endif
        mov si, CrLfMsg
        call DispMsg
        mov ax, 4C01h
        int 21h

DosCreat:
        call DosMake
        _ifnot jc
        ret
        _endif
        xchg ax, dx
        jmp FileError


LfnLeave:
        pop dx
        pop bx
        pop cx
        pop si
        ret
LfnFail:
        cmp ah, 71h
        jne LfnFailExit
        mov byte [lfn_state], 2
        cmp al, al
LfnFailExit:
        ret

DosOpen:
        mov [tmp_mode], al
        push si
        push cx
        push bx
        push dx
        cmp byte [lfn_state], 2
        je dop_sfn
        mov si, dx
        xor bx, bx
        mov bl, al
        xor cx, cx
        mov dx, 1
        mov ax, 716Ch
        stc
        int 21h
        _ifnot jnc
          call LfnFail
          jne dop_end
          pop dx
          push dx
dop_sfn:
          mov al, [tmp_mode]
          mov ah, 3Dh
          int 21h
          jmp dop_end
        _endif
        mov byte [lfn_state], 1
dop_end:
        jmp LfnLeave

DosMake:
        push si
        push cx
        push bx
        push dx
        cmp byte [lfn_state], 2
        je dmk_sfn
        mov si, dx
        mov bx, 2
        mov cx, 20h
        mov dx, 12h
        mov ax, 716Ch
        stc
        int 21h
        _ifnot jnc
          call LfnFail
          jne dmk_end
          pop dx
          push dx
dmk_sfn:
          mov cx, 20h
          mov ah, 3Ch
          int 21h
          jmp dmk_end
        _endif
        mov byte [lfn_state], 1
dmk_end:
        jmp LfnLeave

DosDelete:
        push si
        push cx
        push bx
        push dx
        cmp byte [lfn_state], 2
        je ddl_sfn
        xor si, si
        xor cx, cx
        mov ax, 7141h
        stc
        int 21h
        _ifnot jnc
          call LfnFail
          jne ddl_end
          pop dx
          push dx
ddl_sfn:
          mov ah, 41h
          int 21h
          jmp ddl_end
        _endif
        mov byte [lfn_state], 1
ddl_end:
        jmp LfnLeave

DosRename:
        push si
        push cx
        push bx
        push dx
        cmp byte [lfn_state], 2
        je drn_sfn
        mov ax, 7156h
        stc
        int 21h
        _ifnot jnc
          call LfnFail
          jne drn_end
          pop dx
          push dx
drn_sfn:
          mov ah, 56h
          int 21h
          jmp drn_end
        _endif
        mov byte [lfn_state], 1
drn_end:
        jmp LfnLeave

DosRead:
        mov ah, 3Fh
        int 21h
        push es
        pop ds
        _ifnot jc
        ret
        _endif
        mov si, ReadErrMsg
        mov ax, SrcName
        jmp FileError

section .bss align=16
BSSStart:
in_handle: resw 1
out_handle: resw 1
stub_handle: resw 1
stub_out: resw 1
stub_size_l: resw 1
stub_size_h: resw 1
StubPath: resb 140
msize_l: resw 1
msize_h: resw 1
xmsize_l: resw 1
xmsize_h: resw 1
le_pos_l: resw 1
le_pos_h: resw 1
        resw 1
        resw 1
        resw 1
SrcName: resb 140
DestName: resb 140
TempName: resb 140
disp_buffer: resb 200
MSwitchFlags: resb 1
had_trailer: resb 1
lfn_state: resb 1
tmp_mode: resb 1
parameter: resb 80h * 2
BSSEnd:
        resb 400h
StackTop:
        alignb 16
buffer: resb 4000h
