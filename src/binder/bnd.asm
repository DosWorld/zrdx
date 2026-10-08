%assign PSP 100h
%assign NameFull -(6)
%assign NameWOExt -(4)
%assign NamePath -(2)
%assign sf_w 2
%assign sf_r 4
%assign sf_c 8
%assign sf_y 16
%assign sf_n 32
%assign header_size 200h
;             This file is part of the ZRDX 0.50OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda

%include "autolbl.inc"
;PSP = 100h
segment Text align=1 public class=TEXT use16
;        ends
segment Data align=2 public class=DATA use16
;        ends
segment StubSeg align=2 public class=DATA use16
;        ends
segment Stk align=2 stack class=STACK use16
resb 400h
;        ends
segment BSS align=2 public class=BSS use16
BSSStart:
;        ends
segment Buf align=16 public class=BSS use16
buffer: resb 04000h
;        ends
segment Buf1 align=16 public class=BSS use16
resb 0F000h - 4000h
;        ends
group DGROUP Text Data Stk BSS Buf StubSeg
;assume cs:DGROUP, ds:DGROUP, es:DGROUP
segment Data
ParamBase: dw SrcName + PSP
;        ends
segment StubSeg
;Stub    label byte
le_stub:
%include "stub0_le.asb"
le_stub_size equ $ - le_stub
;xe_stub temporary removed
xe_stub:
;        include stub0_xe.asb
xe_stub_size equ $ - xe_stub
;        ends
segment BSS
in_handle: resw 1
out_handle: resw 1
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
;      ends
segment Text
segment Data
le_found: db 0
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
segment Text
;dx - pointer to file name
CheckForOverwrite:
        push ax
        push si
        mov ax, 3D02h
        int 21h
        _ifnot jnc
          cmp ax, 2         ;file not found ?
          je Exit@1
          mov si, NotAccessMsg + PSP
          jmp ErrorF2
        _endif
        xchg ax, bx
        mov ah, 3Eh
        int 21h           ;close
        test byte [MSwitchFlags + PSP], sf_y
        _ifnot jnz
          push dx
          mov si, OverwriteMsg + PSP
          call DispMsg
          _do
            mov ah, 8
            int 21h
            cmp al, 'n'
            _ifnot jne
Term@1:
              mov si, UserTermMsg + PSP
              jmp ErrorF1
            _endif
            cmp al, 1bh
            je Term@1
            cmp al, 'y'
          _enddo jne
          mov si, YesMsg + PSP
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
        mov di, disp_buffer + PSP
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
segment Data
cclass_table: db 0, 0FFh, ' ', 9, '-', '/', 13
cclass_table_end:
cclass_table1: db _b, _b, _b, _b, _s, _s, _e, _c
segment Text
        push bx
        mov cx, cclass_table_end - cclass_table + 1
        mov bx, cclass_table - 1 + PSP
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

segment BSS
parameter: resb 80h * 2
segment Text
Int23Handler:
        push cs
        pop ds
        push cs
        pop es
        mov si, CBMsg + PSP
        jmp ErrorF
start:
..start:
        mov si, 81h
        xor ax, ax
        mov cx, (BSSEnd - BSSStart) / 2
        mov di, BSSStart + PSP
        cld
        rep stosw            ;initialize BSS to zero
        mov ah, 9
segment Data
IntroMsg: db 'Zurenava DOS extender bind utility ver.0.50OSE. (C) 1998-1999, Sergey Belyakov', 13, 10, '(C) 2026, Viacheslav Komenda', 13, 10, '$'
CfgTrailer: db 'ZRXC'
        dw 2
        dw 400h
        dd 0FFFFFFFFh
        dw 0F000h
CfgTrailerSize equ $ - CfgTrailer
segment Text
        mov dx, IntroMsg + PSP
        int 21h
        mov dx, Int23Handler + PSP
        mov ax, 2523h
        int 21h
        ;mov  di, offset ds:parameter+PSP
%define param_count dl
%define SwitchFlags dh
        ;xor param_count, param_count
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
        mov di, [ParamBase + PSP]  ;get address of parameter buffer
        add word [ParamBase + PSP], 140 ;shift pointer to next parameter
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
segment Data
SwitchTable: db 'wrcyn'
segment Text
;        sf_w equ 2
;        sf_r equ 4
;        sf_c equ 8
;        sf_y equ 16
;        sf_n equ 32
        mov bx, SwitchTable + PSP - 1
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
        mov si, HelpMsg + PSP
        jmp ErrorF1
end_param:
        test SwitchFlags, sf_w
        _ifnot je
        test SwitchFlags, sf_c | sf_r | sf_n
        jne ParamErr
        or param_count, param_count
        xchg ax, dx
        mov di, SrcName + PSP
        mov dx, di
        _ifnot jne
segment Data
ZrdxTpl:
db 'zrdx'
ExeExtTpl: db '.exe'
BakExtTpl: db '.bak'
TempNameTpl: db '!z$bind!.tmp'
segment Text
          mov si, ZrdxTpl + PSP
          movsw
          movsw
          movsw
          movsw
        _else jmp
          mov si, ExeExtTpl + PSP
          call set_default_ext
        _endif
        test ah, sf_n
        _ifnot jnz
          call CheckForOverwrite
        _endif
        mov si, CreatErrMsg + PSP
        call DosCreat
        xchg bx, ax
        call write_le_stub
        mov ax, 4C00h
        int 21h
        _endif
        or param_count, param_count
        jz ParamErr
        mov byte [MSwitchFlags + PSP], SwitchFlags
        mov di, SrcName + PSP
        mov si, ExeExtTpl + PSP
        call set_default_ext
        test SwitchFlags, sf_r
        _ifnot jz                     ;replace operation
          test SwitchFlags, sf_c
          jnz ParamErr
          cmp param_count, 2
          _ifnot jae
            mov di, DestName - 6 + PSP
            mov si, SrcName - 4 + PSP
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
          mov di, DestName + PSP
          mov si, BakExtTpl + PSP
          call set_default_ext         ;.BAK to destination
          mov si, SrcName + PSP        ;get temporary directory from fp[0]
          test SwitchFlags, sf_n
          mov al, 2h
          jz OvrCheck@8
        _else jmp
          ;copy operation
          cmp param_count, 2
          jb ParamErr1             ;b and nz
          test SwitchFlags, sf_n
          jnz ParamErr1
          mov di, DestName + PSP    ;get temporary directory from fp[1]
          push di
          mov si, ExeExtTpl + PSP
          call set_default_ext
          pop si
          mov al, 40h
OvrCheck@8:
          mov dx, DestName + PSP
          call CheckForOverwrite
        _endif
Exit:
        mov dx, SrcName + PSP
        mov ah, 3Dh   ;dos open, al defined below
        int 21h
        _ifnot jnc
          mov si, OpenErrMsg + PSP
          jmp ErrorF2
        _endif
        mov [in_handle + PSP], ax
        mov di, TempName + PSP
        mov dx, di
        mov cx, [si + NamePath]
        rep movsb                          ;copy path only from dest name or srcname
        mov si, TempNameTpl + PSP
        mov cl, 12
        rep movsb                          ;append fixed temporary name
        mov si, TCreatErrMsg + PSP
        call DosCreat                       ;create temporary file
        mov [out_handle + PSP], ax
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
        mov word [xmsize_l + PSP], _msize_l
        mov word [xmsize_h + PSP], _msize_h
        call seek_to_newheader
;        header_size equ 200h
        mov cx, header_size
        mov dx, buffer + PSP
        call DosRead
        cmp ax, 84h
        jb BadHeader
        mov ax, [buffer + 0 + PSP]  ;signature
        cmp ax, 'MZ'
        _ifnot jne
          mov _msize_l, word [buffer + 4 + PSP]
          mov _msize_h, 8000h >> (9 - 1)
          _do
          shl _msize_l, 1
          rcl _msize_h, 1
          _enddo jnc
          mov _t_l, word [buffer + 2 + PSP]
          neg _t_l
          and _t_l, 1FFh
          sub _msize_l, _t_l
          sbb _msize_h, 0
          cmp word [buffer + 24 + PSP], 40h
          _ifnot jne
            cmp _msize_h, word [buffer + 40h - 2 + PSP]
            _ifnot jb
              _toendif ja
              cmp _msize_l, word [buffer + 40h - 4 + PSP]
              _toendif ja
            _else
              mov _msize_h, word [buffer + 40h - 2 + PSP]
              mov _msize_l, word [buffer + 40h - 4 + PSP]
            _endif
          _endif
          _toendif jmp
        _else
          cmp ax, 'BW'
        _toelse jne
          mov _msize_l, word [buffer + 32 + 0 + PSP]
          mov _msize_h, word [buffer + 32 + 2 + PSP]
          mov _t_l, _msize_l
          mov _t_h, _msize_h
          add _t_l, _newheader_pos_l
          adc _t_h, _newheader_pos_h
          cmp _t_l, word [buffer + 28 + 0 + PSP]
          jne BadHeader
          cmp _t_h, word [buffer + 28 + 2 + PSP]
          jne BadHeader
          _toendif jmp
BadHeader:
          cmp byte [le_found + PSP], 0
          _ifnot je
            mov _newheader_pos_l, word [le_pos_l + PSP]
            mov _newheader_pos_h, word [le_pos_h + PSP]
            call seek_to_newheader
            mov bx, [out_handle + PSP]
            call write_le_stub
            call lread
            xor _t_h, _t_h
            mov _t_l, le_stub_size
            sub _t_l, word [msize_l + PSP]
            sbb _t_h, word [msize_h + PSP]
            add word [buffer + 80h + 0 + PSP], _t_l
            adc word [buffer + 80h + 2 + PSP], _t_h
            jmp copy_file
          _endif

          mov dx, SrcName + PSP
          mov si, BadHeadErrMsg + PSP
          jmp ErrorF2
        _else
          cmp ax, 'LE'
        _toelse jne
          mov _msize_l, word [buffer + PSP + 14h]     ;number of pages in image
          mov _msize_h, word [buffer + PSP + 14h + 2]
          mov _t_h, 12
          _do
            shl _msize_l, 1
            rcl _msize_h, 1
            dec _t_h
          _enddo jnz
          add _msize_l, word [buffer + PSP + 80h]
          adc _msize_h, word [buffer + PSP + 80h + 2]
          mov _t_l, word [buffer + PSP + 2Ch]
          neg _t_l
          and _t_l, 0FFFh
          sub _msize_l, _t_l
          sbb _msize_h, _t_h
          mov _t_l, word [xmsize_l + PSP]
          mov _t_h, word [xmsize_h + PSP]
          sub _msize_l, _t_l
          sbb _msize_h, _t_h
          cmp byte [le_found + PSP], 0
          _ifnot jne
            mov byte [le_found + PSP], 1
            mov word [le_pos_l + PSP], _newheader_pos_l
            mov word [le_pos_h + PSP], _newheader_pos_h
            mov word [msize_l + PSP], _t_l
            mov word [msize_h + PSP], _t_h
          _endif
          _toendif jmp
        _else
          cmp ax, 'PM'
        _toelse jne
          cmp word [buffer + PSP + 2], 'W1'
          _ifnot je
            jmp BadHeader
          _endif
          mov _msize_l, word [buffer + PSP + 20h]
          mov _msize_h, word [buffer + PSP + 20h + 2]
          add _msize_l, word [buffer + PSP + 2Ch]
          adc _msize_h, word [buffer + PSP + 2Ch + 2]
          add _msize_l, word [buffer + PSP + 44h]
          adc _msize_h, word [buffer + PSP + 44h + 2]
          add _msize_l, word [buffer + PSP + 4Ch]
          adc _msize_h, word [buffer + PSP + 4Ch + 2]
          ;jne  BadHeader
          ;cmp  @@msize_l, 5000
          ;ja   BadHeader
          _toendif jmp
        _else
          cmp ax, 'XE'
          ;jne  BadHeader
;xe format detection temporary removed
          ;$break je
          jmp BadHeader
        _endif
        ;mov  lheader_pos_l, @@newheader_pos_l
        ;mov  lheader_pos_h, @@newheader_pos_h
        ;cmp  le_found[PSP], 0
        ;$ifnot jne
        ;  mov  msize_l[PSP], @@msize_l
        ;  mov  msize_h[PSP], @@msize_h
        ;$endif
        add _newheader_pos_l, _msize_l
        adc _newheader_pos_h, _msize_h
        _ifnot jno
          jmp BadHeader
        _endif
        ;cmp  @@newheader_pos_h, file_size[2]
        ;ja   BadHeader
        ;$ifnot jne
        ;  cmp @@newheader_pos_l, file_size[0]
        ;  jae BadHeader
        ;$endif
        _enddo jmp

        ;LE:
        ;call seek_to_newheader
        ;mov  bx, out_handle[PSP]
        ;call write_stub
        ;call lread
        ;xor  @@t_h, @@t_h
        ;mov  @@t_l, StubSize
        ;sub  @@t_l, msize_l[PSP]
        ;sbb  @@t_h, msize_h[PSP]
        ;add  word ptr buffer[80h][0][PSP], @@t_l
        ;adc  word ptr buffer[80h][2][PSP], @@t_h
        call seek_to_newheader
        mov dx, xe_stub + PSP
        mov bx, [out_handle + PSP]
        mov cx, xe_stub_size
        call write_stub
        call lread
        jmp copy_file
        _do ;jmp
          call lwrite
          call lread
copy_file:
        _while
          cmp si, cx
        _enddo je
        call lwrite
        mov dx, CfgTrailer + PSP
        mov bx, [out_handle + PSP]
        mov cx, CfgTrailerSize
        mov ah, 40h
        int 21h
        xor bx, bx
        xchg bx, [out_handle + PSP]
        mov ah, 3Eh
        int 21h
        mov bx, [in_handle + PSP]
        mov ah, 3Eh
        int 21h
        mov al, [MSwitchFlags + PSP]
        test al, sf_r
        mov dx, DestName + PSP
        _ifnot jnz
          call remove
          mov di, dx
          mov dx, TempName + PSP
          call rename
        _else jmp
          test byte [ds:MSwitchFlags + PSP], sf_n
          ;test al, sf_n
          _ifnot jnz
            call remove
            mov di, dx
            mov dx, SrcName + PSP
            call rename
          _else jmp
            mov dx, SrcName + PSP
            call remove
          _endif
          mov di, dx
          mov dx, TempName + PSP
          call rename
        _endif
        mov si, SuccMsg + PSP
        call DispMsg
        mov ax, 4C00h
        int 21h

remove:
        mov ah, 41h
        int 21h
        _ifnot jnc
          cmp ax, 2
          _ifnot je
            ;push dx
            mov si, DelErrMsg + PSP
            jmp ErrorF2
          _endif
        _endif
        ret

rename:
        mov ah, 56h
        int 21h
        _ifnot jnc
          push di
          mov si, TRenErrMsg + PSP
          cmp dx, TempName + PSP
          _ifnot je
          ;error with name offset in dx
            mov si, RenErrMsg + PSP
ErrorF2:
            push dx
          _endif
          jmp ErrorF1
        _endif
        ret

seek_to_newheader:
        mov bx, [in_handle + PSP]
        mov cx, _newheader_pos_h
        mov dx, _newheader_pos_l
        mov al, 0
        ;call dos_seek
        mov ah, 42h
        int 21h
        ret
lread:
        mov bx, [in_handle + PSP]
        mov di, Buf
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
          ;int  21h
        ;call dos_read
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
        mov bx, [out_handle + PSP]
        mov bp, Buf
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
          mov ax, TWriteErrMsg + PSP
          call DosWrite
          cmp ax, cx
          _break jne
          ;call dos_write
          cmp bp, di
        _enddo jne
        ret
write_le_stub:
        mov cx, le_stub_size
        mov dx, le_stub + PSP
write_stub:
        mov ax, WriteErrMsg + PSP
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
          mov bx, DiskFullMsg + PSP
        _else jmp
          mov bx, EmptyMsg + PSP
        _endif
        pop si
        cmp si, TWriteErrMsg + PSP
        mov ax, DestName + PSP
        _ifnot jne
          xchg ax, bx
        _endif
ErrorF:
FileError:
        push bx
        push ax
ErrorF1:
        call DispMsg
        mov bx, [out_handle + PSP]
        or bx, bx
        _ifnot je
          mov ah, 3Eh
          int 21h              ;close temp file
          mov dx, TempName + PSP
          mov ah, 41h
          int 21h               ;delete temp file
        _endif
        mov si, CrLfMsg + PSP
        call DispMsg
        mov ax, 4C01h
        int 21h

DosCreat:
        mov cx, 20h
        mov ah, 3Ch
        int 21h
        _ifnot jc
        ret
        _endif
        ;mov  si, offset ds:CreatErrMsg+PSP
        xchg ax, dx
        jmp FileError


DosRead:
        mov ah, 3Fh
        int 21h
        push es
        pop ds
        _ifnot jc
        ret
        _endif
        mov si, ReadErrMsg + PSP
        mov ax, SrcName + PSP
        jmp FileError

;Text    ends
segment BSS
BSSEnd:
;        ends
;end start



