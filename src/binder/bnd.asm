;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

include autolbl.inc
PSP = 100h
Text    segment byte public 'TEXT'
        ends
Data    segment word public 'DATA'
        ends
StubSeg segment word public 'DATA'
        ends
Stk     segment word stack 'STACK'
        DB 400h dup(?)
        ends
BSS     segment word public 'BSS'
        BSSStart label byte
        ends
Buf     segment para public 'BSS'
buffer  DB 04000h dup(?)
        ends
Buf1    segment para public 'BSS'
        DB 0F000h-4000h dup(?)
        ends
DGROUP group  Text, Data, Stk, BSS, Buf, StubSeg
assume cs:DGROUP, ds:DGROUP, es:DGROUP
Data    Segment
ParamBase DW offset dgroup:SrcName+PSP
        ends
StubSeg Segment
;Stub    label byte
le_stub  label byte
        include stub0_le.asb
        le_stub_size = $-le_stub
;xe_stub temporary removed
xe_stub  label byte
;        include stub0_xe.asb
        xe_stub_size = $-xe_stub
        ends
BSS  Segment
in_handle  DW ?
out_handle DW ?
msize_l    DW ?
msize_h    DW ?
xmsize_l   DW ?
xmsize_h   DW ?
le_pos_l DW ?
le_pos_h DW ?
         DW ?, ?, ?
SrcName  DB 140 dup(?)
DestName DB 140 dup(?)
TempName DB 140 dup(?)
disp_buffer DB 200 dup(?)
MSwitchFlags DB ?
      ends
Text segment
Data    Segment
le_found   db 0
SuccMsg      DB 'Binding complete', 13, 10, 0
UserTermMsg  DB 13, 10, 'Terminated by user answer', 0
CBMsg        DB 13, 10, 'Terminated by Control-Break', 0
NotAccessMsg DB 'File "',1,'" is not accessable.', 0
OverwriteMsg DB 'File "',1,'" already exist, do you want to overwrite it?(y/n)', 0
OpenErrMsg   DB 'Can',"'",'t open file "',1,'"', 0
CreatErrMsg  DB 'Can',"'",'t create file "',1,'"', 0
TCreatErrMsg DB 'Can',"'",'t create temporary file' ,0
TWriteErrMsg DB 'Can',"'",'t write temporary file', 1, 0
WriteErrMsg  DB 'Can',"'",'t write file "', 1,'"', 1, 0
TRenErrMsg   DB 'Can',"'",'t rename temporary file to "', 1, '"', 0
RenErrMsg    DB 'Can',"'",'t rename "', 1, '" to "', 1, '"', 0
DelErrMsg    DB 'Can',"'",'t delete file "', 1, '"', 0
ReadErrMsg   DB 'Can',"'",'t read file "',1,'"', 0
BadHeadErrMsg DB 'Bad or unsupported header in "',1,'"', 0
DiskFullMsg  DB ', disk full ?'
EmptyMsg     DB 0
HelpMsg      DB "Usage:", 13, 10
DB "  zrxbind -r [-y][-n] <target file> [backup file]",13, 10
DB "     replace stub in the target file and make a backup copy to backup file",13, 10
DB "     (target name with .bak extention is default)",13, 10
DB "     -n option: don't make backup",13, 10
DB "     -y option: assume yes on all questions(never prompt for overwrite)",13, 10
DB "  zrxbind [-c] [-y] <old file> <new file>",13, 10
DB "     copy <old file> with new stub to <new file>",13, 10
DB "  zrxbind -w [-y] [target file]",13, 10
DB "     write zrdx stub to the target file(zrdx.exe is default)"
CrLfMsg DB 13, 10, 0
YesMsg  DB 'Yes', 13, 10, 0
        Ends
;dx - pointer to file name
CheckForOverwrite PROC
        push ax si
        mov  ax, 3D02h
        int  21h
        $ifnot jnc
          cmp  ax, 2         ;file not found ?
          je   @@Exit
          mov  si, offset ds:NotAccessMsg+PSP
          jmp  ErrorF2
        $endif
        xchg ax, bx
        mov  ah, 3Eh
        int  21h           ;close
        test MSwitchFlags[PSP], sf_y
        $ifnot jnz
          push dx
          mov  si, offset ds:OverwriteMsg+PSP
          call DispMsg
          $do
            mov  ah, 8
            int  21h
            cmp  al, 'n'
            $ifnot jne
              @@Term:
              mov si, offset ds:UserTermMsg+PSP
              jmp ErrorF1
            $endif
            cmp  al, 1bh
            je   @@Term
            cmp  al, 'y'
          $enddo jne
          mov  si, offset ds:YesMsg+PSP
          call DispMsg
          pop  si
          @@Exit:
        $endif
        pop  si ax
        ret
        ENDP

DispMsg PROC
        push ax cx dx di bp
        mov  bp, sp
        add  bp, 5*2
        mov  di, offset ds:disp_buffer+PSP
        mov  dx, di
        $do jmp
        stosb
        $while
        lodsb
        cmp  al, 1
        $ifnot jne
        push si
        inc  bp
        inc  bp
        mov  si, [bp]
        $do  jmp
          stosb
        $while
          lodsb
          or  al, al
        $enddo jnz
        pop  si
        $towhile jmp
        $endif
        or   al, al
        $enddo jnz
        mov  cx, di
        sub  cx, dx
        mov  bx, 1             ;stdout
        mov  ah, 40h           ;write
        int  21h
        pop  bp di dx cx ax
        ret
        ENDP

charclass PROC
@@b equ 090h
@@e equ 0C0h
@@s equ 0D0h
@@c equ 1
Data    segment
cclass_table DB  0, 0FFh,' ', 9, '-', '/', 13
cclass_table_end label byte
cclass_table1 DB @@b,@@b,@@b,@@b,@@s, @@s,@@e, @@c
Data    ends
        push bx
        mov  cx, cclass_table_end - cclass_table+1
        mov  bx, offset ds:cclass_table-1+PSP
        $do
        inc  bx
        cmp  al, ds:[bx]
        $enddo loopnz
        cmp  byte ptr [cclass_table1-cclass_table][bx], 0C0h
        pop  bx
        ret
        ENDP
        ;less  - blank
        ;equal - end of line
        ;greate and above - switch
        ;below and greate - character

NameFull  = -6
NameWOExt = -4
NamePath  = -2
set_default_ext PROC
        @@T equ cx
        mov @@T, [di].NameWOExt
        cmp @@T, [di].NameFull
        $ifnot jne
          add di, @@T
          movsw
          movsw
        $endif
        ret
        ENDP
BSS     segment
parameter DB 80h*2 dup(?)
        ends
Int23Handler:
        push cs
        pop  ds
        push cs
        pop  es
        mov  si, offset ds:CBMsg+PSP
        jmp  ErrorF
start:
        mov  si, 81h
        xor  ax, ax
        mov  cx, (BSSEnd-BSSStart)/2
        mov  di, offset ds:BSSStart+PSP
        cld
        rep  stosw            ;initialize BSS to zero
        mov  ah, 9
Data    segment
IntroMsg DB 'Zurenava DOS extender bind utility ver.0.50. (C) 1998-1999, Sergey Belyakov',13,10,'$'
        ends
        mov  dx, offset ds:IntroMsg+PSP
        int  21h
        mov  dx, offset cs:Int23Handler+PSP
        mov  ax, 2523h
        int  21h
        ;mov  di, offset ds:parameter+PSP
        param_count equ dl
        SwitchFlags equ dh
        ;xor param_count, param_count
        xor dx, dx               ;clear SwitchFlags & ParamCount
        $do
          lodsb                  ;get next symbol from command line
@@L0:     call charclass         ;check it class
        $enddo jl
        je   end_param           ;if EOL
        ja   do_switch           ;if '-' or '/'
        cmp  param_count, 2      ;more then 2 parameters are not allowed
        ja   TooManyParms
        inc  param_count
        mov  di, ParamBase[PSP]  ;get address of parameter buffer
        add  ParamBase[PSP], 140 ;shift pointer to next parameter
        mov  bx, di
        mov  [bx].NamePath, di   ;inital size of the path
        mov  [bx].NameWOExt, di  ;inital size of the name without extension
        cmp  byte ptr [si], ':'  ;parameter has a drive in pathname?
        $ifnot jne
          add word ptr [bx].NamePath, 2
          stosb
          movsb
          lodsb
        $endif
        $do
          cmp  al, '.'
          $ifnot jne
            mov [bx].NameWOExt, di
          $endif
          stosb
          cmp  al, '\'
          $ifnot jne
            mov [bx].NamePath, di
          $endif
          lodsb
          cmp  al, '/'
          $break je
          call charclass
        $enddo jg
        sub  di, bx         ;calculate and store full size of the name
        mov  [bx].NameFull, di
        sub  [bx].NamePath, bx     ;convert offset to size
        sub  [bx].NameWOExt, bx    ;---- // ------
        jnz  @@L0                  ;ext not defined ?
        mov  [bx].NameWOExt, di    ;set to total size
        jmp  @@L0
do_switch:
        lodsb
        call charclass
        je   ParamErr
do_switch1:
        mov  cx, 5
Data    segment
SwitchTable DB 'wrcyn'
        Ends
        sf_w equ 2
        sf_r equ 4
        sf_c equ 8
        sf_y equ 16
        sf_n equ 32
        mov  bx, offset ds:SwitchTable+PSP-1
        mov  ah, 1h
        $do
          shl  ah, 1
          inc  bx
          cmp  al, [bx]
        $enddo loopnz
        jne  ParamErr
        test SwitchFlags, ah    ;check for double switch
        jnz  ParamErr           ;error if this
        or   SwitchFlags, ah    ;set switch flag
        lodsb
        call charclass
        jle  @@L0
        jb   do_switch1
        jmp  @@L0
ParamErr:
TooManyParms:
        mov  si, offset ds:HelpMsg+PSP
        jmp  ErrorF1
end_param:
        test SwitchFlags, sf_w
        $ifnot je
        test SwitchFlags, sf_c or sf_r or sf_n
        jne  ParamErr
        or   param_count, param_count
        xchg ax, dx
        mov  di, offset ds:SrcName+PSP
        mov  dx, di
        $ifnot jne
Data    segment
ZrdxTpl label byte
        DB 'zrdx'
ExeExtTpl DB '.exe'
BakExtTpl DB '.bak'
TempNameTpl DB '!z$bind!.tmp'
        ends
          mov  si, offset ds:ZrdxTpl+PSP
          movsw
          movsw
          movsw
          movsw
        $else jmp
          mov  si, offset ds:ExeExtTpl+PSP
          call set_default_ext
        $endif
        test ah, sf_n
        $ifnot jnz
          call CheckForOverWrite
        $endif
        mov  si, offset ds:CreatErrMsg+PSP
        call DosCreat
        xchg bx, ax
        call write_le_stub
        mov  ax, 4C00h
        int  21h
        $endif
        or   param_count, param_count
        jz   ParamErr
        mov  MSwitchFlags[PSP], SwitchFlags
        mov  di, offset ds:SrcName+PSP
        mov  si, offset ds:ExeExtTpl+PSP
        call set_default_ext
        test SwitchFlags, sf_r
        $ifnot jz                     ;replace operation
          test SwitchFlags, sf_c
          jnz  ParamErr
          cmp  param_count, 2
          $ifnot jae
            mov  di, offset ds:DestName-6+PSP
            mov  si, offset ds:SrcName-4+PSP
            mov  cx, 128+2
            lodsw
            stosw
            stosw
            rep  movsb                ;copy fp[0] to fp[1] without extention len
          $else jmp
            test SwitchFlags, sf_n
            ParamErr1:
            jnz  ParamErr
          $endif
          mov  di, offset ds:DestName+PSP
          mov  si, offset ds:BakExtTpl+PSP
          call set_default_ext         ;.BAK to destination
          mov  si, offset ds:SrcName+PSP        ;get temporary directory from fp[0]
          test SwitchFlags, sf_n
          mov  al, 2h
          jz   @@OvrCheck
        $else jmp
          ;copy operation
          cmp  param_count, 2
          jb   ParamErr1             ;b and nz
          test SwitchFlags, sf_n
          jnz  ParamErr1
          mov  di, offset ds:DestName+PSP    ;get temporary directory from fp[1]
          push di
          mov  si, offset ds:ExeExtTpl+PSP
          call set_default_ext
          pop  si
          mov  al, 40h
@@OvrCheck:
          mov  dx, offset ds:DestName+PSP
          call CheckForOverwrite
        $endif
   Exit:
        mov  dx, offset ds:SrcName+PSP
        mov  ah, 3Dh   ;dos open, al defined below
        int  21h
        $ifnot jnc
          mov   si, offset ds:OpenErrMsg+PSP
          jmp   ErrorF2
        $endif
        mov  in_handle[PSP], ax
        mov  di, offset ds:TempName+PSP
        mov  dx, di
        mov  cx, [si].NamePath
        rep  movsb                          ;copy path only from dest name or srcname
        mov  si, offset ds:TempNameTpl+PSP
        mov  cl, 12
        rep  movsb                          ;append fixed temporary name
        mov  si, offset ds:TCreatErrMsg+PSP
        call DosCreat                       ;create temporary file
        mov  out_handle[PSP], ax
        @@newheader_pos_h equ si
        @@newheader_pos_l equ di
        @@msize_l equ ax
        @@msize_h equ cx
        @@t_l     equ bx
        @@t_h     equ dx
        xor  @@msize_l, @@msize_l
        xor  @@msize_h, @@msize_h
        xor  @@newheader_pos_h, @@newheader_pos_h
        xor  @@newheader_pos_l, @@newheader_pos_l
main_cicle:
        $do
        mov  xmsize_l[PSP], @@msize_l
        mov  xmsize_h[PSP], @@msize_h
        call seek_to_newheader
        header_size equ 200h
        mov  cx, header_size
        mov  dx, offset ds:buffer+PSP
        call DosRead
        cmp  ax, 84h
        jb   BadHeader
        mov  ax, word ptr buffer[0][PSP]  ;signature
        cmp  ax, 'ZM'
        $ifnot  jne
          mov  @@msize_l, word ptr buffer[4][PSP]
          mov  @@msize_h, 8000h shr (9-1)
          $do
          shl  @@msize_l, 1
          rcl  @@msize_h, 1
          $enddo jnc
          mov  @@t_l, word ptr buffer[2][PSP]
          neg  @@t_l
          and  @@t_l, 1FFh
          sub  @@msize_l, @@t_l
          sbb  @@msize_h, 0
          cmp  word ptr buffer[24][PSP], 40h
          $ifnot jne
            cmp  @@msize_h, word ptr buffer[40h-2][PSP]
            $ifnot jb
              $toendif ja
              cmp  @@msize_l, word ptr buffer[40h-4][PSP]
              $toendif ja
            $else
              mov @@msize_h, word ptr buffer[40h-2][PSP]
              mov @@msize_l, word ptr buffer[40h-4][PSP]
            $endif
          $endif
          $toendif jmp
        $else
          cmp ax, 'WB'
        $toelse jne
          mov  @@msize_l, word ptr buffer[32][0][PSP]
          mov  @@msize_h, word ptr buffer[32][2][PSP]
          mov  @@t_l, @@msize_l
          mov  @@t_h, @@msize_h
          add  @@t_l, @@newheader_pos_l
          adc  @@t_h, @@newheader_pos_h
          cmp  @@t_l, word ptr buffer[28][0][PSP]
          jne  BadHeader
          cmp  @@t_h, word ptr buffer[28][2][PSP]
          jne  BadHeader
          $toendif jmp
          BadHeader:
          cmp  le_found[PSP], 0
          $ifnot je
            mov  @@newheader_pos_l, le_pos_l[PSP]
            mov  @@newheader_pos_h, le_pos_h[PSP]
            call seek_to_newheader
            mov  bx, out_handle[PSP]
            call write_le_stub
            call lread
            xor  @@t_h, @@t_h
            mov  @@t_l, le_stub_size
            sub  @@t_l, msize_l[PSP]
            sbb  @@t_h, msize_h[PSP]
            add  word ptr buffer[80h][0][PSP], @@t_l
            adc  word ptr buffer[80h][2][PSP], @@t_h
            jmp  copy_file
          $endif

          mov  dx, offset ds:SrcName+PSP
          mov  si, offset ds:BadHeadErrMsg+PSP
          jmp  ErrorF2
        $else
          cmp  ax, 'EL'
        $toelse jne
          mov  @@msize_l, word ptr buffer[PSP][14h]     ;number of pages in image
          mov  @@msize_h, word ptr buffer[PSP][14h][2]
          mov  @@t_h, 12
          $do
            shl @@msize_l, 1
            rcl @@msize_h, 1
            dec @@t_h
          $enddo jnz
          add  @@msize_l, word ptr buffer[PSP][80h]
          adc  @@msize_h, word ptr buffer[PSP][80h][2]
          mov  @@t_l,     word ptr buffer[PSP][2Ch]
          neg  @@t_l
          and  @@t_l, 0FFFh
          sub  @@msize_l, @@t_l
          sbb  @@msize_h, @@t_h
          mov  @@t_l, xmsize_l[PSP]
          mov  @@t_h, xmsize_h[PSP]
          sub  @@msize_l, @@t_l
          sbb  @@msize_h, @@t_h
          cmp  le_found[PSP], 0
          $ifnot jne
            mov  le_found[PSP], 1
            mov  le_pos_l[PSP], @@newheader_pos_l
            mov  le_pos_h[PSP], @@newheader_pos_h
            mov  msize_l[PSP], @@t_l
            mov  msize_h[PSP], @@t_h
          $endif
          $toendif jmp
        $else
          cmp  ax, 'MP'
        $toelse jne
          cmp  word ptr buffer[PSP][2], '1W'
          $ifnot je
            jmp  BadHeader
          $endif
          mov  @@msize_l, word ptr buffer[PSP][20h]
          mov  @@msize_h, word ptr buffer[PSP][20h][2]
          add  @@msize_l, word ptr buffer[PSP][2Ch]
          adc  @@msize_h, word ptr buffer[PSP][2Ch][2]
          add  @@msize_l, word ptr buffer[PSP][44h]
          adc  @@msize_h, word ptr buffer[PSP][44h][2]
          add  @@msize_l, word ptr buffer[PSP][4Ch]
          adc  @@msize_h, word ptr buffer[PSP][4Ch][2]
          ;jne  BadHeader
          ;cmp  @@msize_l, 5000
          ;ja   BadHeader
          $toendif jmp
        $else
          cmp ax, 'EX'
          ;jne  BadHeader
;xe format detection temporary removed
          ;$break je
          jmp  BadHeader
        $endif
        ;mov  lheader_pos_l, @@newheader_pos_l
        ;mov  lheader_pos_h, @@newheader_pos_h
        ;cmp  le_found[PSP], 0
        ;$ifnot jne
        ;  mov  msize_l[PSP], @@msize_l
        ;  mov  msize_h[PSP], @@msize_h
        ;$endif
        add  @@newheader_pos_l, @@msize_l
        adc  @@newheader_pos_h, @@msize_h
        $ifnot jno
          jmp   BadHeader
        $endif
        ;cmp  @@newheader_pos_h, file_size[2]
        ;ja   BadHeader
        ;$ifnot jne
        ;  cmp @@newheader_pos_l, file_size[0]
        ;  jae BadHeader
        ;$endif
        $enddo  jmp

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
        mov  dx, offset ds:xe_stub+PSP
        mov  bx, out_handle[PSP]
        mov  cx, xe_stub_size
        call write_stub
        call lread
        jmp  copy_file
        $do ;jmp
          call lwrite
          call lread
copy_file:
        $while
          cmp  si, cx
        $enddo je
        call lwrite
        xor  bx, bx
        xchg bx, out_handle[PSP]
        mov  ah, 3Eh
        int  21h
        mov  bx, in_handle[PSP]
        mov  ah, 3Eh
        int  21h
        mov  al, MSwitchFlags[PSP]
        test al, sf_r
        mov  dx, offset ds:DestName[PSP]
        $ifnot jnz
          call remove
          mov  di, dx
          mov  dx, offset ds:TempName[PSP]
          call rename
        $else jmp
          test ds:MSwitchFlags[PSP], sf_n
          ;test al, sf_n
          $ifnot jnz
            call remove
            mov  di, dx
            mov  dx, offset ds:SrcName[PSP]
            call rename
          $else jmp
            mov  dx, offset ds:SrcName[PSP]
            call remove
          $endif
          mov  di, dx
          mov  dx, offset ds:TempName[PSP]
          call rename
        $endif
        mov  si, offset ds:SuccMsg+PSP
        call DispMsg
        mov  ax, 4C00h
        int  21h

remove:
        mov  ah, 41h
        int  21h
        $ifnot jnc
          cmp ax, 2
          $ifnot je
            ;push dx
            mov  si, offset ds:DelErrMsg[PSP]
            jmp  ErrorF2
          $endif
        $endif
        ret

rename:
        mov  ah, 56h
        int  21h
        $ifnot jnc
          push di
          mov  si, offset ds:TRenErrMsg[PSP]
          cmp  dx, offset ds:TempName[PSP]
          $ifnot je
          ;error with name offset in dx
            mov  si, offset ds:RenErrMsg[PSP]
          ErrorF2:
            push dx
          $endif
          jmp  ErrorF1
        $endif
        ret

seek_to_newheader:
        mov  bx, in_handle[PSP]
        mov  cx, @@newheader_pos_h
        mov  dx, @@newheader_pos_l
        mov  al, 0
        ;call dos_seek
        mov  ah, 42h
        int  21h
        ret
lread:
        mov  bx, in_handle[PSP]
        mov  di, seg Buf
        $do
          mov  ax, ds:[2]
          sub  ax, di
          cmp  ax, 200h
          jb   @@ExitN
          cmp  ax, 0F00h
          sbb  si, si
          $ifnot jb
            mov ax, 0F00h
          $endif
          and  ax, not 1Fh
          mov  cl, 4
          shl  ax, cl
          xchg ax, cx
          xor  dx, dx
          mov  ds, di
          call DosRead
          ;int  21h
        ;call dos_read
          push es
          pop  ds
          xchg si, ax
          $break jc
          mov  dx, di
          add  di, 0F00h
          cmp  si, cx
          $break jne
          sahf
        $enddo jnc
        @@ExitN:
        mov  di, dx
        ret
;di:si - counter
lwrite: mov  bx, out_handle[PSP]
        mov  bp, seg Buf
        $do jmp
          add  bp, 0F00h
        $while
          mov  cx, si
          cmp  bp, di
          $ifnot je
            mov cx, 0F000h
          $endif
          mov  ds, bp
          xor  dx, dx
          mov  ax, offset ds:TWriteErrMsg+PSP
          call DosWrite
          cmp  ax, cx
          $break jne
          ;call dos_write
          cmp  bp, di
        $enddo jne
        ret
write_le_stub:
        mov  cx, le_stub_size
        mov  dx, offset ds:le_stub+PSP
write_stub:
        mov  ax, offset ds:WriteErrMsg+PSP
DosWrite PROC
        push ax
        mov  ah, 40h
        int  21h
        push es
        pop  ds
        $ifnot jc
          cmp  ax, cx
          $ifnot jne
            add sp, 2
            ret
          $endif
          mov  bx, offset ds:DiskFullMsg+PSP
        $else jmp
          mov  bx, offset ds:EmptyMsg+PSP
        $endif
        pop  si
        cmp  si, offset ds:TWriteErrMsg+PSP
        mov  ax, offset ds:DestName+PSP
        $ifnot jne
          xchg ax, bx
        $endif
ErrorF:
FileError:
        push bx
        push ax
ErrorF1:
        call DispMsg
        mov  bx, out_handle[PSP]
        or   bx, bx
        $ifnot je
          mov  ah, 3Eh
          int  21h              ;close temp file
          mov  dx, offset ds:TempName+PSP
          mov  ah, 41h
          int  21h               ;delete temp file
        $endif
        mov  si, offset ds:CrLfMsg+PSP
        call DispMsg
        mov  ax, 4C01h
        int  21h
        ENDP
DosCreat PROC
        mov  cx, 20h
        mov  ah, 3Ch
        int  21h
        $ifnot jc
        ret
        $endif
        ;mov  si, offset ds:CreatErrMsg+PSP
        xchg ax, dx
        jmp  FileError
        ENDP

DosRead PROC
        mov  ah, 3Fh
        int  21h
        push es
        pop  ds
        $ifnot jc
        ret
        $endif
        mov  si, offset ds:ReadErrMsg+PSP
        mov  ax, offset ds:SrcName+PSP
        jmp  FileError
        ENDP
Text    ends
BSS     segment
BSSEnd  label byte
        ends
end start



