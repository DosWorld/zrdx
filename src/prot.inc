BIOS_MOVE STRUC
          DB 10h dup(?)
BM_SLIMIT DW ?
BM_SOURCE DB 3 DUP (?)
BM_SACR   DB ?
          DW ?
BM_DLIMIT DW ?
BM_DEST   DB 3 DUP(?)
BM_DACR   DB ?
          DB 12h DUP(?)
BIOS_MOVE ENDS

TSS_DEF STRUC
TSS_Link    DW 0
            DW 0
TSS_ESP0    DD 0
TSS_SS0     DW 0
            DW 0
TSS_ESP1    DD 0
TSS_SS1     DW 0
            DW 0
TSS_ESP2    DD 0
TSS_SS2     DW 0
            DW 0
TSS_CR3     DD 0
TSS_EIP     DD 0
TSS_EFLAGS  DD 0
TSS_EAX     DD 0
TSS_ECX     DD 0
TSS_EDX     DD 0
TSS_EBX     DD 0
TSS_ESP     DD 0
TSS_EBP     DD 0
TSS_ESI     DD 0
TSS_EDI     DD 0
TSS_ES      DW 0
            DW 0
TSS_CS      DW 0
            DW 0
TSS_SS      DW 0
            DW 0
TSS_DS      DW 0
            DW 0
TSS_FS      DW 0
            DW 0
TSS_GS      DW 0
            DW 0
TSS_LDT     DW 0
            DW 0
TSS_TRACE   DW 0
TSS_IOBASE  DW 0
TSS_DEF ENDS

SS_FREE_TSS2 = 1
SS_LDT       = 2
SS_BUSY_TSS2 = 3
SS_GATE_PROC2= 4
SS_GATE_TASK = 5
SS_GATE_INT2 = 6
SS_GATE_TRAP2= 7
SS_FREE_TSS3 = 9
SS_BUSY_TSS3 = 11
SS_GATE_PROC3= 12
SS_GATE_INT3 = 14
SS_GATE_TRAP3= 15

DS_DATA_WR     =12h      ;Данные расширяются вверх, можно писать
DS_DATA        =10h      ;Данные расширяются вверх, нельзя писать
DS_DATA_DN_WR  =16h      ;Данные расширяются вниз(стек), можно писать
DS_DATA_DN     =14h      ;Данные расширяются вниз, нельзя писать
DS_CODE        =18h      ;код, нельзя читать
DS_CODE_RD     =1Ah      ;код, можно читать
DS_CODE_C      =1Ch      ;код подчиненный, нельзя читать
DS_CODE_C_RD   =1Eh      ;код подчиненный, можно читать
DS_0=0
DS_1=20h
DS_2=40h
DS_3=60h
DS_PAGE = 200h
DS_32 = 100h

DESCR MACRO name, seg, ofs, lim, ar
name = $ - DTableBase
DW lim and 0FFFFh
DW ofs
DW seg
DB (lim shr 16) or ((ar shr 2) and 0C0h)
DB 80h or (ar and 0FFh)
      ENDM

DSetup MACRO
Local L
L:
if 0 eq 1
       mov   ax, [bx+4]
       rol   ax, 4
       mov   dx, ax
       and   dx, 0Fh
       and   ax, 0FFF0h
       add   ax, [bx+2]
       adc   dx, 0
       mov   [bx+2], dx
       mov   [bx+4], ax
endif
       movzx eax, word ptr [bx+2]
       movzx edx, word ptr [bx+4]
       shl   edx, 4
       add   eax, edx
       mov   dword ptr [bx+2], eax
       mov   al, [bx+7]
       xchg  al, [bx+5]
       mov   [bx+7], al
       add   bx, 8
       loop  L
       ENDM
