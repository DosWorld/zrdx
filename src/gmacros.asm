;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

;                      Macros definitions
;Define label for virtual segments only
DEFLabel MACRO MM, Label, Disp, Disp16, Shift
        IFNB <Label>
          Label      EQU MM ds:[LARGE Disp]
          Label&A16  EQU MM ds:[SMALL Disp]
          Off&Label = Disp
          Label&R    EQU MM ds:[SMALL Disp16]
          ROff&Label = Disp16
        ENDIF
        LC   = LC + Shift
        LC16 = LC16 + Shift
        ENDM
DFB MACRO Label, lCount
    IFNB <lCount>
      DefLabel <byte ptr>, Label, %LC, %LC16, lCount
    ELSE
      DefLabel <byte ptr>, Label, %LC, %LC16, 1
    ENDIF
    ENDM
DFW MACRO Label, lCount
    IFNB <lCount>
      DefLabel <word ptr>, Label, %LC, %LC16, (lCount)*2
    ELSE
      DefLabel <word ptr>, Label, %LC, %LC16, 2
    ENDIF
    ENDM

DFD MACRO Label, lCount
    IFNB <lCount>
      DefLabel <dword ptr>, Label, %LC, %LC16, (lCount)*4
    ELSE
      DefLabel <dword ptr>, Label, %LC, %LC16, 4
    ENDIF
    ENDM

_DEFLLabel MACRO MM, Label, Disp, DispA16
          Label&R    EQU MM ds:[Disp]
          Label&A16  EQU MM ds:[SMALL DispA16]
          ROff&Label = Disp
          ENDM

DEFLLabel MACRO MM, Label
        IFNB <Label>
          Label      EQU MM ds:[LARGE $-CurSegBase]
          Off&Label = $-CurSegBase
          _DEFLLabel <MM>, <Label>, %($-CurSegBaseR), %($-CurSegBase)
          ;Label&R    EQU MM ds:[$-CurSegBaseR]
          ;ROff&Label = $-CurSegBaseR
        ENDIF
        ENDM

LByte MACRO Label
    DefLLabel <byte ptr>, Label
    ENDM
LWord MACRO Label
      DefLLabel <word ptr>, Label
      ENDM

LDWord MACRO Label
       DefLLabel <dword ptr>, Label
       ENDM

LLabel  MACRO Label
        DefLLabel , Label,
        ENDM
DFL     MACRO Label
        DFB Label, 0
        ENDM
DFP     MACRO Label
        Off&Label  = $ - CurSegBase
        ROff&Label = $ - CurSegBaseR
        ;Label      EQU ds:[SegStartText][$ - CurSegBase]
        Label&R    EQU ds:[$ - CurSegBaseR][SegStartText16]
        ENDM
LinkLC  MACRO
        LC   = $ - CurSegBase
        LC16 = $ - CurSegBaseR
        ENDM

PutRTData MACRO XDisp, SName, N
        SName&N segment
          IF XDisp GE 80h
            DW ((XDisp and 0FFh) shl 8) + ((XDisp and 7F00h) shr 7)
          ELSE
            DB XDisp*2+1
          ENDIF
        SName&N ends
        ENDM

RRT     MACRO Disp
        _RRTEntryes = _RRTEntryes+1
        ___W = $ - CurSegBaseR
        RelocR0 segment
        IFB <Disp>
          DW ___W-4+PSP
        ELSE
          DW ___W-4-Disp+PSP
        ENDIF
        ends
        ENDM

        comment #
        LOCAL XDisp
        if CurRTNum EQ - 1
          .err illegal rrt use
        endif
        XDisp = $ - LastRelocR - 4
        IFNB <Disp>
          XDisp = XDisp - Disp
          LastRelocR = $ - Disp
        ELSE
          LastRelocR = $
        ENDIF
        PutRTData XDisp, RelocR, %CurRTNum
        ENDM     #

RT      MACRO Disp
        LOCAL XDisp
        XDisp = $ - LastReloc - 4
        IFNB <Disp>
          XDisp = XDisp - Disp
          LastReloc = $ - Disp
        ELSE
          LastReloc = $
        ENDIF
        PutRTData XDisp, Reloc, %CurRTNum
        _RTEntryes = _RTEntryes+1
        ENDM

SEGM    MACRO Name
        Name segment
        ;SavedRRT&Name = LastRelocR
        ;SavedRT&Name  = LastReloc
        ;LastRelocR    = SegRRT&Name
        ;LastReloc     = SegRT&Name

        SavedSB&Name  = CurSegBase
        SavedSBR&Name = CurSegBaseR
        CurSegBase    = SegBase&Name
        CurSegBaseR   = SegBaseR&Name
        ;SavedRTNum    = CurRTNum
        ;CurRTNum      = SegRTNum&Name
        LC   = $ - CurSegBase
        LC16 = $ - CurSegBaseR
        ENDM

ESEG    MACRO Name
        ;SegRRT&Name = LastRelocR
        ;SegRT&Name  = LastReloc
        ;LastRelocR  = SavedRRT&Name
        ;LastReloc   = SavedRT&Name

        CurSegBase  = SavedSB&Name
        CurSegBaseR = SavedSBR&Name
        ;CurRTNum    = SavedRTNum
        Name ends
        ENDM
VSegm   MACRO Name
        SavedLC   = LC
        SavedLC16 = LC16
        LC   = Name&LC
        LC16 = Name&LC16
        ENDM
EVSeg   MACRO Name
        Name&LC   = LC
        Name&LC16 = LC16
        LC   = SavedLC
        LC16 = SavedLC16
        ENDM
VSAlign MACRO A
        Local L
        if (LC mod A) ne 0
          DFB L, %(A - (LC mod A))
        endif
        ENDM

CurSegBase  = 0      ;define any values for correct SEGM work
CurSegBaseR = 0

int6code    = 06CDh        ;int 6 code in word format
JmpFarCode  = 0EAh         ;jmp far code in byte format
JmpNearCode = 0E9h
CallFarCode = 09Ah
PushWCode   = 68h
PushBCode   = 6Ah
JmpShortCode= 0EBh
NearCallCode= 0E8h
RetfCode    = 0CBh
S32Bit      = 18
XXX         = 45h
SSPrefix    = 36h
PSP         = 100h
VCPIPageBit = 8  ;this bit in the page table or directory indicates,
                 ;that page is from VCPI, and must be returned to it
;Init flags:
PriorVCPIUse = 1

;PMTR = XXX
;PMCS = XXX
;DataSelector = 8h
;CodeSelector = 10h
;TSSSelector  = 18h
;LDTSelector  = 20h
;GatesSelector= 28h
;VCPISelector = GatesSelector+8+NTraps3*8

;ClientHandlerCS = XXX
IFBitMask    = 2

DefRealFlags = XXX

;stack frame for interrupt
IFrame struc
IFrEIP    DD ?
IFrCS     DD ?
IFrFlags  DD ?
IFrESP    DD ?
IFrSS     DD ?
IFrame ends
CFrame struc    ;for call
CFrEIP    DD ?
CFrCS     DD ?
CFrESP    DD ?
CFrSS     DD ?
CFrame ends

;frame of this structure are created on the real mode stack before
;returning to VM86
;fields StartEIP0, StartCS0 initalized by SwitcherToVM
VMIShortStruct struc
        ;VMI_EIP0       DD ?
        ;VMI_CS0        DD ?
        VMI_Reserv     DD ?
        VMI_ESP        DD ?;ss:esp must points to VMI_EAX
        VMI_SS         DD ?
        VMI_ES         DD ?
        VMI_DS         DD ?
        VMI_FS         DD ?
        VMI_GS         DD ?
        VMI_EAX        DD ?
        VMI_IP         DW ?
        VMI_CS         DW ?
        VMI_Flags      DW ?
VMIShortStruct ends
VMIStruct struc
        VMIShortStruct <>
        VMI_EndIP      DW ?
        VMI_EndCS      DW ?
        VMI_EndFlags   DW ?
VMIStruct ends

RMIEStruct STRUC
        ;saved client registers
        RMIE_EBX       DD ?
        RMIE_ECX       DD ?
        RMIE_EDX       DD ?
        RMIE_ESI       DD ?
        RMIE_EBP       DD ?
        RMIE_EAX       DD ?
        RMIE_DS        DW ?
        RMIE_EDI       DD ?
        RMIE_ES        DW ?
        RMIE_FS        DW ?
        RMIE_GS        DW ?
        RMIE_RealStack DD ?
        ;client iret frame
        RMIE_EIP       DD ?
        RMIE_CS        DD ?
        RMIE_EFlags    DD ?
RMIEStruct ENDS

;stack frame for simple interrupt from client PM - save all selectors,
;replaced with RM segments
PMIStruct STRUC
        ;saved registers
        PMI_ES         DW ?
        PMI_DS         DW ?
        PMI_FS         DW ?
        PMI_GS         DW ?
        ;client iret frame
        PMI_EIP        DD ?
        PMI_CS         DD ?
        PMI_EFlags     DD ?
PMIStruct ENDS

RMSStruct struc
        RMS_Flags      DW ?
        RMS_ESI        DD ?
        RMS_EBP        DD ?
        RMS_EAX        DD ?
        RMS_ES         DW ?
        RMS_DS         DW ?
        RMS_FS         DW ?
        RMS_GS         DW ?
        RMS_SwitchCode DW ?
RMSStruct ends

;structure with client real mode callbacks info
CBTStruct STRUC
        CBT_EIP     DD ?
        CBT_CS      DW ?
        CBT_SPtrOff DD ?
        CBT_SPtrSeg DW ?
CBTStruct ENDS

DC_Struct STRUC
          DC_EDI   DD ?
          DC_ESI   DD ?
          DC_EBP   DD ?
          DC_ESP   DD ?
          DC_EBX   DD ?
          DC_EDX   DD ?
          DC_ECX   DD ?
          DC_EAX   DD ?
          DC_Flags DW ?
          DC_ES    DW ?
          DC_DS    DW ?
          DC_FS    DW ?
          DC_GS    DW ?
          DC_IP    DW ?
          DC_CS    DW ?
          DC_SP    DW ?
          DC_SS    DW ?
DC_Struct ENDS
EDC_Struct STRUC
          DC_Struct <?>
;several extra fields special for DOS extender
          DC_ES0   DW ?     ;saved PM client selectors
          DCStructSize = DC_ES0
          DC_DS0   DW ?
          DC_FS0   DW ?
                   ;DW ?
                   ;DD 10 dup(?)
          ;DC_BUFFERSIZE DD ?
          DC_REIP     DD ?
          DC_EID      DD ?
          DC_REFLAGS  DD ?
          DC_REIP1    DD ?
          DC_RCS1     DD ?
          DC_REFLAGS1 DD ?
          ENDS
          ;DC_GS0   DW ?
          ;DC_SavedSize DD ?       ;saved size of the moved data
          ;DC_IntNum DB ?          ;number of requested interrupt
          ;DC_OffsetIndex  DB ?    ;index of register, contains offset of the transferred date
          ;DC_SegmentIndex DB ?    ;-- for segment register
          DC_FirstSR0 EQU DC_ES0
          DC_FirstSR EQU DC_ES
          DC_FirstR EQU DC_EDI
          DC_RCS  equ DC_EID
          ;DC_REIP equ dword ptr DC_RCS

EXC_Struct STRUC
           EXC_REIP    DD ?
           EXC_RCS     DD ?
           EXC_Errcode DD ?
           EXC_EIP     DD ?
           EXC_CS      DD ?
           EXC_EFlags  DD ?
           EXC_ESP     DD ?
           EXC_SS      DD ?
EXC_Struct ENDS

DPROC MACRO Label
        DFP Label
        Label PROC C
        ENDM
CHK     MACRO n
        mov ax, 4C00h+n
        int 21h
        ENDM

Descr  MACRO Base, Limit, ACR
        DW Limit and 0FFFFh
        DW Base and 0FFFFh
        DB (Base shr 16) and 0FFh
        DW (ACR and 0C0FFh) + ((Limit shr 8) and 0F00h)
        DB (Base shr 24) and 0FFh
       ENDM
GDescr MACRO Offset, Selector, ACR
        DW Offset and 0FFFFh
        DW Selector
        DW (ACR) shl 8
        DW (Offset shr 16) and 0FFFFh
       ENDM

VCPICallTrap MACRO
        DB CallFarCode
        DD 0
        DW VCPICallGateSelector
        ENDM
VCPITrap MACRO
        DB CallFarCode
        DD 0
        DW VCPITrapGateSelector
         ENDM
InvalidateTLB MACRO
        DB CallFarCode
        DD 0
        DW InvalidateTLBGateSelector
        ENDM
Log     MACRO
        call DispLog
        ENDM

MCBStruct STRUC
        MCB_Prev        DD ?
        MCB_Next        DD ?
        MCB_StartOffset DD ?
        MCB_EndOffset   DD ?
        ENDS

PAStruct STRUC
        PA_EDI DD ?
        PA_ESI DD ?
        PA_EBP DD ?
        PA_ESP DD ?
        PA_EBX DD ?
        PA_EDX DD ?
        PA_ECX DD ?
        PA_EAX DD ?
        ENDS

rdtsc   MACRO
        DB 0Fh, 31h
        ENDM
ETextG equ EText
IETextG equ IEText
