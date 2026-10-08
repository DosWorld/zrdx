;             This file is part of the ZRDX 0.50OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda
;NASM version

;                      Macros definitions

%define zcat2(a,b) a %+ b

%macro zsetv 2
%assign %1 %2
%endmacro

%macro zequ 2
%1 equ %2
%endmacro

%assign LC 0
%assign LC16 0
%assign CurSegBase 0
%assign CurSegBaseR 0
%xdefine CurSegName none
%assign _RRTEntryes 0
%assign vsn 0

;Define label for virtual segments only
%macro DefLabel 4
        zsetv zcat2(Off,%1), %2
        zsetv zcat2(ROff,%1), %3
        %assign LC LC+(%4)
        %assign LC16 LC16+(%4)
%endmacro
%macro DFB 1-2 1
        DefLabel %1, LC, LC16, %2
%endmacro
%macro DFW 1-2 1
        DefLabel %1, LC, LC16, (%2)*2
%endmacro
%macro DFD 1-2 1
        DefLabel %1, LC, LC16, (%2)*4
%endmacro
%macro DFL 1
        DFB %1, 0
%endmacro

%macro LBack 2
Off %+ %1 equ ($ - $$) - (%2) - CurSegBase
ROff %+ %1 equ ($ - $$) - (%2) - CurSegBaseR
%endmacro
%macro DefLLabel 1
Off %+ %1 equ ($ - $$) - CurSegBase
ROff %+ %1 equ ($ - $$) - CurSegBaseR
%endmacro
%macro LByte 1-2
        DefLLabel %1
%endmacro
%macro LWord 1
        DefLLabel %1
%endmacro
%macro LDWord 1
        DefLLabel %1
%endmacro
%macro LLabel 1
        DefLLabel %1
%endmacro
%macro DPROC 1
        DefLLabel %1
%1:
%endmacro

%macro RRT 0-1 0
        %assign _RRTEntryes _RRTEntryes+1
%%w     equ ($ - $$) - CurSegBaseR
        %xdefine _rrtseg CurSegName
        segment RelocR0
        dw %%w - 4 - (%1) + PSP
        segment _rrtseg
%endmacro

%macro SetBits 1
%if %1 = 32
        bits 32
%else
        bits 16
%endif
%endmacro

%macro SEGM 1
%push seg
        %xdefine %$prev CurSegName
        %assign %$sb CurSegBase
        %assign %$sbr CurSegBaseR
        %xdefine CurSegName %1
        segment %1
        SetBits zcat2(SegBits_,%1)
        %assign CurSegBase zcat2(SegBase,%1)
        %assign CurSegBaseR zcat2(SegBaseR,%1)
%endmacro

%macro ESEG 1
        %assign CurSegBase %$sb
        %assign CurSegBaseR %$sbr
        %xdefine CurSegName %$prev
%ifnidn %$prev, none
        segment %$prev
        SetBits zcat2(SegBits_,%$prev)
%endif
%pop
%endmacro

%macro VSegm 1
        %assign SavedLC LC
        %assign SavedLC16 LC16
%ifidn %1, BSS16
        %assign LC BSS16LC
        %assign LC16 BSS16LC16
%elifidn %1, EBSS
        %assign LC EBSSLC
        %assign LC16 EBSSLC16
%elifidn %1, IEBSS
        %assign LC IEBSSLC
        %assign LC16 IEBSSLC16
%elifidn %1, BSS
        %assign LC BSSLC
        %assign LC16 BSSLC16
%else
%error unknown virtual segment %1
%endif
%endmacro

%macro EVSeg 1
%ifidn %1, BSS16
        %assign BSS16LC LC
        %assign BSS16LC16 LC16
%elifidn %1, EBSS
        %assign EBSSLC LC
        %assign EBSSLC16 LC16
%elifidn %1, IEBSS
        %assign IEBSSLC LC
        %assign IEBSSLC16 LC16
%elifidn %1, BSS
        %assign BSSLC LC
        %assign BSSLC16 LC16
%else
%error unknown virtual segment %1
%endif
        %assign LC SavedLC
        %assign LC16 SavedLC16
%endmacro

%macro VSAlign 1
%if (LC - (LC // (%1)) * (%1)) != 0
        %assign vsn vsn+1
        DFB zcat2(_vsa,vsn), (%1) - (LC - (LC // (%1)) * (%1))
%endif
%endmacro

%macro Descr 3
        dw (%2) & 0FFFFh
        dw (%1) & 0FFFFh
        db ((%1) >> 16) & 0FFh
        dw ((%3) & 0C0FFh) + (((%2) >> 8) & 0F00h)
        db ((%1) >> 24) & 0FFh
%endmacro
%macro GDescr 3
        dw (%1) & 0FFFFh
        dw %2
        dw (%3) << 8
        dw ((%1) >> 16) & 0FFFFh
%endmacro

%macro VCPICallTrap 0
        db CallFarCode
        dd 0
        dw VCPICallGateSelector
%endmacro
%macro VCPITrap 0
        db CallFarCode
        dd 0
        dw VCPITrapGateSelector
%endmacro
%macro InvalidateTLB 0
        db CallFarCode
        dd 0
        dw InvalidateTLBGateSelector
%endmacro
%macro Log 0
        call DispLog
%endmacro
%macro rdtsc 0
        db 0Fh, 31h
%endmacro


%assign CurSegBase 0 ;define any values for correct SEGM work
%assign CurSegBaseR 0

int6code equ 06CDh ;int 6 code in word format
JmpFarCode equ 0EAh ;jmp far code in byte format
JmpNearCode equ 0E9h
CallFarCode equ 09Ah
PushWCode equ 68h
PushBCode equ 6Ah
JmpShortCode equ 0EBh
NearCallCode equ 0E8h
RetfCode equ 0CBh
S32Bit equ 18
%assign XXX 45h
SSPrefix equ 36h
PSP equ 100h
VCPIPageBit equ 8 ;this bit in the page table or directory indicates,
                 ;that page is from VCPI, and must be returned to it
;Init flags:
PriorVCPIUse equ 1

;PMTR = XXX
;PMCS = XXX
;DataSelector = 8h
;CodeSelector = 10h
;TSSSelector  = 18h
;LDTSelector  = 20h
;GatesSelector= 28h
;VCPISelector = GatesSelector+8+NTraps3*8

;ClientHandlerCS = XXX
IFBitMask equ 2

DefRealFlags equ XXX

;stack frame for interrupt
;IFrame struc
IFrEIP equ 0
IFrCS equ 4
IFrFlags equ 8
IFrESP equ 12
IFrSS equ 16
IFrame_size equ 20
;CFrame struc    ;for call
CFrEIP equ 0
CFrCS equ 4
CFrESP equ 8
CFrSS equ 12
CFrame_size equ 16

;frame of this structure are created on the real mode stack before
;returning to VM86
;fields StartEIP0, StartCS0 initalized by SwitcherToVM
;VMIShortStruct struc
;        ;VMI_EIP0       DD ?
;        ;VMI_CS0        DD ?
VMI_Reserv equ 0
VMI_ESP equ 4 ;ss:esp must points to VMI_EAX
VMI_SS equ 8
VMI_ES equ 12
VMI_DS equ 16
VMI_FS equ 20
VMI_GS equ 24
VMI_EAX equ 28
VMI_IP equ 32
VMI_CS equ 34
VMI_Flags equ 36
VMIShortStruct_size equ 38
;VMIStruct struc
;        VMIShortStruct <>
VMI_EndIP equ 38
VMI_EndCS equ 40
VMI_EndFlags equ 42
VMIStruct_size equ 44

;RMIEStruct STRUC
;        ;saved client registers
RMIE_EBX equ 0
RMIE_ECX equ 4
RMIE_EDX equ 8
RMIE_ESI equ 12
RMIE_EBP equ 16
RMIE_EAX equ 20
RMIE_DS equ 24
RMIE_EDI equ 26
RMIE_ES equ 30
RMIE_FS equ 32
RMIE_GS equ 34
RMIE_RealStack equ 36
;        ;client iret frame
RMIE_EIP equ 40
RMIE_CS equ 44
RMIE_EFlags equ 48
RMIEStruct_size equ 52

;stack frame for simple interrupt from client PM - save all selectors,
;replaced with RM segments
;PMIStruct STRUC
;        ;saved registers
PMI_ES equ 0
PMI_DS equ 2
PMI_FS equ 4
PMI_GS equ 6
;        ;client iret frame
PMI_EIP equ 8
PMI_CS equ 12
PMI_EFlags equ 16
PMIStruct_size equ 20

;RMSStruct struc
RMS_Flags equ 0
RMS_ESI equ 2
RMS_EBP equ 6
RMS_EAX equ 10
RMS_ES equ 14
RMS_DS equ 16
RMS_FS equ 18
RMS_GS equ 20
RMS_SwitchCode equ 22
RMSStruct_size equ 24

;structure with client real mode callbacks info
;CBTStruct STRUC
CBT_EIP equ 0
CBT_CS equ 4
CBT_SPtrOff equ 6
CBT_SPtrSeg equ 10
%assign CBTStruct_size 12

;DC_Struct STRUC
%assign DC_EDI 0
%assign DC_ESI 4
%assign DC_EBP 8
DC_ESP equ 12
%assign DC_EBX 16
%assign DC_EDX 20
%assign DC_ECX 24
%assign DC_EAX 28
DC_Flags equ 32
%assign DC_ES 34
DC_DS equ 36
DC_FS equ 38
DC_GS equ 40
DC_IP equ 42
DC_CS equ 44
DC_SP equ 46
DC_SS equ 48
DC_Struct_size equ 50
;EDC_Struct STRUC
;          DC_Struct <?>
%assign DC_ES0 50 ;saved PM client selectors
DCStructSize equ DC_ES0
DC_DS0 equ 52
DC_FS0 equ 54
;                   ;DW ?
;                   ;DD 10 dup(?)
;          ;DC_BUFFERSIZE DD ?
DC_REIP equ 56
%assign DC_EID 60
DC_REFLAGS equ 64
DC_REIP1 equ 68
DC_RCS1 equ 72
DC_REFLAGS1 equ 76
EDC_Struct_size equ 80
          ;DC_GS0   DW ?
          ;DC_SavedSize DD ?       ;saved size of the moved data
          ;DC_IntNum DB ?          ;number of requested interrupt
          ;DC_OffsetIndex  DB ?    ;index of register, contains offset of the transferred date
          ;DC_SegmentIndex DB ?    ;-- for segment register
DC_FirstSR0 equ DC_ES0
DC_FirstSR equ DC_ES
DC_FirstR equ DC_EDI
DC_RCS equ DC_EID
          ;DC_REIP equ dword ptr DC_RCS

;EXC_Struct STRUC
EXC_REIP equ 0
EXC_RCS equ 4
EXC_Errcode equ 8
EXC_EIP equ 12
EXC_CS equ 16
EXC_EFlags equ 20
EXC_ESP equ 24
EXC_SS equ 28
EXC_Struct_size equ 32

;MCBStruct STRUC
MCB_Prev equ 0
MCB_Next equ 4
MCB_StartOffset equ 8
MCB_EndOffset equ 12
%assign MCBStruct_size 16

;PAStruct STRUC
PA_EDI equ 0
PA_ESI equ 4
PA_EBP equ 8
PA_ESP equ 12
PA_EBX equ 16
PA_EDX equ 20
PA_ECX equ 24
PA_EAX equ 28
PAStruct_size equ 32

%define ETextG EText
%define IETextG IEText
