;             This file is part of the ZRDX 0.50OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda
;NASM version

;segment definitions for
;real mode rezident switchers for ZRDX DPMI host under VCPI
%assign KernelBase 4*1024*1024
SegRTNum equ 0
%ifndef BePacked
%assign PLShift 10h
%else
%assign PLShift 1100h
%endif
%assign Release 1
;%assign VMM 1

%define zcat3(a,b,c) a %+ b %+ c

%assign XCurSegBase 0
%assign XCurSegBaseR 0

%macro SetupSeg 3-4
%ifdef Release
        %assign _Sz ((%1) + 15) & ~15
%else
        %assign _Sz ((%2) + 15) & ~15
%endif
        zsetv zcat2(SegSize,%3), _Sz
        zsetv zcat2(SegBase,%3), 0 - XCurSegBase
        zequ zcat2(SegStart,%3), XCurSegBase
        zsetv zcat2(SegBaseR,%3), 0 - XCurSegBaseR
        %assign XCurSegBase XCurSegBase + _Sz
        %assign XCurSegBaseR XCurSegBaseR + _Sz
%endmacro
%macro SetupRTSeg 3-4
        SetupSeg %1, %2, %3
%endmacro

%macro VSegInit 3
%ifidn %1, BSS16
        %assign BSS16LC %2
        %assign BSS16LC16 %3
%elifidn %1, EBSS
        %assign EBSSLC %2
        %assign EBSSLC16 %3
%elifidn %1, IEBSS
        %assign IEBSSLC %2
        %assign IEBSSLC16 %3
%elifidn %1, BSS
        %assign BSSLC %2
        %assign BSSLC16 %3
%else
%error unknown virtual segment %1
%endif
%endmacro

%macro VSegment 3
%ifdef Release
        zsetv zcat2(SegSize,%1), %2
%else
        zsetv zcat2(SegSize,%1), %3
%endif
        VSegInit %1, XCurSegBase, XCurSegBaseR
        zsetv zcat3(SegStart,%1,R), XCurSegBaseR
%endmacro

segment Text16  align=16 public class=CODE16 use16
SegBits_Text16 equ 16
segment Data16  align=16 public class=DATA16 use16
SegBits_Data16 equ 16
segment RelocR0 align=16 public class=RELOCR use16
SegBits_RelocR0 equ 16
segment IText16 align=16 public class=IDATA use16
SegBits_IText16 equ 16
segment IData16 align=16 public class=IDATA use16
SegBits_IData16 equ 16
segment EText   align=16 public class=EXTENDER use32
SegBits_EText equ 32
segment EData   align=16 public class=EXTENDER use32
SegBits_EData equ 32
%ifdef EDebug
segment DBText  align=1 public use16
SegBits_DBText equ 16
%endif
segment IEText  align=16 public class=LOADER use32
SegBits_IEText equ 32
segment IEData  align=16 public class=LOADER use32
SegBits_IEData equ 32
segment Text    align=16 public class=CODE use32
SegBits_Text equ 32
segment IText   align=16 public class=CODE use32
SegBits_IText equ 32
segment Data    align=16 public class=DATA use32
SegBits_Data equ 32
segment BSSX    align=16 public class=DATA use32
SegBits_BSSX equ 32
segment Stock   align=16 public class=XXXX use16
SegBits_Stock equ 16
segment Stack16 align=16 stack class=STACK use16
SegBits_Stack16 equ 16

group DGROUP16 Text16 Data16 IText16 IData16 RelocR0
group EGroup EText EData
group LGROUP IEData IEText
group DGROUP Text Data

%define ETextG EText
%define IETextG IEText

segment Text16
DPMIHOSTMaxLowData equ XCurSegBaseR
        SetupRTSeg 760, 800, Text16, EText16
         ;RM data for IDPMI host
segment Data16
        SetupSeg 000, 0, Data16, Text16
         ;RM stack for IDMPI host
        VSegment BSS16, 1000, 1000
segment RelocR0
        SetupSeg 88 , 98, RelocR0
segment IText16
        SetupRTSeg 2200, 2400, IText16, RelocR0
segment IData16
        SetupSeg 480, 600, IData16, IText16
segment EText
ExtenderStart equ XCurSegBaseR
%assign TXCurSegBase XCurSegBase
%assign XCurSegBase 0
        SetupSeg 2720, 2800, EText, IData16
segment EData
        SetupSeg 793, 862, EData, EText
        VSegment EBSS, 200, 200
%assign ExtenderSize XCurSegBase
ExtenderFullSize equ ExtenderSize+200
%ifdef EDebug
segment DBText
DebuggerStart equ XCurSegBaseR
        SetupSeg 9000, 9000, DBText
%endif
segment IEText
LoaderStart equ XCurSegBaseR
%assign TXCurSegBase TXCurSegBase + XCurSegBase
%assign XCurSegBase 0
        SetupSeg 1660, 2000, IEText, EData
segment IEData
        SetupSeg 313, 500, IEData, IEText
%assign ROffLoaderEnd XCurSegBaseR
%assign WinSize (ROffLoaderEnd+0FFFh+200h) & ~0FFFh
%assign LoaderSize XCurSegBase
LoaderFullSize equ LoaderSize+3000
        VSegment IEBSS, 3008, 3008
        ;IDPMI host rezident code
segment Text
%assign OffProtectedStart XCurSegBaseR
%assign XCurSegBase KernelBase
        SetupRTSeg 7350, 9000, Text, IEData
segment IText
        SetupSeg 0000, 000, IText, Text
segment Data
        SetupSeg 364, 600, Data, IText
        VSegment BSS, 9F00h, 9F00h
LastRelocR equ 0
LastReloc equ 0
CurRTNum equ 0
