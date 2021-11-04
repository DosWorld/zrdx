;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

;segment definitions for
;real mode rezident switchers for ZRDX DPMI host under VCPI
KernelBase = 4*1024*1024
SegRTNum = 0
ifndef BePacked
  PLShift = 10h
else
  ifdef XLE
    PLShift = 1000h
  else
    PLShift = 1100h
  endif
endif
Release = 1
;VMM = 1
SetupSeg MACRO Sz, DSz, S, PS
        Local _Sz
        ifdef RELEASE
          _Sz = (Sz + 15) and (not 0Fh)
        else
          _Sz = (DSz + 15) and (not 0Fh)
        endif
        SegSize&S = _Sz
        SegBase&S    = $ - XCurSegBase
        SegStart&S   = XCurSegBase
        XSegStart&S  = $
        SegStart&S&R = XCurSegBaseR
        SegEnd&S&R   = XCurSegBaseR+_Sz
        SegBaseR&S   = $ - XCurSegBaseR
        XCurSegBase  = XCurSegBase + _Sz
        XCurSegBaseR = XCurSegBaseR + _Sz
        ENDM
SetupRTSeg EQU SetupSeg
        XCurSegBase  = 0
        XCurSegBaseR = 0
VSegment MACRO Name, Sz, DSz
        ifdef RELEASE
          SegSize&Name = Sz
        else
          SegSize&Name = DSz
        endif
        Name&LC   = XCurSegBase
        Name&LC16 = XCurSegBaseR
        SegStart&Name&R = XCurSegBaseR
        ENDM
segment Text16  para use16 public 'CODE16'
ends Text16
segment Data16  para  use16 public 'DATA16'
ends Data16
segment RelocR0 use16 para public 'RELOCR'
        ;SegStartRelocR label byte
ends RelocR0
segment IText16 para use16 public 'IDATA'
ends IText16
segment IData16 para use16 public 'IDATA'
ends IData16
segment EText   para use32 public 'EXTENDER'
        ends
segment EData   para use32 public 'EXTENDER'
        ends
IFDEF EDebug
segment DBText
        ends
ENDIF
segment IEText  para use32 public 'LOADER'
        ends
segment IEData  para use32 public 'LOADER'
        ends
segment Text    para use32 public 'CODE'
ends Text
segment IText   para use32 public 'CODE'
ends IText
segment Data    para use32 public 'DATA'
ends Data
segment BSSX    para use32 public 'DATA'
        ends
segment Stock     para public 'XXXX'
;          db 40000 dup(?)
          ends
segment Stack16 para use16 stack 'STACK'
ends Stack16
comment &
Text16__536
Data16__0
RelocR0__78
IText16__2036
IData16__420
EText__2366
EData__725
DBText__0
IEText__1580
IEData__316
Text__6714
IText__0
Data__338

Text16__543
Data16__0
RelocR0__88
IText16__2049
IData16__425
EText__2360
EData__751
IEText__1589
IEData__321
Text__6862
IText__0
Data__338
&
segment Text16  para use16 public 'CODE16'
        DPMIHOSTMaxLowData = XCurSegBaseR
        SetupRTSeg 736, 750, Text16, EText16
ends Text16
         ;RM data for IDPMI host
segment Data16  para  use16 public 'DATA16'
        SetupSeg 000, 0, Data16, Text16
ends Data16
         ;RM stack for IDMPI host
VSegment BSS16, 1000, 1000
segment RelocR0 use16 para public 'RELOCR'
        SetupSeg 88 , 98, RelocR0
ends RelocR0
        IDPMIDataSize = OffRStackEnd
segment IText16 para use16 public 'IDATA'
        SetupRTSeg 1923, 2200, IText16, RelocR0
ends IText16
segment IData16 para use16 public 'IDATA'
        SetupSeg 431 , 600, IData16, IText16
ends IData16
segment EText   para use32 public 'EXTENDER'
        ExtenderStart = XCurSegBaseR
        TXCurSegBase  = XCurSegBase
        XCurSegBase   = 0
        SetupSeg 2451, 2600, EText, IData16
        ends
segment EData   para use32 public 'EXTENDER'
        SetupSeg 793, 862, EData, EText
        ends
VSegment EBSS, 200, 200
         ExtenderSize = XCurSegBase
         ExtenderFullSize = ExtenderSize+200
IFDEF EDebug
segment DBText
        DebuggerStart = XCurSegBaseR
        SetupSeg 9000, 9000, DBText
        ends
ENDIF
segment IEText  para use32 public 'LOADER'
        LoaderStart  = XCurSegBaseR
        TXCurSegBase = TXCurSegBase + XCurSegBase
        XCurSegBase = 0
IFDEF XLE
        SetupSeg 1113, 2000, IEText, EData
ELSE
        SetupSeg 1536, 2000, IEText, EData
ENDIF
        ends
segment IEData  para use32 public 'LOADER'
        SetupSeg 313, 500, IEData, IEText
        ends
        ROffLoaderEnd = XCurSegBaseR
        WinSize = (ROffLoaderEnd+0FFFh+200h) and not 0FFFh
        LoaderSize = XCurSegBase
        LoaderFullSize = LoaderSize+3000
VSegment IEBSS, 3008, 3008
        ;IDPMI host rezident code
segment Text    para use32 public 'CODE'
        OffProtectedStart = XCurSegBaseR
        XCurSegBase = KernelBase
        SetupRTSeg 6817, 9000, Text, IEData
ends Text
segment IText   para use32 public 'CODE'
        SetupSeg 0000, 000, IText, Text
ends IText
segment Data    para use32 public 'DATA'
        SetupSeg 364, 600, Data, IText
ends Data
VSegment BSS, 9F00h, 9F00h
segment Stock   para  public 'XXXX'
          nStock = (OffLastInit-KernelBase) + WinSize+2000h + ROffRStackEnd - XCurSegBaseR
          db nStock dup(?)
          XCurSegBaseR = XCurSegBaseR + nStock
          ends
segment Stack16 para use16 stack 'STACK'
        StackSegBaseR = XCurSegBaseR
        DB  200h dup (?)
        ;DB (OffLastInit - OffFirstZeroInit+0FFFh) and 0FF000h dup(?)
ends Stack16
GROUP DGROUP16 Text16, Data16, IText16, IData16, RelocR0;, Reloc0, Stack16, RelocREnd, RelocEnd
GROUP EGroup EText, Edata
GROUP LGROUP IEData, IEText
GROUP DGROUP  Text, Data
LastRelocR = 0
LastReloc  = 0
CurRTNum   = 0
_RRTEntryes = 0

