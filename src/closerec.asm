;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov

Out1    MACRO n,n1
        %OUT n&__&n1
        ENDM

IRP S, <Text16, Data16, RelocR0, IText16, IData16, EText, EData, IEText, IEData, Text, IText, Data>
        S segment
        Out1 S, %($-XSegStart&S)
        if SegSize&S + XSegStart&S - $ NE 0
          DB (SegSize&S + XSegStart&S - $) dup(?)
        endif
        XSegEnd&S  = $
        S ends
        ENDM
