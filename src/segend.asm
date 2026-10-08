;             This file is part of the ZRDX 0.50OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda
;NASM version

RRTEntryes equ _RRTEntryes
segment Stock
%assign nStock (OffLastInit - KernelBase) + WinSize + 2000h + ROffRStackEnd - XCurSegBaseR
        resb nStock
%assign XCurSegBaseR XCurSegBaseR + nStock
segment Stack16
StackSegBaseR equ XCurSegBaseR
        resb 200h
