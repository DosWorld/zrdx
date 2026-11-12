;             This file is part of the ZRDX OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda
;NASM version

RRTEntryes equ _RRTEntryes
segment Stock
;end of the real mode data (BSS16 may grow past RStackEnd)
RMBSSEnd equ BSS16LC16
%assign nStock (OffLastInit - KernelBase) + WinSize + 2000h \
        + BSS16LC16 - XCurSegBaseR
        resb nStock
%assign XCurSegBaseR XCurSegBaseR + nStock
segment Stack16
StackSegBaseR equ XCurSegBaseR
        resb 200h
