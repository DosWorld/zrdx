;             This file is part of the ZRDX 0.50OSE project
;                     (C) 1998, Sergey Belyakov
;                     (C) 2026, Viacheslav Komenda
;NASM version
cpu 386
%include "autolbl.inc"
%include "prot.inc"
%include "gmacros.asm"
%include "segdefs.asm"
%include "layout.inc"
%include "protdata.asm"
%include "realhand.asm"
%include "protinit.asm"
%include "vcpiemu.asm"
%include "dpmifunc.asm"
%include "mem.asm"
%ifdef VMM
%include "vmm.asm"
%else
%include "smm.asm"
%endif
%include "prothand.asm"
%include "loader.asm"
%ifdef EDebug
        SEGM DBText
%include "debugger.asm"
        ESEG DBText
%endif
%include "extender.asm"
%include "realinit.asm"
%include "closerec.asm"
%include "segend.asm"
