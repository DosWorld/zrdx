;             This file is part of the ZRDX 0.50 project
;                     (C) 1998, Sergey Belyakov
XLE = 1
.386P
Locals @@
include autolbl.inc
include prot.inc
include gmacros.asm
include segdefs.asm
include realhand.asm
include protdata.asm
include protinit.asm
include vcpiemu.asm
include dpmifunc.asm
include mem.asm
IFDEF VMM
  include vmm.asm
ELSE
  include smm.asm
ENDIF
include prothand.asm
include xeload.asm
include extender.asm
include realinit.asm
include closerec.asm
RRTEntryes=_RRTEntryes
end RSetup
