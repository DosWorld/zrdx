# ZRDX 0.50OSE build file for Open Watcom wmake
#
# Tools (must be in PATH):
#   NASM 0.98.39 (other versions may produce different OMF objects),
#   Open Watcom 1.9 or later: wmake, wlink, wcl. Set WATCOM environment variable.
#   UPX (packs the stub, zrxsetup.exe and, when it helps, zrxbind.exe).
#
# Usage:
#   wmake                      build binder\zrxbind.exe and zrxsetup.exe
#   wmake STUB=zrdx.exe        the same, but the binder embeds the unpacked stub
#   wmake zrdxu.exe            build the UPX packed stub only (it is embedded by default)
#   wmake install              copy the binder, zrxsetup and the stub (stub-le.exe) to ..\bin
#   wmake clean                remove intermediate files and built programs
#
# Settings of a bound program (zrxsetup) are stored in the last 14 bytes of
# the program (signature "ZRXC" written by zrxbind), so the stub may be packed.

NASM    = nasm -O1 -f obj
WLINK   = wlink option quiet
WCL     = wcl -zq -ox
UPX     = upx -9 --8086
STUB    = zrdxu.exe
STUBSZ  = 29384
PSTUBSZ = 12800

all: binder\zrxbind.exe zrxsetup.exe .SYMBOLIC

binder\zrxbind.exe: bnd.obj
	$(WLINK) name binder\zrxbind.exe format dos file bnd.obj
	-$(UPX) binder\zrxbind.exe

bnd.obj: binder\bnd.asm stub0_le.asb autolbl.inc
	$(NASM) binder\bnd.asm -o bnd.obj

stub0_le.asb: $(STUB) bin2asm.exe
	bin2asm.exe $(STUB) stub0_le.asb

bin2asm.exe: bin2asm.c
	$(WCL) -ml -fe=bin2asm.exe bin2asm.c

zrxsetup.exe: zrxsetup.c
	$(WCL) -ml -fe=zrxsetup.exe zrxsetup.c
	$(UPX) zrxsetup.exe

aligner.exe: aligner\aligner.c
	$(WCL) -ml -fe=aligner.exe aligner\aligner.c

zrdx.exe: zrdx0.exe aligner.exe
	copy zrdx0.exe zrdx.exe
	aligner.exe zrdx.exe $(STUBSZ)

zrdxu.exe: zrdx0.exe aligner.exe
	-del zrdxu.exe
	$(UPX) -o zrdxu.exe zrdx0.exe
	aligner.exe zrdxu.exe $(PSTUBSZ)

zrdx0.exe: zrdx.obj
	$(WLINK) name zrdx0.exe format dos file zrdx.obj

zrdx.obj: zrdx.asm prot.inc segdefs.asm layout.inc traps.inc autolbl.inc &
          protdata.asm closerec.asm prothand.asm realhand.asm gmacros.asm &
          dpmifunc.asm vcpiemu.asm mem.asm protinit.asm realinit.asm &
          loader.asm extender.asm smm.asm vmm.asm segend.asm
	$(NASM) zrdx.asm -o zrdx.obj

..\bin\stub-le.exe: $(STUB)
	copy $(STUB) ..\bin\stub-le.exe

install: binder\zrxbind.exe zrxsetup.exe ..\bin\stub-le.exe .SYMBOLIC
	copy binder\zrxbind.exe ..\bin\zrxbind.exe
	copy zrxsetup.exe ..\bin\zrxsetup.exe

clean: .SYMBOLIC
	-del *.obj
	-del *.err
	-del *.map
	-del *.o
	-del zrdx0.exe
	-del zrdx.exe
	-del zrdxu.exe
	-del aligner.exe
	-del bin2asm.exe
	-del zrxsetup.exe
	-del stub0_le.asb
	-del binder\zrxbind.exe
