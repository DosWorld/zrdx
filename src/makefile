# ZRDX 0.51OSE build file for Open Watcom wmake
#
# Tools (must be in PATH):
#   NASM 0.98.39 (other versions may produce different OMF objects),
#   Open Watcom 1.9 or later: wmake, wlink, wcl. Set WATCOM environment variable.
#   UPX (packs the stubs, zrxsetup.exe and zrxbind.com).
#
# Usage:
#   wmake                      build zrxbind.com, zrxsetup.exe and the stubs (stub-le.exe, stub-pe.exe, stub-rdf.exe) in binder\
#   wmake STUB=zrdx.exe        the same, but use the unpacked LE stub
#   wmake install              copy everything from binder\ and zrxsetup.exe to ..\bin
#   wmake clean                remove intermediate files and built programs
#
# zrxbind.com reads the stubs (stub-le.exe, stub-pe.exe, stub-rdf.exe) from the directory
# where zrxbind.com itself lives, so they must be copied together.
#
# Settings of a bound program (zrxsetup) are stored in the last 14 bytes of
# the program (signature "ZRXC", written by zrxbind), so the stub may be packed.

NASMOPT = -Ox
NASM    = nasm $(NASMOPT) -f obj
NASMBIN = nasm $(NASMOPT) -f bin
WLINK   = wlink option quiet
WCL     = wcl -zq -ox
UPX     = upx -9 --8086
STUB    = zrdxu.exe
STUBSZ  = 29384
PSTUBSZ = 13312

all: binder\zrxbind.com zrxsetup.exe binder\stub-le.exe binder\stub-pe.exe binder\stub-rdf.exe .SYMBOLIC

binder\zrxbind.com: binder\bnd.asm autolbl.inc
	$(NASMBIN) binder\bnd.asm -o binder\zrxbind.com
	-$(UPX) binder\zrxbind.com

binder\stub-le.exe: $(STUB)
	copy $(STUB) binder\stub-le.exe

binder\stub-pe.exe: zrdxpu.exe
	copy zrdxpu.exe binder\stub-pe.exe

binder\stub-rdf.exe: zrdxru.exe
	copy zrdxru.exe binder\stub-rdf.exe

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

zrdxpu.exe: zrdxpe0.exe aligner.exe
	-del zrdxpu.exe
	-del zrdxrdf0.exe
	-del zrdxru.exe
	$(UPX) -o zrdxpu.exe zrdxpe0.exe
	aligner.exe zrdxpu.exe $(PSTUBSZ)

zrdxpe0.exe: zrdxpe.obj
	$(WLINK) name zrdxpe0.exe format dos file zrdxpe.obj

zrdxru.exe: zrdxrdf0.exe aligner.exe
	-del zrdxru.exe
	$(UPX) -o zrdxru.exe zrdxrdf0.exe
	aligner.exe zrdxru.exe $(PSTUBSZ)

zrdxrdf0.exe: zrdxrdf.obj
	$(WLINK) name zrdxrdf0.exe format dos file zrdxrdf.obj

zrdx0.exe: zrdx.obj
	$(WLINK) name zrdx0.exe format dos file zrdx.obj

zrdx.obj: zrdx.asm prot.inc segdefs.asm layout.inc traps.inc autolbl.inc &
          protdata.asm closerec.asm prothand.asm realhand.asm gmacros.asm &
          dpmifunc.asm vcpiemu.asm mem.asm protinit.asm realinit.asm &
          leldr.asm extender.asm smm.asm vmm.asm segend.asm
	$(NASM) zrdx.asm -o zrdx.obj

zrdxpe.obj: zrdxpe.asm prot.inc segdefs.asm layout.inc traps.inc autolbl.inc &
          protdata.asm closerec.asm prothand.asm realhand.asm gmacros.asm &
          dpmifunc.asm vcpiemu.asm mem.asm protinit.asm realinit.asm &
          peldr.asm extender.asm smm.asm vmm.asm segend.asm
	$(NASM) zrdxpe.asm -o zrdxpe.obj

zrdxrdf.obj: zrdxrdf.asm prot.inc segdefs.asm layout.inc traps.inc autolbl.inc &
          protdata.asm closerec.asm prothand.asm realhand.asm gmacros.asm &
          dpmifunc.asm vcpiemu.asm mem.asm protinit.asm realinit.asm &
          rdfldr.asm extender.asm smm.asm vmm.asm segend.asm
	$(NASM) zrdxrdf.asm -o zrdxrdf.obj

install: binder\zrxbind.com zrxsetup.exe binder\stub-le.exe binder\stub-pe.exe binder\stub-rdf.exe .SYMBOLIC
	copy binder\zrxbind.com ..\bin\zrxbind.com
	copy binder\stub-le.exe ..\bin\stub-le.exe
	copy binder\stub-pe.exe ..\bin\stub-pe.exe
	copy binder\stub-rdf.exe ..\bin\stub-rdf.exe
	copy zrxsetup.exe ..\bin\zrxsetup.exe

clean: .SYMBOLIC
	-del *.obj
	-del *.err
	-del *.map
	-del *.o
	-del zrdx0.exe
	-del zrdx.exe
	-del zrdxu.exe
	-del zrdxpe0.exe
	-del zrdxpu.exe
	-del zrdxrdf0.exe
	-del zrdxru.exe
	-del aligner.exe
	-del zrxsetup.exe
	-del binder\zrxbind.com
	-del binder\stub-le.exe
	-del binder\stub-pe.exe
	-del binder\stub-rdf.exe
