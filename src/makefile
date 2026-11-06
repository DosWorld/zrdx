# ZRDX OSE build file for Open Watcom wmake
#
# Tools (must be in PATH):
#   NASM 0.98.39 (other versions may produce different OMF objects),
#   Open Watcom 1.9 or later: wmake, wlink, wcl. Set WATCOM environment variable.
#   UPX (packs the stubs, zrdxconf.exe and zrdxbind.com).
#
# Usage:
#   wmake                      build zrdxbind.com, zrdxconf.exe,
#                              zrdxdpmi.exe (host + PE loader)
#                              and the stubs (stub-le.exe,
#                              stub-pe.exe, stub-pmx.exe,
#                              stub-rdf.exe) in binder\
#   Stubs are packed to 14 KB, stub-pmx to 18 KB; see
#   docs/D09WORK.MD and docs/BUILD.MD.
#   wmake install              copy everything from binder\ and zrdxconf.exe to ..\bin
#   wmake clean                remove intermediate files and built programs
#
# zrdxbind.com reads the stubs (stub-le.exe, stub-pe.exe, stub-rdf.exe) from the directory
# where zrdxbind.com itself lives, so they must be copied together.
#
# Settings of a bound program (zrdxconf) are stored in the last 14 bytes of
# the program (signature "ZRDX", written by zrdxbind), so the stub may be packed.

NASMOPT = -O9
NASM    = nasm $(NASMOPT) -f obj
NASMBIN = nasm $(NASMOPT) -f bin
WLINK   = wlink option quiet
WCL     = wcl -zq -ox
UPX     = upx -9 --8086
STUB    = zrdxu.exe
PESTUB  = zrdxpu.exe
PMXSTUB = zrdxpmu.exe
RDFSTUB = zrdxru.exe
# Binder requires whole 512-byte pages; round the old 29384 ceiling down.
STUBSZ  = 29184
# User-approved 14 KB packed-file ceiling; runtime limits are unchanged.
PSTUBSZ = 14336
# Approved 2026-11-05: stub-pmx (PE DLL service) only; 36 pages.
PMXSZ   = 18432

all: binder\zrdxbind.com zrdxconf.exe zrdxdpmi.exe &
	binder\stub-le.exe binder\stub-pe.exe binder\stub-pmx.exe &
	binder\stub-rdf.exe .SYMBOLIC

binder\zrdxbind.com: binder\bnd.asm autolbl.inc
	$(NASMBIN) binder\bnd.asm -o binder\zrdxbind.com
	-$(UPX) binder\zrdxbind.com

binder\stub-le.exe: $(STUB)
	copy $(STUB) binder\stub-le.exe

binder\stub-pe.exe: $(PESTUB)
	copy $(PESTUB) binder\stub-pe.exe

binder\stub-pmx.exe: $(PMXSTUB)
	copy $(PMXSTUB) binder\stub-pmx.exe

binder\stub-rdf.exe: $(RDFSTUB)
	copy $(RDFSTUB) binder\stub-rdf.exe

zrdxconf.exe: zrdxconf.c
	$(WCL) -ml -fe=zrdxconf.exe zrdxconf.c
	$(UPX) zrdxconf.exe

aligner.exe: aligner\aligner.c
	$(WCL) -ml -fe=aligner.exe aligner\aligner.c

zrdx.exe: zrdx0.exe aligner.exe makefile
	copy zrdx0.exe zrdx.exe
	aligner.exe zrdx.exe $(STUBSZ)

zrdxu.exe: zrdx0.exe aligner.exe makefile
	-del zrdxu.exe
	$(UPX) -o zrdxu.exe zrdx0.exe
	aligner.exe zrdxu.exe $(PSTUBSZ)

zrdxpe.exe: zrdxpe0.exe aligner.exe makefile
	copy zrdxpe0.exe zrdxpe.exe
	aligner.exe zrdxpe.exe $(STUBSZ)

zrdxrdf.exe: zrdxrdf0.exe aligner.exe makefile
	copy zrdxrdf0.exe zrdxrdf.exe
	aligner.exe zrdxrdf.exe $(STUBSZ)

zrdxpu.exe: zrdxpe0.exe aligner.exe makefile
	-del zrdxpu.exe
	-del zrdxpm0.exe
	-del zrdxpmu.exe
	$(UPX) -o zrdxpu.exe zrdxpe0.exe
	aligner.exe zrdxpu.exe $(PSTUBSZ)

zrdxpmu.exe: zrdxpm0.exe aligner.exe makefile
	-del zrdxpmu.exe
	$(UPX) -o zrdxpmu.exe zrdxpm0.exe
	aligner.exe zrdxpmu.exe $(PMXSZ)

zrdxpm0.exe: zrdxpm.obj
	$(WLINK) name zrdxpm0.exe format dos file zrdxpm.obj

zrdxpe0.exe: zrdxpe.obj
	$(WLINK) name zrdxpe0.exe format dos file zrdxpe.obj

zrdxru.exe: zrdxrdf0.exe aligner.exe makefile
	-del zrdxru.exe
	$(UPX) -o zrdxru.exe zrdxrdf0.exe
	aligner.exe zrdxru.exe $(PSTUBSZ)

# Product: host + PE loader + DLL service in one image (NATPE.MD).
zrdxdpmi.exe: zrdxnp.obj
	$(WLINK) name zrdxdpmi.exe format dos file zrdxnp.obj

zrdxrdf0.exe: zrdxrdf.obj
	$(WLINK) name zrdxrdf0.exe format dos file zrdxrdf.obj

zrdx0.exe: zrdx.obj
	$(WLINK) name zrdx0.exe format dos file zrdx.obj

zrdx.obj: zrdx.asm prot.inc segdefs.asm layout.inc traps.inc autolbl.inc &
	protdata.asm closerec.asm prothand.asm realhand.asm gmacros.asm &
	dpmifunc.asm vcpiemu.asm mem.asm protinit.asm realinit.asm &
	leldr.asm extender.asm smm.asm swapmm.asm segend.asm
	$(NASM) zrdx.asm -o zrdx.obj

zrdxpe.obj: zrdxpe.asm prot.inc segdefs.asm layout.inc traps.inc autolbl.inc &
	protdata.asm closerec.asm prothand.asm realhand.asm gmacros.asm &
	dpmifunc.asm vcpiemu.asm mem.asm protinit.asm realinit.asm &
	peldr.asm peload.asm extender.asm smm.asm swapmm.asm segend.asm
	$(NASM) zrdxpe.asm -o zrdxpe.obj

zrdxpm.obj: zrdxpm.asm zrdxpe.asm prot.inc segdefs.asm layout.inc &
	traps.inc autolbl.inc protdata.asm closerec.asm prothand.asm &
	realhand.asm gmacros.asm dpmifunc.asm vcpiemu.asm mem.asm &
	protinit.asm realinit.asm peldr.asm peload.asm pereg.asm &
	peexp.asm peimp.asm extender.asm &
	smm.asm swapmm.asm segend.asm
	$(NASM) zrdxpm.asm -o zrdxpm.obj

zrdxnp.obj: zrdxnp.asm zrdxdpmi.asm prot.inc segdefs.asm layout.inc &
	traps.inc autolbl.inc protdata.asm closerec.asm prothand.asm &
	realhand.asm gmacros.asm dpmifunc.asm vcpiemu.asm mem.asm &
	protinit.asm realinit.asm nathost.asm peldr.asm peload.asm &
	pereg.asm peexp.asm peimp.asm extender.asm vendor.asm smm.asm &
	swapmm.asm segend.asm
	$(NASM) zrdxnp.asm -o zrdxnp.obj

zrdxdpmi.obj: zrdxdpmi.asm prot.inc segdefs.asm layout.inc &
	traps.inc autolbl.inc protdata.asm closerec.asm &
	prothand.asm realhand.asm gmacros.asm dpmifunc.asm &
	vcpiemu.asm mem.asm protinit.asm realinit.asm &
	nathost.asm smm.asm swapmm.asm segend.asm
	$(NASM) zrdxdpmi.asm -o zrdxdpmi.obj

zrdxrdf.obj: zrdxrdf.asm prot.inc segdefs.asm layout.inc traps.inc autolbl.inc &
	protdata.asm closerec.asm prothand.asm realhand.asm gmacros.asm &
	dpmifunc.asm vcpiemu.asm mem.asm protinit.asm realinit.asm &
	rdfldr.asm extender.asm smm.asm swapmm.asm segend.asm
	$(NASM) zrdxrdf.asm -o zrdxrdf.obj

install: binder\zrdxbind.com zrdxconf.exe zrdxdpmi.exe &
	binder\stub-le.exe binder\stub-pe.exe binder\stub-pmx.exe &
	binder\stub-rdf.exe .SYMBOLIC
	copy binder\zrdxbind.com ..\bin\zrdxbind.com
	copy binder\stub-le.exe ..\bin\stub-le.exe
	copy binder\stub-pe.exe ..\bin\stub-pe.exe
	copy binder\stub-pmx.exe ..\bin\stub-pmx.exe
	copy binder\stub-rdf.exe ..\bin\stub-rdf.exe
	copy zrdxconf.exe ..\bin\zrdxconf.exe
	copy zrdxdpmi.exe ..\bin\zrdxdpmi.exe

clean: .SYMBOLIC
	-del *.obj
	-del *.err
	-del *.map
	-del *.o
	-del zrdx0.exe
	-del zrdx.exe
	-del zrdxu.exe
	-del zrdxpe.exe
	-del zrdxrdf.exe
	-del zrdxpe0.exe
	-del zrdxpu.exe
	-del zrdxrdf0.exe
	-del zrdxru.exe
	-del aligner.exe
	-del zrdxconf.exe
	-del zrdxdpmi.exe
	-del binder\zrdxbind.com
	-del binder\stub-le.exe
	-del binder\stub-pe.exe
	-del binder\stub-pmx.exe
	-del binder\stub-rdf.exe
