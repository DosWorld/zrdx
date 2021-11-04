# You must build this project with Borland C v. 3.1 and Microsoft linker
# define "P" to make packed version, "E" to rebuild XLE stub

binder\zrxbind.exe: binder\bnd.asm stub0_le.asb
  tasm binder\bnd.asm /m
  tlink bnd.obj, binder\zrxbind.exe

stub0_le.asb: zrdx.exe bin2asm.exe
  bin2asm.exe zrdx.exe stub0_le.asb

#stub0_xe.asb: zrdxe.exe bin2asm.exe
#  bin2asm.exe zrdxe.exe stub0_xe.asb

bin2asm.exe: bin2asm.cpp
  #set lib=%WATCOM@%\lib286
  #set include=%WATCOM%\h
  bcc bin2asm.cpp

aligner.exe: aligner\aligner.cpp
  bcc aligner\aligner.cpp

zrdx.exe: zrdx0.exe aligner.exe
!ifdef P
  pack 22 zrdx0.exe zrdx.exe
  aligner zrdx.exe 12048
!else
  copy zrdx0.exe zrdx.exe
  aligner zrdx.exe 29384
!endif

#!ifdef E
#zrdxe.exe: zrdx0e.exe aligner.exe
#!ifdef P
#  pack 18 zrdx0e.exe zrdxe.exe
#  aligner zrdxe.exe 11776
#!else
#  copy zrdx0e.exe zrdxe.exe
#  aligner zrdxe.exe 18384
#!endif
#!endif

zrdx0.exe: zrdx.obj
  link.exe zrdx, zrdx0, zrdx;

#zrdx0e.exe: zrdxe.obj
#  link.exe zrdxe, zrdx0e, zrdxe;

zrdx.obj: prot.inc segdefs.asm autolbl.inc protdata.asm closerec.asm \
          zrdx.asm prothand.asm realhand.asm gmacros.asm dpmifunc.asm \
          vcpiemu.asm mem.asm protinit.asm realinit.asm loader.asm \
          extender.asm smm.asm vmm.asm
  tasmx zrdx.asm /m/l

#zrdxe.obj: prot.inc segdefs.asm autolbl.inc protdata.asm closerec.asm \
#          zrdx.asm prothand.asm realhand.asm gmacros.asm dpmifunc.asm \
#          vcpiemu.asm mem.asm protinit.asm realinit.asm \
#          extender.asm xeload.asm smm.asm vmm.asm
#  tasmx zrdxe.asm /m/l
