@echo off
rem Win32 layer tests (tasks 178+): R:\BIN is the release.
if exist RESULT.LOG del RESULT.LOG
cd ..\..\win32
call BUILD.BAT
cd ..\tests\win32
copy ..\..\win32\KERNEL32.DLL . > NUL
wcc386 -zq -s -ecc -bt=nt -i=..\..\sdk T1.C
call LNK T1 BUILD.LOG
R:\BIN\ZRDXDPMI.EXE T1.EXE > T1.LOG
if errorlevel 5 if not errorlevel 6 echo T1 OK >> RESULT.LOG
if not errorlevel 5 echo T1 FAILED >> RESULT.LOG
if errorlevel 6 echo T1 FAILED >> RESULT.LOG
copy ..\..\win32\USER32.DLL . > NUL
wcl386 -zq -bt=nt -l=nt W.C
R:\BIN\ZRDXDPMI.EXE W.EXE > W.LOG
if errorlevel 1 echo W FAILED >> RESULT.LOG
if not errorlevel 1 echo W OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt T2.C
call LNK T2 BUILD2.LOG
R:\BIN\ZRDXDPMI.EXE T2.EXE > T2.LOG
wcc386 -zq -s -ecc -bt=nt TAPI.C
call LNK TAPI BUILDA.LOG
R:\BIN\ZRDXDPMI.EXE TAPI.EXE > TAPI.LOG
if errorlevel 1 echo TAPI FAILED >> RESULT.LOG
if not errorlevel 1 echo TAPI OK >> RESULT.LOG
R:\BIN\ZRDXBIND -y W.EXE WB.EXE > NUL
WB.EXE > WB.LOG
if errorlevel 1 echo WB FAILED >> RESULT.LOG
if not errorlevel 1 echo WB OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TPROC.C
call LNK TPROC BUILDP.LOG
R:\BIN\ZRDXDPMI.EXE TPROC.EXE > TPROC.LOG
if errorlevel 1 echo TPROC FAILED >> RESULT.LOG
if not errorlevel 1 echo TPROC OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TTEB.C
call LNK TTEB BUILDB.LOG
R:\BIN\ZRDXDPMI.EXE TTEB.EXE > TTEB.LOG
if errorlevel 1 echo TTEB FAILED >> RESULT.LOG
if not errorlevel 1 echo TTEB OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TSEH.C
call LNK TSEH BUILDX.LOG
R:\BIN\ZRDXDPMI.EXE TSEH.EXE > TSEH.LOG
if errorlevel 1 echo TSEH FAILED >> RESULT.LOG
if not errorlevel 1 echo TSEH OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TUNW.C
call LNK TUNW BUILDZ.LOG
R:\BIN\ZRDXDPMI.EXE TUNW.EXE > TUNW.LOG
if errorlevel 1 echo TUNW FAILED >> RESULT.LOG
if not errorlevel 1 echo TUNW OK >> RESULT.LOG
wcl -zq -ml -fe=ADDTLS.EXE ADDTLS.C
wcc386 -zq -s -ecc -bt=nt TTLS.C
call LNK TTLS BUILDT.LOG
ADDTLS.EXE TTLS.EXE
R:\BIN\ZRDXDPMI.EXE TTLS.EXE > TTLS.LOG
if errorlevel 1 echo TTLS FAILED >> RESULT.LOG
if not errorlevel 1 echo TTLS OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TTLSD.C
wlink @TTLSD.LNK > BUILDL.LOG
ADDTLS.EXE TTLSD.DLL
wcc386 -zq -s -ecc -bt=nt TTLD.C
wlink @TTLD.LNK > BUILDL2.LOG
R:\BIN\ZRDXDPMI.EXE TTLD.EXE > TTLD.LOG
if errorlevel 1 echo TTLD FAILED >> RESULT.LOG
if not errorlevel 1 echo TTLD OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TSEHX.C
call LNK TSEHX BUILDY.LOG
R:\BIN\ZRDXDPMI.EXE TSEHX.EXE > TSEHX.LOG
if errorlevel 148 if not errorlevel 149 echo TSEHX OK>>RESULT.LOG
if errorlevel 149 echo TSEHX FAILED >> RESULT.LOG
if not errorlevel 148 echo TSEHX FAILED >> RESULT.LOG
if exist CC\CC32W.EXE call CCRUN.BAT
wcc386 -zq -s -ecc -bt=nt TUSR.C
call LNK TUSR BUILDU.LOG
R:\BIN\ZRDXDPMI.EXE TUSR.EXE > TUSR.LOG
if errorlevel 1 echo TUSR FAILED >> RESULT.LOG
if not errorlevel 1 echo TUSR OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TKBS.C
wlink @TKBS.LNK > BUILDK.LOG
R:\BIN\ZRDXDPMI.EXE TKBS.EXE > TKBS.LOG
if errorlevel 1 echo TKBS FAILED >> RESULT.LOG
if not errorlevel 1 echo TKBS OK >> RESULT.LOG
copy ..\..\win32\NTDLL.DLL . > NUL
wcc386 -zq -s -ecc -bt=nt TNTD.C
wlink @TNTD.LNK > BUILDN.LOG
R:\BIN\ZRDXDPMI.EXE TNTD.EXE > TNTD.LOG
if errorlevel 1 echo TNTD FAILED >> RESULT.LOG
if not errorlevel 1 echo TNTD OK >> RESULT.LOG
copy ..\..\win32\ADVAPI32.DLL . > NUL
wcc386 -zq -s -ecc -bt=nt TADV.C
wlink @TADV.LNK > BUILDV.LOG
R:\BIN\ZRDXDPMI.EXE TADV.EXE > TADV.LOG
if errorlevel 1 echo TADV FAILED >> RESULT.LOG
if not errorlevel 1 echo TADV OK >> RESULT.LOG
copy ..\..\win32\SHELL32.DLL . > NUL
wcc386 -zq -s -ecc -bt=nt TSHL.C
wlink @TSHL.LNK > BUILDH.LOG
R:\BIN\ZRDXDPMI.EXE TSHL.EXE > TSHL.LOG
if errorlevel 1 echo TSHL FAILED >> RESULT.LOG
if not errorlevel 1 echo TSHL OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TMFN.C
call LNK TMFN BUILDM2.LOG
R:\BIN\ZRDXDPMI.EXE TMFN.EXE > TMFN.LOG
if errorlevel 1 echo TMFN FAILED >> RESULT.LOG
if not errorlevel 1 echo TMFN OK >> RESULT.LOG
R:\BIN\ZRDXBIND -y TMFN.EXE TMFB.EXE > NUL
TMFB.EXE > TMFB.LOG
if errorlevel 1 echo TMFB FAILED >> RESULT.LOG
if not errorlevel 1 echo TMFB OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TMEM.C
call LNK TMEM BUILDM.LOG
R:\BIN\ZRDXDPMI.EXE TMEM.EXE > TMEM.LOG
if errorlevel 1 echo TMEM FAILED >> RESULT.LOG
if not errorlevel 1 echo TMEM OK >> RESULT.LOG
if exist ZRDXD\\*.* del ZRDXD\\*.* > NUL
if exist ZRDXD rd ZRDXD
wcc386 -zq -s -ecc -bt=nt TFS.C
call LNK TFS BUILDF.LOG
R:\BIN\ZRDXDPMI.EXE TFS.EXE > TFS.LOG
if errorlevel 1 echo TFS FAILED >> RESULT.LOG
if not errorlevel 1 echo TFS OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TTXT.C
call LNK TTXT BUILDT.LOG
R:\BIN\ZRDXDPMI.EXE TTXT.EXE > TTXT.LOG
if errorlevel 1 echo TTXT FAILED >> RESULT.LOG
if not errorlevel 1 echo TTXT OK >> RESULT.LOG
copy ..\..\win32\MSVCRT.DLL . > NUL
if exist TCRT.EXE del TCRT.EXE
wcc386 -zq -s -ecc -bt=nt -fr=TCRT.ERR TCRT.C
wlink @TCRT.LNK > BUILDC.LOG
R:\BIN\ZRDXDPMI.EXE TCRT.EXE a "b c" d\\e > TCRT.LOG
if errorlevel 7 if not errorlevel 8 echo TCRT OK >> RESULT.LOG
if not errorlevel 7 echo TCRT FAILED >> RESULT.LOG
if errorlevel 8 echo TCRT FAILED >> RESULT.LOG
if exist TIO.EXE del TIO.EXE
wcc386 -zq -s -ecc -bt=nt -fr=TIO.ERR TIO.C
wlink @TIO.LNK > BUILDI.LOG
R:\BIN\ZRDXDPMI.EXE TIO.EXE > TIO.LOG
if errorlevel 1 echo TIO FAILED >> RESULT.LOG
if not errorlevel 1 echo TIO OK >> RESULT.LOG
if exist TFILE.EXE del TFILE.EXE
wcc386 -zq -s -ecc -bt=nt -fr=TFILE.ERR TFILE.C
wlink @TFILE.LNK > BUILDL.LOG
R:\BIN\ZRDXDPMI.EXE TFILE.EXE > TFILE.LOG
if errorlevel 1 echo TFILE FAILED >> RESULT.LOG
if not errorlevel 1 echo TFILE OK >> RESULT.LOG
if exist TFMT.EXE del TFMT.EXE
wcc386 -zq -s -ecc -fpi87 -bt=nt -fr=TFMT.ERR TFMT.C
wlink @TFMT.LNK > BUILDF2.LOG
R:\BIN\ZRDXDPMI.EXE TFMT.EXE > TFMT.LOG
if errorlevel 1 echo TFMT FAILED >> RESULT.LOG
if not errorlevel 1 echo TFMT OK >> RESULT.LOG
if exist TSCAN.EXE del TSCAN.EXE
wcc386 -zq -s -ecc -fpi87 -bt=nt -fr=TSCAN.ERR TSCAN.C
wlink @TSCAN.LNK > BUILDS.LOG
R:\BIN\ZRDXDPMI.EXE TSCAN.EXE > TSCAN.LOG
if errorlevel 1 echo TSCAN FAILED >> RESULT.LOG
if not errorlevel 1 echo TSCAN OK >> RESULT.LOG
if exist TLIB.EXE del TLIB.EXE
wcc386 -zq -s -ecc -fpi87 -bt=nt -fr=TLIB.ERR TLIB.C
wlink @TLIB.LNK > BUILDB.LOG
R:\BIN\ZRDXDPMI.EXE TLIB.EXE > TLIB.LOG
if errorlevel 1 echo TLIB FAILED >> RESULT.LOG
if not errorlevel 1 echo TLIB OK >> RESULT.LOG
if exist TMATH.EXE del TMATH.EXE
wcc386 -zq -s -ecc -fpi87 -bt=nt -fr=TMATH.ERR TMATH.C
wlink @TMATH.LNK > BUILDC.LOG
R:\BIN\ZRDXDPMI.EXE TMATH.EXE > TMATH.LOG
if errorlevel 1 echo TMATH FAILED >> RESULT.LOG
if not errorlevel 1 echo TMATH OK >> RESULT.LOG
if exist TWCS.EXE del TWCS.EXE
wcc386 -zq -s -ecc -fpi87 -bt=nt -fr=TWCS.ERR TWCS.C
wlink @TWCS.LNK > BUILDD.LOG
R:\BIN\ZRDXDPMI.EXE TWCS.EXE > TWCS.LOG
if errorlevel 1 echo TWCS FAILED >> RESULT.LOG
if not errorlevel 1 echo TWCS OK >> RESULT.LOG
copy ..\..\win32\UCRTBASE.DLL . > NUL
if exist TUCRT.EXE del TUCRT.EXE
wcc386 -zq -s -ecc -fpi87 -bt=nt -fr=TUCRT.ERR TUCRT.C
wlink @TUCRT.LNK > BUILDE.LOG
R:\BIN\ZRDXDPMI.EXE TUCRT.EXE > TUCRT.LOG
if errorlevel 1 echo TUCRT FAILED >> RESULT.LOG
if not errorlevel 1 echo TUCRT OK >> RESULT.LOG
wlink @TAMS.LNK > BUILDA2.LOG
R:\BIN\ZRDXDPMI.EXE TAMS.EXE > TAMS.LOG
if errorlevel 1 echo TAMS FAILED >> RESULT.LOG
if not errorlevel 1 echo TAMS OK >> RESULT.LOG
