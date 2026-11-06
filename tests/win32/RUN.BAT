@echo off
rem Win32 layer tests (tasks 178+): R:\BIN is the release.
if exist RESULT.LOG del RESULT.LOG
cd ..\..\win32
call BUILD.BAT
cd ..\tests\win32
copy ..\..\win32\KERNEL32.DLL . > NUL
wcc386 -zq -s -ecc -bt=nt -i=..\..\sdk T1.C
wlink system nt name T1.EXE file T1.OBJ option start=_mainCRTStartup option nodefaultlibs > BUILD.LOG
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
wlink system nt name T2.EXE file T2.OBJ option start=_mainCRTStartup option nodefaultlibs > BUILD2.LOG
R:\BIN\ZRDXDPMI.EXE T2.EXE > T2.LOG
wcc386 -zq -s -ecc -bt=nt TAPI.C
wlink system nt name TAPI.EXE file TAPI.OBJ option start=_mainCRTStartup option nodefaultlibs > BUILDA.LOG
R:\BIN\ZRDXDPMI.EXE TAPI.EXE > TAPI.LOG
if errorlevel 1 echo TAPI FAILED >> RESULT.LOG
if not errorlevel 1 echo TAPI OK >> RESULT.LOG
R:\BIN\ZRDXBIND -y W.EXE WB.EXE > NUL
WB.EXE > WB.LOG
if errorlevel 1 echo WB FAILED >> RESULT.LOG
if not errorlevel 1 echo WB OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TPROC.C
wlink system nt name TPROC.EXE file TPROC.OBJ option start=_mainCRTStartup option nodefaultlibs > BUILDP.LOG
R:\BIN\ZRDXDPMI.EXE TPROC.EXE > TPROC.LOG
if errorlevel 1 echo TPROC FAILED >> RESULT.LOG
if not errorlevel 1 echo TPROC OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TMEM.C
wlink system nt name TMEM.EXE file TMEM.OBJ option start=_mainCRTStartup option nodefaultlibs > BUILDM.LOG
R:\BIN\ZRDXDPMI.EXE TMEM.EXE > TMEM.LOG
if errorlevel 1 echo TMEM FAILED >> RESULT.LOG
if not errorlevel 1 echo TMEM OK >> RESULT.LOG
if exist ZRDXD\\*.* del ZRDXD\\*.* > NUL
if exist ZRDXD rd ZRDXD
wcc386 -zq -s -ecc -bt=nt TFS.C
wlink system nt name TFS.EXE file TFS.OBJ option start=_mainCRTStartup option nodefaultlibs > BUILDF.LOG
R:\BIN\ZRDXDPMI.EXE TFS.EXE > TFS.LOG
if errorlevel 1 echo TFS FAILED >> RESULT.LOG
if not errorlevel 1 echo TFS OK >> RESULT.LOG
wcc386 -zq -s -ecc -bt=nt TTXT.C
wlink system nt name TTXT.EXE file TTXT.OBJ option start=_mainCRTStartup option nodefaultlibs > BUILDT.LOG
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
