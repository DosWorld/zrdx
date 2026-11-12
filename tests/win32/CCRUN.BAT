@echo off
rem Task 184: CC32W, LINK32W and LIB32W (Win32 programs of
rem the supplied compiler, ZRDX_CCDIR) run under the service;
rem the programs they build run too. Called by RUN.BAT.
cd CC
copy ..\CCRET.C . > NUL
copy ..\CCPRN.C . > NUL
copy ..\KERNEL32.DLL . > NUL
set ZR=R:\BIN\ZRDXDPMI.EXE
set LK=%ZR% LINK32W.EXE /def=WIN.DEF /s=1024
if exist CCRET.EXE del CCRET.EXE
if exist CCPRN.EXE del CCPRN.EXE
%ZR% CC32W.EXE -target=win32 CCRET.C > CCRET1.LOG
%ZR% CC32W.EXE -target=win32 CCPRN.C > CCPRN1.LOG
%LK% /o=CCRET.EXE CCRET.RDF LIBC32W.RDL > CCRET2.LOG
%LK% /o=CCPRN.EXE CCPRN.RDF LIBC32W.RDL > CCPRN2.LOG
%ZR% CCRET.EXE > CCRET3.LOG
if errorlevel 7 if not errorlevel 8 echo CCRET OK>>..\RESULT.LOG
if errorlevel 8 echo CCRET FAILED >> ..\RESULT.LOG
if not errorlevel 7 echo CCRET FAILED >> ..\RESULT.LOG
%ZR% CCPRN.EXE > CCPRN3.LOG
if errorlevel 1 echo CCPRN FAILED >> ..\RESULT.LOG
if not errorlevel 1 echo CCPRN OK >> ..\RESULT.LOG
echo CCPRN.RDF > LST.TXT
if exist MYLIB.RDL del MYLIB.RDL
%ZR% LIB32W.EXE MYLIB.RDL LST.TXT > CCLIB1.LOG
%ZR% LIB32W.EXE -l MYLIB.RDL > CCLIB2.LOG
%ZR% LIB32W.EXE -l LIBC32W.RDL > CCLIB3.LOG
if errorlevel 1 echo CCLIB FAILED >> ..\RESULT.LOG
if not errorlevel 1 echo CCLIB OK >> ..\RESULT.LOG
cd ..
