@echo off
%1.EXE >> RUN.LOG
if not errorlevel 255 echo *** %1 FAILED: file accepted>>RUN.LOG
if errorlevel 255 echo %1 rejected: ok >> RUN.LOG
