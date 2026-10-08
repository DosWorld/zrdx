@echo off
%1.EXE %2 >> RUN.LOG
if errorlevel 1 echo *** %1 FAILED >> RUN.LOG
