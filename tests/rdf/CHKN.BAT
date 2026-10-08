@echo off
%1.EXE >> RUN.LOG
if not errorlevel 255 echo *** %1 FAILED: loader accepted the file >> RUN.LOG
if errorlevel 255 echo %1 rejected: ok >> RUN.LOG
