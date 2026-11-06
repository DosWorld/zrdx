@echo off
nasm -f rdf -o %1.rdo %1.asm
if errorlevel 1 goto err
%ZB%\zrdxbind -y %1.rdo %1.exe
if errorlevel 1 goto err
%ZB%\zrdxconf /B0 %1.exe
if errorlevel 1 goto err
goto end
:err
echo *** BUILD FAILED: %1
:end
