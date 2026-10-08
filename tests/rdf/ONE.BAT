@echo off
nasm -f rdf -o %1.rdo %1.asm
if errorlevel 1 goto err
rlink rdf /o=%1.rdf %1.rdo %2 %3 %4
if errorlevel 1 goto err
%ZB%\zrxbind -y %1.rdf %1.exe
if errorlevel 1 goto err
%ZB%\zrxsetup /B0 %1.exe
if errorlevel 1 goto err
goto end
:err
echo *** BUILD FAILED: %1
:end
