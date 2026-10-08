@echo off
nasm -f rdf -o %1.rdo %1.asm
if errorlevel 1 goto err
rlink le /zrdx /o=%1L.exe %1.rdo
if errorlevel 1 goto err
%ZB%\zrxbind -r -n -y %1L.exe
if errorlevel 1 goto err
%ZB%\zrxsetup /B0 %1L.exe
if errorlevel 1 goto err
goto end
:err
echo *** BUILD FAILED: %1L
:end
