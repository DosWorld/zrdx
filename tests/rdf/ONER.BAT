@echo off
%ZB%\zrxbind -y %1.rdo %1R.exe
if errorlevel 1 goto err
%ZB%\zrxsetup /B0 %1R.exe
if errorlevel 1 goto err
goto end
:err
echo *** BUILD FAILED: %1R
:end
