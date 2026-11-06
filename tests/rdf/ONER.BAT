@echo off
%ZB%\zrdxbind -y %1.rdo %1R.exe
if errorlevel 1 goto err
%ZB%\zrdxconf /B0 %1R.exe
if errorlevel 1 goto err
goto end
:err
echo *** BUILD FAILED: %1R
:end
