@echo off
rem Task 219: a stub over its limit must stop wmake at once.
if exist X.OUT del X.OUT
if exist ALTEST.RES del ALTEST.RES
wmake -f ALTEST.MAK > ALTEST.LOG
if errorlevel 1 echo ALIGN OK > ALTEST.RES
