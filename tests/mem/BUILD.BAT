@echo off
if "%ZB%"=="" set ZB=..\..\bin
wcl386 -zq -bt=dos -l=dos4g -fe=memd.exe mem.c
wcl386 -zq -bt=dos -l=causeway -fe=memc.exe mem.c
copy memd.exe memz.exe > nul
%ZB%\zrxbind -y memd.exe memz.exe
