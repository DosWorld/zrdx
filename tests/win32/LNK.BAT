@echo off
rem LNK.BAT name log: link name.obj as a console NT program
set O=option start=_mainCRTStartup option nodefaultlibs
wlink system nt name %1.EXE file %1.OBJ %O% > %2
