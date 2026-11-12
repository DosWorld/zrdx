@echo off
rem Open Watcom C fixtures without a C runtime (task 165).
rem -s: no stack check call, -ecc: cdecl, nodefaultlibs: no CRT.
rem wlink cuts a module name at the first dot, so PLUGC.DLL
rem imports from the 9 character name HOSTCZEXE, which GEN.PY
rem patches to HOSTC.EXE (the implib option is not reliable).
set F=format windows nt
set S=option nodefaultlibs option start=_mainCRTStartup
set E=export _HostVal %S%
set P=export _PlugVal import _HostVal HOSTCZEXE._HostVal option nodefaultlibs
if exist HOSTC.EXE del HOSTC.EXE
if exist PLUGC.DLL del PLUGC.DLL
wcc386 -zq -s -ecc -bt=nt -i=..\..\..\sdk HOSTC.C
wlink %F% name HOSTC.EXE file HOSTC.OBJ %E% > BUILD.LOG
wcc386 -zq -s -ecc -bt=nt PLUGC.C
wlink %F% dll name PLUGC.DLL file PLUGC.OBJ %P% >> BUILD.LOG
