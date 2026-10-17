@echo off
if "%ZB%"=="" set ZB=..\..\bin
nasm -f bin HOST.ASM -o HOST.COM
if errorlevel 1 goto end
HOST.COM > HOST.LOG
for %%n in (PROBE UNSUP CBACK PHYSMAP) do call ..\rdf\ONE %%n
PROBE.EXE > PROBE.LOG
if errorlevel 1 echo PROBE found conformance failures; read PROBE.LOG.
UNSUP.EXE > UNSUP.LOG
if errorlevel 1 echo UNSUP failed; read UNSUP.LOG.
:end
CBACK.EXE > CBACK.LOG
PHYSMAP.EXE > PHYSMAP.LOG
for %%n in (MAPEXIT CBEXIT) do nasm -f bin %%n.ASM -o %%n.COM
MAPEXIT.COM > MAPEXIT.LOG
CBEXIT.COM > CBEXIT.LOG
