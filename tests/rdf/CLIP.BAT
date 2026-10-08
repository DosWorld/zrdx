@echo off
if "%ZB%"=="" set ZB=..\..\bin
if exist CLIP.LOG del CLIP.LOG
for %%n in (T34 T35 T36 T37 T40) do call onele %%n
for %%n in (T34L T35L T36L T37L T40L) do call chk %%n
type RUN.LOG
