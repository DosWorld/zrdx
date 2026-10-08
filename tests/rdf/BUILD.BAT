@echo off
if "%ZB%"=="" set ZB=..\..\bin
for %%n in (T01 T02 T03 T04 T05 T06 T07 T09 T11 T12 T13 T14 T15 T16) do call one %%n
for %%n in (T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T28 T29 T30 T31 T32 T33 T34 T35 T36 T37 T40 T50 T51 T52 T53 T54 T55 T56 T57) do call one %%n
for %%n in (T03 T04 T05 T06 T07 T09 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T28 T29 T30 T31 T32 T33 T34 T35 T36 T37 T40 T50 T51 T52 T53 T54 T55 T56 T57) do call oner %%n
call onen T10
call nasm -f rdf -o T08B.RDO T08B.ASM
call one T08A T08B.RDO
ren T08A.EXE T08.EXE
call nasm -f rdf -o T27B.RDO T27B.ASM
call nasm -f rdf -o T27C.RDO T27C.ASM
call one T27A T27B.RDO T27C.RDO
ren T27A.EXE T27.EXE
for %%n in (N01 N02 N03) do call onen %%n
