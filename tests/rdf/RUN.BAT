@echo off
if exist RUN.LOG del RUN.LOG
for %%n in (T01 T03 T04 T05 T06 T07 T08 T09 T10 T11 T12 T14 T15 T16) do call chk %%n
call chk T13 ABC
for %%n in (T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32 T33 T34 T35 T36 T37 T40 T50 T51 T52 T53 T54 T55 T56 T57) do call chk %%n
for %%n in (T03R T04R T05R T06R T07R T09R T11R T12R T14R T15R T16R) do call chk %%n
call chk T13R ABC
for %%n in (T17R T18R T19R T20R T21R T22R T23R T24R T25R T26R T28R T29R T30R T31R T32R T33R T34R T35R T36R T37R T40R T50R T51R T52R T53R T54R T55R T56R T57R) do call chk %%n
T02.EXE
if errorlevel 42 if not errorlevel 43 echo T02 exit code: ok >> RUN.LOG
if not errorlevel 42 echo *** T02 FAILED >> RUN.LOG
if errorlevel 43 echo *** T02 FAILED >> RUN.LOG
for %%n in (N01 N02 N03) do call chkn %%n
type RUN.LOG
