@echo off
if exist RUN.LOG del RUN.LOG
for %%n in (T01 T03 T04 T05 T06 T07 T08 T09 T10) do call chk %%n
for %%n in (T11 T12 T14 T15 T16) do call chk %%n
call chk T13 ABC
for %%n in (T17 T18 T19 T20 T21 T22 T23 T24 T25) do call chk %%n
for %%n in (T26 T27 T28 T29 T30 T31 T32 T33 T34) do call chk %%n
for %%n in (T35 T36 T37 T40 T50 T51 T52 T53 T54) do call chk %%n
for %%n in (T55 T56 T57 T58 T59) do call chk %%n
for %%n in (T03R T04R T05R T06R T07R T09R T11R) do call chk %%n
for %%n in (T12R T14R T15R T16R) do call chk %%n
call chk T13R ABC
for %%n in (T17R T18R T19R T20R T21R T22R T23R) do call chk %%n
for %%n in (T24R T25R T26R T28R T29R T30R T31R) do call chk %%n
for %%n in (T32R T33R T34R T35R T36R T37R T40R) do call chk %%n
for %%n in (T50R T51R T52R T53R T54R T55R T56R) do call chk %%n
for %%n in (T57R T58R T59R) do call chk %%n
call chk T58L
T02.EXE
if not errorlevel 42 goto t02f
if not errorlevel 43 echo T02 exit code: ok>>RUN.LOG
:t02f
if not errorlevel 42 echo *** T02 FAILED >> RUN.LOG
if errorlevel 43 echo *** T02 FAILED >> RUN.LOG
for %%n in (N01 N02 N03) do call chkn %%n
T59X.COM >> RUN.LOG
if errorlevel 1 echo *** T59X FAILED >> RUN.LOG
type RUN.LOG
