; Disk error injection (task 138, SWAPFAIL build, /S72h): the
; 3rd page read fails; that is fatal for the client (exit
; status 1) and the swap file is still deleted.
%define TITLE 'SWAPR read failure'
%include "SWAPT.ASM"
