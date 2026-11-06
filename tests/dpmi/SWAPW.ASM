; Disk error injection (task 138, needs a SWAPFAIL build and
; zrdxconf /S71h): the 3rd page write fails, the allocation
; that needed it is refused, the next smaller one works and
; its pages survive the paging.
%define FAILW
%define TITLE 'SWAPW write failure'
%include "SWAPT.ASM"
