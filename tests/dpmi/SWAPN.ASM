; SWAPT as a parent: it fills a third of its pool, runs
; SWAPCH.EXE (which pages in a host of its own) and checks
; its data and the pool afterwards. Needs zrdxconf /X800 so
; that memory is left for the child.
%define NEST
%define TITLE 'SWAPN nested paging'
%include "SWAPT.ASM"
