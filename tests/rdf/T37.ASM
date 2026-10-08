; clipboard API: calls without pointers (open/empty/compact/caps/close) do not crash
%include "t.inc"
bits 32
global start
TNAME 'T37 clipboard misc'
section .data
skip:   db 'skipped (no clipboard driver)', 13, 10, '$'
section .text
start:  BEGIN
        mov eax, 1700h
        int 2Fh
        cmp ax, 1700h
        je near .skip
        mov eax, 1701h
        int 2Fh
        mov eax, 1709h
        xor esi, esi
        mov ecx, 1000
        int 2Fh
        mov eax, 170Ah
        xor edx, edx
        int 2Fh
        mov eax, 1704h
        mov edx, 1
        int 2Fh
        mov eax, 1706h
        int 2Fh
        mov eax, 170Bh
        int 2Fh
        mov eax, 17FFh
        int 2Fh
        mov eax, 1708h
        int 2Fh
        PASS
.skip:  PRINT skip
        EXITC 0
