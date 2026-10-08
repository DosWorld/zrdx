;BIOS_MOVE STRUC

BM_SLIMIT equ 16
BM_SOURCE equ 18
BM_SACR equ 21

BM_DLIMIT equ 24
BM_DEST equ 26
BM_DACR equ 29

BIOS_MOVE_size equ 48

;TSS_DEF STRUC
TSS_Link equ 0

TSS_ESP0 equ 4
TSS_SS0 equ 8

TSS_ESP1 equ 12
TSS_SS1 equ 16

TSS_ESP2 equ 20
TSS_SS2 equ 24

TSS_CR3 equ 28
TSS_EIP equ 32
TSS_EFLAGS equ 36
TSS_EAX equ 40
TSS_ECX equ 44
TSS_EDX equ 48
TSS_EBX equ 52
TSS_ESP equ 56
TSS_EBP equ 60
TSS_ESI equ 64
TSS_EDI equ 68
TSS_ES equ 72

TSS_CS equ 76

TSS_SS equ 80

TSS_DS equ 84

TSS_FS equ 88

TSS_GS equ 92

TSS_LDT equ 96

TSS_TRACE equ 100
TSS_IOBASE equ 102
%assign TSS_DEF_size 104

SS_FREE_TSS2 equ 1
%assign SS_LDT 2
SS_BUSY_TSS2 equ 3
SS_GATE_PROC2 equ 4
SS_GATE_TASK equ 5
SS_GATE_INT2 equ 6
SS_GATE_TRAP2 equ 7
%assign SS_FREE_TSS3 9
SS_BUSY_TSS3 equ 11
%assign SS_GATE_PROC3 12
SS_GATE_INT3 equ 14
SS_GATE_TRAP3 equ 15

DS_DATA_WR equ 12h ;Data, expands up, writable
DS_DATA equ 10h ;Data, expands up, read-only
DS_DATA_DN_WR equ 16h ;Data, expands down (stack), writable
DS_DATA_DN equ 14h ;Data, expands down, read-only
DS_CODE equ 18h ;Code, not readable
DS_CODE_RD equ 1Ah ;Code, readable
DS_CODE_C equ 1Ch ;Code, conforming, not readable
DS_CODE_C_RD equ 1Eh ;Code, conforming, readable
DS_0 equ 0
DS_1 equ 20h
DS_2 equ 40h
DS_3 equ 60h
DS_PAGE equ 200h
DS_32 equ 100h
