; This file is part of the ZRDX OSE project
; (C) 2026, Viacheslav Komenda
;
; Vendor API "ZRDX" (task 149): INT 2Fh 168Ah lookup and the
; far entry VendorTrapH. Functions: see docs/PEABI.MD.

%ifdef VENDORON
VendorVersion equ 00010000h
VendorMaxFn equ 5

        SEGM EText
; INT 2Fh AX=168Ah, DS:ESI -> "ZRDX",0 : ES:EDI = vendor entry
Vendor168A:
        movzx edx, word [ebp + DC_DS0]
        lsl eax, edx
        jnz near ToOldInt0
        mov ebx, [ebp + DC_ESI]         ;esi must stay: ToOldInt0
        lea ecx, [ebx + 4]
        jc near ToOldInt0
        cmp ecx, eax
        ja near ToOldInt0
        push ds                         ;flat ds: ToOldInt0
        mov ds, edx
        cmp dword [ebx], 'ZRDX'
        jne .other
        cmp byte [ebx + 4], 0
.other: pop ds                          ;pop keeps the flags
        jne near ToOldInt0
%ifdef VENDORPE
        mov dword [ebp + DC_EDI], OffPEVendor
        mov byte [ebp + DC_EAX], 0
        mov ax, cs
%else
        mov dword [ebp + DC_EDI], OffVendorTrap3
        mov byte [ebp + DC_EAX], 0
        mov ax, Trap3Selector
%endif
        mov es, eax
        jmp rrrr1
        ESEG EText

%ifndef VENDORPE
        SEGM Text
; Far entry, AX = function. CF set and EAX = error on failure.
        DPROC VendorTrapH
        cmp ax, VendorMaxFn
        ja .bad
        jne .notlast
        xor eax, eax                    ;5: GetLastError
        clc
        retf
.notlast:
        or ax, ax
        jnz .bad
        mov eax, VendorVersion          ;0: GetVersion
        clc
        retf
.bad:   mov eax, 1                      ;bad function
        stc
        retf
        ESEG Text
%endif
%endif
