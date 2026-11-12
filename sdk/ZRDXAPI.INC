;             This file is part of the ZRDX OSE project
;                     (C) 2026, Viacheslav Komenda
;
; ZRDXAPI.INC: cdecl wrappers of the ZRDX vendor API for a flat
; 32-bit NASM program (see docs/SDK.MD). Put one 5-byte JMP at
; the start of the code section, then include this file:
;
;       jmp start
;       %include "ZRDXAPI.INC"
;  start: call ZrdxInit
;
; Offsets from the section start: LoadLibrary 20h,
; GetModuleHandle
; 40h, GetProcAddress 60h, FreeLibrary 80h, GetLastError A0h,
; ZrdxInit C0h. A program exports the first five so that its DLLs
; can import them. LoadLibrary, GetModuleHandle and
; GetProcAddress
; return 0 on failure (GetLastError tells why); FreeLibrary
; returns 0 or an error code.

        align 32
LoadLibrary:
        mov edx, [esp + 4]
        mov eax, 1
        call far [zrdx_vp]
        jnc .ok
        xor eax, eax
.ok:    ret

        align 32
GetModuleHandle:
        mov edx, [esp + 4]
        mov eax, 2
        call far [zrdx_vp]
        jnc .ok
        xor eax, eax
.ok:    ret

        align 32
GetProcAddress:
        push ebx
        mov ebx, [esp + 8]
        mov edx, [esp + 12]
        mov eax, 3
        call far [zrdx_vp]
        jnc .ok
        xor eax, eax
.ok:    pop ebx
        ret

        align 32
FreeLibrary:
        push ebx
        mov ebx, [esp + 8]
        mov eax, 4
        call far [zrdx_vp]
        pop ebx
        ret

        align 32
GetLastError:
        mov eax, 5
        call far [zrdx_vp]
        ret

        align 32
ZrdxInit:
        push esi
        push edi
        ; the call returns the entry
        push es
        mov ax, 168Ah                   ;selector in ES
        mov esi, zrdx_vname
        int 2Fh
        mov [zrdx_vp], edi
        mov [zrdx_vp + 4], es
        pop es
        pop edi
        pop esi
        movzx eax, al
        ret

zrdx_vp: dd 0, 0
zrdx_vname: db 'ZRDX', 0
