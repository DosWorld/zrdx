bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
; Module registry fixture (task 008), runs on stub-pmx only.
; Looks up the vendor entry "ZRDX" (INT 2Fh 168Ah) and checks
; GetVersion, GetModuleHandle (case, path) and GetLastError.
        mov ax, 168Ah
        mov esi, vname
        int 2Fh
        or al, al
        jnz bad1
        mov [vp], edi
        mov [vp + 4], es
        xor eax, eax
        call far [vp]
        jc bad2
        cmp eax, 10000h
        jne bad2
        mov edx, nm1
        call lookup
        jc bad3
        mov ebx, eax
        mov edx, nm2
        call lookup
        jc bad3
        cmp eax, ebx
        jne bad3
        mov edx, nm3
        call lookup
        jc bad3
        cmp eax, ebx
        jne bad3
        mov edx, nm4
        call lookup
        jnc bad4
        cmp eax, 5
        jne bad4
        mov eax, 5
        call far [vp]
        jc bad4
        cmp eax, 5
        jne bad4
        mov eax, 10                     ;unused function
        call far [vp]
        jnc bad5
        cmp eax, 1
        jne bad5
        mov esi, vname                  ;DPMI 0A00h (task 152)
        mov ax, 0A00h
        int 31h
        jc nohost
        cmp edi, [vp]
        jne bad6
        mov ax, es
        cmp ax, [vp + 4]
        jne bad6
        push ds
        pop es
        mov esi, bname                  ;unknown vendor
        mov ax, 0A00h
        int 31h
        jnc bad6
        cmp ax, 8001h
        jne bad6
        mov edx, okmsg
        jmp fin
nohost: cmp ax, 8001h                   ;host without 0A00h
        jne bad6
        mov edx, okmsg2
        jmp fin
lookup: mov eax, 2
        call far [vp]
        ret
bad1:   mov edx, m1
        jmp fin
bad2:   mov edx, m2
        jmp fin
bad3:   mov edx, m3
        jmp fin
bad4:   mov edx, m4
        jmp fin
bad5:   mov edx, m5
        jmp fin
bad6:   mov edx, m6
fin:    mov ah, 9
        int 21h
        mov eax, 4c00h
        cmp edx, okmsg
        je quit
        cmp edx, okmsg2
        je quit
        mov eax, 4c01h
quit:   int 21h
vp: dd 0, 0
vname: db 'ZRDX', 0
bname: db 'ZRDY', 0
nm1: db 'MODS.EXE', 0
nm2: db 'mods.exe', 0
nm3: db 'C:\X\Mods.Exe', 0
nm4: db 'NOSUCH.DLL', 0
okmsg: db 'PE modules: ok', 13, 10, '$'
okmsg2: db 'PE modules: ok, host lacks 0A00h', 13, 10, '$'
m1: db 'PE modules: no vendor entry', 13, 10, '$'
m2: db 'PE modules: bad version', 13, 10, '$'
m3: db 'PE modules: bad handle', 13, 10, '$'
m4: db 'PE modules: bad error', 13, 10, '$'
m5: db 'PE modules: bad function', 13, 10, '$'
m6: db 'PE modules: bad vendor 0A00h', 13, 10, '$'
