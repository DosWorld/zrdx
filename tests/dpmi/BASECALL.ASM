; Native 16/32-bit DPMI contract fixture. No intermediate execution.
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 0
%endif
; Masks record ZRDX register guarantees, including stronger host policy.
; S1 permits AX/flags to change and memory services to enable IF. Exact AX,
; DF/high-word preservation and 800xh errors are host regression contracts;
; non-output general/segment registers and pointer widths follow S1.
%macro KEEP31 6-7 0
        pushfd
        pushad
%ifdef POSITIVE
        mov [ss:regfn], ax
        mov [ss:regbx], bx
%endif
        int 31h
%ifdef POSITIVE
        mov [ss:regax], ax
%endif
        pushfd
        pushad
%if !CLIENT32
        mov bp, sp               ;word addressing remains valid across IRQ returns
%define SNAP bp
%else
%define SNAP esp
%endif
%ifdef POSITIVE
        mov byte [ss:regwhy], 1
%endif
        mov eax, [ss:SNAP+28]
        xor eax, [ss:SNAP+64]
        and eax, (0FFFFFFFFh ^ (%1))
        jnz bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 2
%endif
        mov eax, [ss:SNAP+16]
        xor eax, [ss:SNAP+52]
        and eax, (0FFFFFFFFh ^ (%2))
        jnz bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 3
%endif
        mov eax, [ss:SNAP+24]
        xor eax, [ss:SNAP+60]
        and eax, (0FFFFFFFFh ^ (%3))
        jnz bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 4
%endif
        mov eax, [ss:SNAP+20]
        xor eax, [ss:SNAP+56]
        and eax, (0FFFFFFFFh ^ (%4))
        jnz bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 5
%endif
        mov eax, [ss:SNAP+4]
        xor eax, [ss:SNAP+40]
        and eax, (0FFFFFFFFh ^ (%5))
        jnz bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 6
%endif
        mov eax, [ss:SNAP+0]
        xor eax, [ss:SNAP+36]
        and eax, (0FFFFFFFFh ^ (%6))
        jnz bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 7
%endif
        mov eax, [ss:SNAP+8]
        xor eax, [ss:SNAP+44]
        jnz bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 8
%endif
        mov eax, [ss:SNAP+32]
        xor eax, [ss:SNAP+68]
        and eax, 600h & ~(%7)     ;IF/DF, excluding explicit IF outputs
        jnz bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 9
%endif
        mov ax, ds
%ifdef POSITIVE
        cmp ax, [cs:keepds]
%else
        cmp ax, [cs:pmds]
%endif
        jne bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 10
%endif
        mov ax, es
        cmp ax, [cs:pmds]
        jne bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 11
%endif
        mov ax, fs
        cmp ax, [cs:pmds]
        jne bad
%ifdef POSITIVE
        mov byte [ss:regwhy], 12
%endif
        mov ax, gs
        cmp ax, [cs:pmds]
        jne bad
        popad
        popfd
        lea esp, [esp+36]
%ifdef POSITIVE
        mov byte [ss:regwhy], 0
%endif
%endmacro
start:  cld
        mov sp, stacktop
        push cs
        pop ds
        push cs
        pop es
        mov bx, (stacktop - $$ + 100h + 15) / 16
        mov ah, 4Ah
        int 21h
        jc bad
        mov ax, 1687h
        int 2Fh
        test ax, ax
        jnz bad
        mov [entry], di
        mov [entry+2], es
        mov bx, si
        mov ah, 48h
        int 21h
        jc bad
        mov es, ax
        mov ax, CLIENT32
        call far [entry]
        jc bad
        push ds
        pop es
        movzx esp, sp
        mov [pmds], ds
        mov ax, ds
        mov fs, ax
        mov gs, ax
        mov ebx, 12345678h
        mov ecx, 23456789h
        mov edx, 3456789Ah
        mov esi, 456789ABh
        mov edi, 56789ABCh
        mov ebp, 6789ABCDh
%ifdef NEGATIVE
        mov byte [step], 1
        xor ecx, ecx
        xor eax, eax
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        xor ebx, ebx
%assign fn 1
%rep 12
%if fn == 1 || (fn >= 6 && fn <= 0Ch)
        mov eax, fn
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
%endif
%assign fn fn+1
%endrep
        mov byte [step], 2
        mov ebx, 80h
        mov eax, 0Dh
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov ebx, 20h
        mov eax, 202h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov eax, 203h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov ebx, 3
        mov cx, ds
        mov eax, 203h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov eax, 205h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov byte [step], 3
        mov ebx, 0FFFFh
        mov eax, 100h
        KEEP31 0FFFFh, 0FFFFh, 0, 0, 0, 0
        jnc bad
        mov dx, ds
        mov eax, 101h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov eax, 102h
        KEEP31 0FFFFh, 0FFFFh, 0, 0, 0, 0
        jnc bad
        mov byte [step], 4
        xor ebx, ebx
        xor ecx, ecx
        mov eax, 501h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        xor esi, esi
        xor edi, edi
        mov eax, 502h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov ecx, 1000h
        mov eax, 503h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov byte [step], 5
        xor ebx, ebx
        xor ecx, ecx
        mov esi, 0
        mov edi, 1000h
        mov eax, 800h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov byte [step], 6
        mov ebx, 4
%assign fn 0B01h
%rep 3
        mov eax, fn
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
%assign fn fn+1
%endrep
        xor ebx, ebx
        xor ecx, ecx
        mov edx, 0103h
        mov eax, 0B00h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cld
%elifdef POSITIVE
        mov [keepds], ds
        std
        mov byte [step], 1
        mov ecx, 1
        xor eax, eax
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jc bad
        mov bx, ax
        mov [testsel], ax
        mov eax, 6
        KEEP31 0, 0, 0FFFFh, 0FFFFh, 0, 0
        jc bad
        xor ecx, ecx
        xor edx, edx
        mov eax, 7
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov edx, 0FFFFh
        mov eax, 8
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov ecx, 0F2h
        mov eax, 9
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov edi, testdesc
        mov eax, 0Bh
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0Ch
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0Ah
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jc bad
        mov bx, ax
        mov eax, 1
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov bx, [testsel]
        mov eax, 1
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov ebx, 0B800h
        mov eax, 2
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jc bad
        mov ebx, 4
        mov eax, 0Dh
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 1
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov byte [step], 2
        mov ebx, 10h
        mov eax, 100h
        KEEP31 0FFFFh, 0, 0, 0FFFFh, 0, 0
        jc bad
        mov ebx, 20h
        mov eax, 102h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 101h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov byte [step], 3
        mov ebx, 60h
        mov eax, 200h
        KEEP31 0, 0, 0FFFFh, 0FFFFh, 0, 0
        jc bad
        mov eax, 201h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov ebx, 3
        mov eax, 202h
%if CLIENT32
%define OFFSETMASK 0FFFFFFFFh
%else
%define OFFSETMASK 0FFFFh
%endif
        KEEP31 0, 0, 0FFFFh, OFFSETMASK, 0, 0
        jc bad
        mov eax, 203h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov ebx, 60h
        mov eax, 204h
        KEEP31 0, 0, 0FFFFh, OFFSETMASK, 0, 0
        jc bad
        mov eax, 205h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov byte [step], 4
        push ds
        push cs
        pop ds
        mov [es:keepds], ds
        mov esi, testcb
        mov edi, testregs
        mov eax, 303h
        KEEP31 0, 0, 0FFFFh, 0FFFFh, 0, 0
        pop ds
        mov [keepds], ds
        jc bad
        mov eax, 304h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 305h
        KEEP31 0FFFFh, 0FFFFh, 0FFFFh, 0, 0FFFFh, OFFSETMASK
        jc bad
        mov eax, 306h
        KEEP31 0, 0FFFFh, 0FFFFh, 0, 0FFFFh, OFFSETMASK
        jc bad
        mov byte [step], 5
        mov edi, testinfo
        mov eax, 500h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        xor ebx, ebx
        mov ecx, 1000h
        mov eax, 501h
        KEEP31 0, 0FFFFh, 0FFFFh, 0, 0FFFFh, 0FFFFh
        jc bad
        xor ebx, ebx
        mov ecx, 2000h
        mov eax, 503h
        KEEP31 0, 0FFFFh, 0FFFFh, 0, 0FFFFh, 0FFFFh
        jc bad
        mov eax, 502h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov byte [step], 6
        mov ebx, 0A0h
        xor ecx, ecx
        xor esi, esi
        mov edi, 1000h
        mov eax, 800h
        KEEP31 0, 0FFFFh, 0FFFFh, 0, 0, 0
        jc bad
        cld
%else
        mov byte [step], 1
        mov eax, 0BAAD0003h
        std
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jc bad
        cmp ax, 8
        jne bad
        mov eax, 0BAAD0400h
        KEEP31 0FFFFh, 0FFFFh, 0FFh, 0FFFFh, 0, 0
        jc bad
        cmp ax, 5Ah
        jne bad
        test bx, 0FFFCh
        jnz bad
        test bx, 1
        jz bad
        cmp cl, 3
        jb bad
        cmp dx, 0870h
        jne bad
        mov byte [step], 2
        mov eax, 0BAAD0600h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0BAAD0601h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0BAAD0602h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0BAAD0603h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0BAAD0702h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0BAAD0703h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0BAAD0604h
        KEEP31 0, 0FFFFh, 0FFFFh, 0, 0, 0
        jc bad
        test bx, bx
        jnz bad
        cmp cx, 1000h
        jne bad
        mov byte [step], 3
        mov eax, 0BAAD0900h
        KEEP31 0FFh, 0, 0, 0, 0, 0, 200h
        jc bad
        mov eax, 0BAAD0902h
        KEEP31 0FFh, 0, 0, 0, 0, 0
        jc bad
        test al, al
        jnz bad
        pushfd
        pop eax
        test ah, 2
        jnz bad
        mov eax, 0BAAD0901h
        KEEP31 0FFh, 0, 0, 0, 0, 0, 200h
        jc bad
        test al, al
        jnz bad
        mov eax, 0BAAD0902h
        KEEP31 0FFh, 0, 0, 0, 0, 0
        jc bad
        cmp al, 1
        jne bad
        pushfd
        pop eax
        test ah, 2
        jz bad
        mov byte [step], 4
        mov esi, vendor
%if !CLIENT32
        or esi, 0DEAD0000h
%endif
        mov eax, 0BAAD0A00h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        mov eax, 0BAAD0004h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        mov eax, 0BAAD0005h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        mov eax, 0BAAD000Eh
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        mov eax, 0BAAD000Fh
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        mov eax, 0BAAD0206h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        mov eax, 0BAAD0401h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        mov eax, 0BAAD0801h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        mov eax, 0BAAD0B04h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        mov eax, 0BAAD0C00h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        mov eax, 0BAADFFFFh
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
        cmp ax, 8001h
        jne bad
        cld
        mov byte [step], 5
        mov bx, [pmds]
        mov eax, 6
        int 31h
        jc bad
        shl ecx, 16
        mov cx, dx
        add ecx, watched
        mov ebx, ecx
        shr ebx, 16
        mov edx, 0101h
        mov eax, 0B00h
        KEEP31 0, 0FFFFh, 0, 0, 0, 0
        jc bad
        mov [debug], bx
        mov eax, 0B02h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jc bad
        test ax, ax
        jnz bad
        mov eax, 0B03h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0B01h
        KEEP31 0, 0, 0, 0, 0, 0
        jc bad
        mov eax, 0B01h
        KEEP31 0FFFFh, 0, 0, 0, 0, 0
        jnc bad
%endif
        mov si, ok
        xor bx, bx
        jmp print
bad:    cld
%ifdef POSITIVE
        mov dl, [regwhy]
        add dl, 'A'
        mov ah, 2
        int 21h
%macro SHOWWORD 1
        mov bx, [cs:%1]
        mov cx, 4
%%hex:  rol bx, 4
        mov dl, bl
        and dl, 15
        add dl, '0'
        cmp dl, '9'
        jbe %%put
        add dl, 7
%%put:  mov ah, 2
        int 21h
        loop %%hex
        mov dl, '/'
        int 21h
%endmacro
        SHOWWORD regfn
        SHOWWORD regax
        SHOWWORD regbx
%endif
        mov dl, [step]
        add dl, '0'
        mov ah, 2
        int 21h
        mov si, fail
        mov bx, 1
print:  lodsb
        test al, al
        jz done
        mov dl, al
        pushad
        mov ah, 2
        int 21h
        popad
        jmp print
done:   mov ax, 4C00h
        mov al, bl
        int 21h
entry: dd 0
step: db 0
pmds: dw 0
debug: dw 0
watched: db 0
vendor: db 'unknown vendor',0
%ifdef POSITIVE
keepds: dw 0
regfn: dw 0
regax: dw 0
regbx: dw 0
regwhy: db 0
testsel: dw 0
testdesc: times 8 db 0
testregs: times 50 db 0
testinfo: times 64 db 0
testcb:
%if CLIENT32
        iretd
%else
        iret
%endif
%endif
%ifdef POSITIVE
%if CLIENT32
ok: db 'REG32 service register contracts: ok',13,10,0
%else
ok: db 'REG16 service register contracts: ok',13,10,0
%endif
%elifdef NEGATIVE
%if CLIENT32
ok: db 'ERR32 failure preservation: ok',13,10,0
%else
ok: db 'ERR16 failure preservation: ok',13,10,0
%endif
%else
%if CLIENT32
ok: db 'BAS32 base and register contracts: ok',13,10,0
%else
ok: db 'BAS16 base and register contracts: ok',13,10,0
%endif
%endif
fail: db ' base and register contracts: FAIL',13,10,0
        times 2048 db 0
stacktop:
