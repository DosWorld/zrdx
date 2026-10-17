; 0503h failed moved-block growth: whole-block and neighbor rollback.
; Exact error codes/accounting are ZRDX policy; S13.4 forbids size zero.
%ifndef CLIENT32
%define CLIENT32 0
%endif
%ifndef SPARE
%define SPARE 0
%endif
%assign REQUEST ((SPARE + 2) * 4096)
%ifdef BOUND
%define ERRJ near
bits 32
global start
section .text
start:  cld
        push ds
        pop es
        push ds
        pop fs
%else
%define ERRJ
bits 16
org 100h
start:  cld
        mov sp, stacktop
        push cs
        pop ds
        push cs
        pop es
        mov bx, (stacktop - $$ + 100h + 15) / 16
        mov ah, 4Ah
        int 21h
        jc ERRJ bad
        mov ax, 1687h
        int 2Fh
        test ax, ax
        jnz bad
        mov [entry], di
        mov [entry+2], es
        mov bx, si
        mov ah, 48h
        int 21h
        jc ERRJ bad
        mov es, ax
        mov ax, CLIENT32
        call far [entry]
        jc ERRJ bad
        push ds
        pop es
        mov ax, 1Bh
        mov fs, ax
%endif
        mov byte [step], 1
        mov edi, info
        mov eax, 500h
        int 31h
        mov eax, [info]
        mov [largest], eax
.again: xor ebx, ebx
        mov ecx, 1000h
        mov eax, 501h
        int 31h
        jc ERRJ bad
        mov [blocka], di
        mov [blocka+2], si
        shl ebx, 16
        mov bx, cx
        mov [basea], ebx
        mov eax, 12345678h
        call fill
        xor ebx, ebx
        mov ecx, 1000h
        mov eax, 501h
        int 31h
        jc ERRJ bad
        mov [blockb], di
        mov [blockb+2], si
        shl ebx, 16
        mov bx, cx
        mov [baseb], ebx
        mov eax, 87654321h
        call fill
        mov byte [step], 2
        mov edi, info
        mov eax, 500h
        int 31h
        jc ERRJ bad
        mov ecx, [info]
        sub ecx, SPARE * 4096
        jbe ERRJ bad
        mov ebx, ecx
        shr ebx, 16
        mov eax, 501h
        int 31h
        jc ERRJ bad
        mov [handle], di
        mov [handle+2], si
        mov byte [step], 3
        mov di, [blocka]
        mov si, [blocka+2]
        mov ecx, REQUEST
        mov ebx, REQUEST >> 16
        mov edx, 12345678h
        mov ebp, 89ABCDEFh
        mov eax, 503h
        int 31h
        mov [retax], ax
        pushfd
        pop dword [retfl]
        jnc ERRJ bad
        cmp ax, 8012h
        jne ERRJ bad
        cmp ecx, REQUEST
        jne ERRJ bad
        cmp ebx, REQUEST >> 16
        jne ERRJ bad
        cmp edx, 12345678h
        jne ERRJ bad
        cmp ebp, 89ABCDEFh
        jne ERRJ bad
        cmp di, [blocka]
        jne ERRJ bad
        cmp si, [blocka+2]
        jne ERRJ bad
        ; Reject a zero resize too, while the original handle remains live.
        mov di, [blocka]
        mov si, [blocka+2]
        xor ebx, ebx
        xor ecx, ecx
        mov eax, 503h
        int 31h
        jnc ERRJ bad
        mov byte [step], 4
        mov ebx, [basea]
        mov eax, 12345678h
        call verify
        mov ebx, [baseb]
        mov eax, 87654321h
        call verify
        mov di, [handle]
        mov si, [handle+2]
        mov eax, 502h
        int 31h
        jc ERRJ bad
        mov di, [blocka]
        mov si, [blocka+2]
        mov eax, 502h
        int 31h
        jc ERRJ bad
        mov di, [blockb]
        mov si, [blockb+2]
        mov eax, 502h
        int 31h
        jc ERRJ bad
        mov byte [step], 5
        mov edi, info
        mov eax, 500h
        int 31h
        jc ERRJ bad
        mov eax, [info]
        cmp eax, [largest]
        jne ERRJ bad
        dec byte [rounds]
        jnz .again
        mov esi, ok
        xor bx, bx
        jmp print
; FS is the flat client alias in both native and bound builds.
fill:   mov ecx, 1024
.next:  mov [fs:ebx], eax
        add ebx, 4
        loop .next
        ret
verify: mov ecx, 1024
.next:  cmp [fs:ebx], eax
        jne ERRJ bad
        add ebx, 4
        loop .next
        ret
bad:    pushad
        mov ax, [retax]
        call hexword
        mov ax, [retfl]
        call hexword
        popad
        mov dl, [step]
        add dl, '0'
        mov ah, 2
        int 21h
        mov esi, fail
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
hexword:
        push cx
        mov cx, 4
.hex:   rol ax, 4
        push ax
        mov dl, al
        and dl, 15
        add dl, '0'
        cmp dl, '9'
        jbe .write
        add dl, 7
.write: pushad
        mov ah, 2
        int 21h
        popad
        pop ax
        loop .hex
        pop cx
        ret
%ifdef BOUND
section .data
%endif
retax: dw 0
retfl: dd 0
entry: dd 0
step: db 0
rounds: db 16
blocka: dd 0
blockb: dd 0
basea: dd 0
baseb: dd 0
largest: dd 0
handle: dd 0
info: times 48 db 0
%ifdef BOUND
ok: db 'OOM bound rollback: ok',13,10,0
%elif CLIENT32
ok: db 'OOM32 moved rollback: ok',13,10,0
%else
ok: db 'OOM16 moved rollback: ok',13,10,0
%endif
fail: db ' moved rollback: FAIL',13,10,0
%ifndef BOUND
        times 1024 db 0
stacktop:
%endif
