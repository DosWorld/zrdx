bits 16
org 100h
start:
        mov sp, stacktop
        mov bx, (stacktop - $$ + 100h + 15) / 16
        mov ah, 4Ah
        int 21h
        jc near bad
        mov ax, 3567h
        int 21h
        mov ax, es
        or ax, bx
        push cs
        pop es
        jz .xms
        mov ax, 0DE00h
        int 67h
        test ah, ah
        jnz .xms
        mov byte [hasvcpi], 1
.xms:   mov ax, 4300h
        int 2Fh
        cmp al, 80h
        jne .ready
        mov ax, 4310h
        int 2Fh
        mov [xms], bx
        mov [xms + 2], es
        push cs
        pop es
        xor ah, ah
        call far [xms]
        mov byte [xmsfn], 8
        cmp ax, 0300h
        jb .ready
        mov byte [xmsfn], 88h
.ready: mov di, before
        call snapshot
        mov [block + 4], ds
        mov [block + 8], ds
        mov [block + 12], ds
        mov word [runs], 4
.next:  mov dx, child
        mov bx, block
        mov ax, 4B00h
        int 21h
        mov ax, cs
        mov ds, ax
        mov es, ax
        jc near bad
        mov ah, 4Dh
        int 21h
        test ax, ax
        jnz near bad
        mov di, after
        call snapshot
        mov si, before
        mov di, after
        mov cx, 7
        cld
        repe cmpsw
        jne near bad
        dec word [runs]
        jnz .next
        mov dx, ok
        mov ah, 9
        int 21h
        mov ax, 4C00h
        int 21h
snapshot:
        cmp byte [hasvcpi], 0
        je .xms
        mov ax, 0DE03h
        int 67h
        test ah, ah
        jnz near bad
        mov [di], edx
.xms:   cmp byte [xmsfn], 0
        je .dos
        xor eax, eax
        xor edx, edx
        xor bx, bx
        mov ah, [xmsfn]
        call far [xms]
        test bl, bl
        jnz near bad
        cmp byte [xmsfn], 8
        jne .store
        movzx eax, ax
        movzx edx, dx
.store: mov [di + 4], eax
        mov [di + 8], edx
.dos:   mov bx, 0FFFFh
        mov ah, 48h
        int 21h
        jnc bad
        cmp ax, 8
        jne bad
        mov [di + 12], bx
        ret
bad:    push cs
        pop ds
        mov dx, failed
        mov ah, 9
        int 21h
        mov ax, 4C01h
        int 21h
%ifndef TestChild
%define TestChild 'T59.EXE'
%endif
%ifndef TestTitle
%define TestTitle 'T59X LDT exit cleanup'
%endif
child:  db TestChild, 0
block:  dw 0, tail, 0, 5Ch, 0, 6Ch, 0
tail:   db 0, 13
xms:    dd 0
xmsfn:  db 0
hasvcpi: db 0
runs:   dw 0
before: times 14 db 0
after:  times 14 db 0
ok:     db TestTitle, ': ok', 13, 10, '$'
failed: db TestTitle, ': FAIL', 13, 10, '$'
        times 512 db 0
stacktop:
