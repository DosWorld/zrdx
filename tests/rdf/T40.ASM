; long file name API (INT 21h, AX=71xxh): volume info, mkdir, create, find, rename, attrs, truename, delete, rmdir
%include "t.inc"
bits 32
global start
TNAME 'T40 lfn'
section .data
dirname: db 'long directory name 1', 0
fname:  db 'long directory name 1\a long file name.txt', 0
fname2: db 'long directory name 1\renamed long name.txt', 0
wild:   db 'long directory name 1\*', 0
shortsrc: db 'longname for test.txt', 0
dot:    db '.', 0
root:   db 'C:\', 0
text:   db 'long file contents'
TLEN    equ $ - text
expect: db 'a long file name.txt', 0
ELEN    equ $ - expect
skip:   db 'skipped (no LFN support)', 13, 10, '$'
section .bss
fsname: resb 32
info:   resb 64
fd:     resb 340
tn:     resb 300
cd:     resb 300
sn:     resb 32
qw:     resb 8
found:  resd 1
step:   resb 1
errax:  resw 1
section .text
start:  BEGIN
        push ds
        pop es
        mov ah, 19h
        int 21h
        add al, 'A'
        mov [root], al
        mov byte [step], 1
        stc
        mov ax, 71A0h
        mov edx, root
        mov edi, fsname
        mov ecx, 32
        int 21h
        jc near .skip
        test bh, 40h
        jz near .skip
        cmp cx, 255
        jne near bad
        mov byte [step], 2
        stc
        mov ax, 7141h
        mov edx, fname
        xor esi, esi
        xor ecx, ecx
        int 21h
        mov byte [step], 3
        stc
        mov ax, 7141h
        mov edx, fname2
        xor esi, esi
        xor ecx, ecx
        int 21h
        mov byte [step], 4
        stc
        mov ax, 713Ah
        mov edx, dirname
        int 21h
        mov byte [step], 5
        stc
        mov ax, 7139h
        mov edx, dirname
        int 21h
        jc near bad
        mov byte [step], 6
        stc
        mov ax, 716Ch
        mov ebx, 2
        xor ecx, ecx
        mov edx, 12h
        mov esi, fname
        xor edi, edi
        int 21h
        jc near bad
        cmp cx, 2
        jne near bad
        mov ebp, eax
        mov ebx, ebp
        mov ah, 40h
        mov edx, text
        mov ecx, TLEN
        int 21h
        jc near bad
        cmp eax, TLEN
        jne near bad
        mov ebx, ebp
        mov ah, 68h
        int 21h
        mov byte [step], 7
        stc
        mov ax, 71A6h
        mov ebx, ebp
        mov edx, info
        int 21h
        jc near bad
        cmp dword [info + 24h], TLEN
        jne near bad
        mov ebx, ebp
        mov ah, 3Eh
        int 21h
        mov dword [found], 0
        mov byte [step], 8
        stc
        mov ax, 714Eh
        mov ecx, 16h
        mov esi, 1
        mov edx, wild
        mov edi, fd
        int 21h
        jc near bad
        mov ebp, eax
.next:  mov esi, fd + 2Ch
        mov edi, expect
        mov ecx, ELEN
        repe cmpsb
        jne .nomatch
        cmp byte [fd + 130h], 0
        je near bad
        cmp dword [fd + 20h], TLEN
        jne near bad
        inc dword [found]
.nomatch:
        mov byte [step], 9
        stc
        mov ax, 714Fh
        mov ebx, ebp
        mov esi, 1
        mov edi, fd
        int 21h
        jnc .next
        mov byte [step], 10
        stc
        mov ax, 71A1h
        mov ebx, ebp
        int 21h
        jc near bad
        cmp dword [found], 1
        jne near bad
        mov byte [step], 11
        stc
        mov ax, 7156h
        mov edx, fname
        mov edi, fname2
        int 21h
        jc near bad
        mov byte [step], 12
        stc
        mov ax, 7143h
        xor ebx, ebx
        mov edx, fname2
        int 21h
        jc near bad
        test cl, 10h
        jnz near bad
        mov byte [step], 13
        stc
        mov ax, 7160h
        mov ecx, 2
        mov esi, dot
        mov edi, tn
        int 21h
        jc near bad
        cmp byte [tn + 1], ':'
        jne near bad
        mov byte [step], 14
        stc
        mov ax, 7160h
        mov ecx, 1
        mov esi, fname2
        mov edi, tn
        int 21h
        jc near bad
        cmp byte [tn + 1], ':'
        jne near bad
        mov byte [step], 15
        stc
        mov ax, 7147h
        xor edx, edx
        mov esi, cd
        int 21h
        jc near bad
        mov byte [step], 16
        stc
        mov ax, 71A8h
        mov esi, shortsrc
        mov edi, sn
        mov edx, 100h
        int 21h
        jc .noshort
        cmp byte [sn], 0
        je near bad
.noshort:
        mov dword [qw], 0
        mov dword [qw + 4], 0
        mov byte [step], 17
        stc
        mov ax, 71A7h
        mov ebx, 1
        mov ecx, 6000h
        mov edx, 5A21h
        mov edi, qw
        int 21h
        jc .notime
        mov byte [step], 18
        stc
        mov ax, 71A7h
        xor ebx, ebx
        mov esi, qw
        int 21h
        jc near bad
        cmp cx, 6000h
        jne near bad
        cmp dx, 5A21h
        jne near bad
.notime:
        mov byte [step], 19
        stc
        mov ax, 7141h
        mov edx, fname2
        xor esi, esi
        xor ecx, ecx
        int 21h
        jc near bad
        mov byte [step], 20
        stc
        mov ax, 713Ah
        mov edx, dirname
        int 21h
        jc near bad
        PASS
.skip:  PRINT skip
        EXITC 0
bad:    mov [errax], ax
        mov al, [step]
        aam
        mov bx, ax
        mov dl, bh
        add dl, '0'
        mov ah, 2
        int 21h
        mov dl, bl
        add dl, '0'
        mov ah, 2
        int 21h
        mov dl, ' '
        mov ah, 2
        int 21h
        mov bx, [errax]
        mov cx, 4
.hx:    rol bx, 4
        mov dl, bl
        and dl, 0Fh
        add dl, '0'
        cmp dl, '9'
        jbe .hd
        add dl, 7
.hd:    mov ah, 2
        int 21h
        loop .hx
        mov dl, ' '
        mov ah, 2
        int 21h
        FAIL
