; Vendor API "ZRDX" (task 149, VENDOR build): INT 2Fh 168Ah
; lookup, far entry, GetVersion/GetLastError, error path and
; register preservation.
%include "../rdf/T.INC"
bits 32
global start
TNAME 'VENDOR entry'
section .data
good:   db 'ZRDX', 0
bad:    db 'ZRDY', 0
entry:  dd 0
        dw 0
section .text
start:  BEGIN
        mov ax, ds                  ;fs/gs must survive 168Ah
        mov fs, ax
        mov gs, ax
        mov esi, bad                ;unknown vendor: not found
        mov ax, 168Ah
        int 2Fh
        cmp al, 0
        je near fail1
        call segchk
        jne near fail1
        mov esi, good               ;known vendor
        mov ax, 168Ah
        int 2Fh
        cmp al, 0
        jne near fail1
        call segchk
        jne near fail1
        mov [entry], edi
        mov [entry + 4], es
        mov ax, ds                  ;restore es for the data
        mov es, ax
        mov ebx, 11111111h          ;sentinels
        mov ecx, 22222222h
        mov edx, 33333333h
        mov esi, 44444444h
        mov edi, 55555555h
        mov ebp, 66666666h
        mov ax, 0                   ;GetVersion
        call far [entry]
        jc near fail1
        cmp eax, 00010000h
        jne near fail1
        cmp ebx, 11111111h
        jne near fail1
        cmp ecx, 22222222h
        jne near fail1
        cmp edx, 33333333h
        jne near fail1
        cmp esi, 44444444h
        jne near fail1
        cmp edi, 55555555h
        jne near fail1
        cmp ebp, 66666666h
        jne near fail1
        mov ax, 5                   ;GetLastError
        call far [entry]
        jc near fail1
        or eax, eax
        jnz near fail1
        mov ax, 6                   ;bad function
        call far [entry]
        jnc near fail1
        cmp eax, 1
        jne near fail1
        PASS
fail1:  FAIL
segchk: mov bx, ds                  ;ZF = ds, fs, gs unchanged
        mov ax, fs
        cmp ax, bx
        jne .x
        mov ax, gs
        cmp ax, bx
.x:     ret
