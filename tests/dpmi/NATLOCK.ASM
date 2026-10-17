; Native 32-bit exception/IRQ delivery must use the host's 4 KB stack.
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 1
%endif
start:  cld
        push cs
        pop ds
        push cs
        pop es
        mov [rmseg], cs
        mov sp, stacktop
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
        mov [original], ss
        mov [saveddata], ds
        mov ebx, 200h           ;two client-owned real stacks, not host workspace
        mov eax, 100h
        int 31h
        jc bad
        mov [realstack], ax
        mov [realsel], dx
        add ax, 100h
        mov [realalt], ax
        push ds
        pop es
        mov eax, 305h
        mov edi, 0DEAD0000h
        int 31h
        jc bad
        cmp ax, 16
        jne bad
%if CLIENT32
        mov [pmstate], edi
        mov [pmstate+4], si
%else
        mov [pmstate], di
        mov [pmstate+2], si
%endif
        mov edi, statebuf
%if !CLIENT32
        or edi, 0CAFE0000h
%endif
        xor al, al
%if CLIENT32
        call dword far [pmstate]
%else
        call far [pmstate]
        shr edi, 16
        cmp di, 0CAFEh
        jne bad
%endif
        cmp dword [statebuf+16], 0A5A5A5A5h
        jne bad
        cmp dword [statebuf], 0A5A5A5A5h
        je bad
        mov edi, statebuf
        mov al, 1
%if CLIENT32
        call dword far [pmstate]
%else
        call far [pmstate]
%endif
        mov ax, 202h
        mov bx, 3
        int 31h
        jc bad
        mov [oldexc], edx
        mov [oldexc+4], cx
        mov cx, cs
        mov edx, exception
        mov ax, 203h
        int 31h
        jc bad
        int3
        cmp byte [excseen], 1
        jne bad
        mov bx, 8
        mov ax, 204h
        int 31h
        mov [oldirq], edx
        mov [oldirq+4], cx
        mov [oldirq16], dx
        mov [oldirq16+2], cx
        mov cx, cs
        mov edx, irq
        mov ax, 205h
        int 31h
        jc bad
        sti
        mov ecx, 40000000
.wait:  cmp byte [irqseen], 0
        jne .seen
        dec ecx
        jnz .wait
.seen:  pushfd
        cli
        mov edx, [oldirq]
        mov cx, [oldirq+4]
        mov bx, 8
        mov ax, 205h
        int 31h
        popfd
        jc bad
        push ds
        push ds
        pop es
        push cs
        pop ds
        mov esi, callback
        mov edi, cbpacket
        mov eax, 303h
        int 31h
        pop ds
        jc bad
        mov [cbaddr], dx
        mov [cbaddr+2], cx
.repeat:
        mov byte [cbseen], 0
        mov word [rmregs+42], realcall
        mov ax, [rmseg]
        mov [rmregs+44], ax
        mov ax, [realstack]
        mov [rmregs+48], ax
        mov word [rmregs+46], 1000h
        mov edi, rmregs
        xor ecx, ecx
        mov eax, 301h
        mov bh, 1                ;A20 reset must survive nested callbacks
        int 31h
        jc bad
        cmp byte [cbseen], 2
        jne bad
        dec byte [rounds]
        jnz .repeat
        mov dx, [cbaddr]
        mov cx, [cbaddr+2]
        mov eax, 304h
        int 31h
        jc bad
        mov bx, 3
        mov edx, [oldexc]
        mov cx, [oldexc+4]
        mov ax, 203h
        int 31h
        jc bad

        cmp byte [irqseen], 1
        jne bad
        cmp byte [excseen], 2
        jb bad
        cmp byte [broken], 0
        jne bad
        mov dx, [realsel]
        mov eax, 101h
        int 31h
        jc bad
        mov ax, ss
        cmp ax, [original]
        jne bad
        mov si, ok
.print: lodsb
        test al, al
        jz good
        mov dl, al
        pushad
        mov ah, 2
        int 21h
        popad
        jmp .print
good:   mov ax, 4C00h
        int 21h
bad:    mov dl, [broken]
        add dl, '0'
        mov ah, 2
        int 21h
        mov dl, [cbseen]
        add dl, '0'
        mov ah, 2
        int 21h
        mov ax, 4C01h
        int 21h
check:  pushad
        mov ax, ss
        cmp ax, [original]
        je .broken
        movzx ebx, ax
        lsl eax, ebx
        jnz .broken
        cmp eax, 0fffh
        jne .broken
        ; Reentrant service on the locked stack must preserve this frame.
        mov eax, 604h
        int 31h
        jc .broken
        cmp cx, 1000h
        jne .broken
        popad
        ret
.broken:
        mov byte [broken], 1
        popad
        ret
exception:
        call check
        inc byte [excseen]
%if CLIENT32
        o32 retf
%else
        retf
%endif
irq:    pushad
        call check
        mov byte [irqseen], 1
%if !CLIENT32
        or esp, 12340000h       ;SS.B=0: only SP addresses the interrupted stack
%endif
        int3                  ;nested event must continue on this stack
%if CLIENT32
        pushfd
        call dword far [oldirq]
%else
        pushf
        call far [oldirq16]
%endif
        popad
%if CLIENT32
        iretd
%else
        iret
%endif
callback:
%if !CLIENT32
        and esp, 0FFFFh         ;use 32-bit local addressing on a word stack
%endif
        mov ax, [esi]
        mov [es:edi+42], ax
        mov ax, [esi+2]
        mov [es:edi+44], ax
        add word [es:edi+46], 4
        push ds
        mov ax, [cs:saveddata]
        mov ds, ax
        call check
%if !CLIENT32
        and esp, 0FFFFh         ;a word-stack API return leaves high ESP undefined
%endif
        int3                   ;exception nested inside callback, same locked stack
        inc byte [cbdepth]
%if !CLIENT32
        ; Preserve both opaque states, then briefly switch the alias base
        ; without any intervening API call or frame allocation.
        cmp byte [cbdepth], 1
        jne .inner_state
        mov edi, outerstate
        xor al, al
        call far [pmstate]
        jmp .state_done
.inner_state:
        mov bx, sp
        mov ax, [ss:bx]          ;callback's saved real-stack alias selector
        mov [alias], ax
        mov edi, innerstate
        xor al, al
        call far [pmstate]
        mov edi, outerstate
        mov al, 1
        call far [pmstate]
        push ds
        mov ds, [alias]
        cmp word [ds:20h], 1357h
        pop ds
        je .outer_ok
        mov byte [broken], 2
.outer_ok:
        mov edi, innerstate
        mov al, 1
        call far [pmstate]
        push ds
        mov ds, [alias]
        cmp word [ds:20h], 2468h
        pop ds
        je .state_done
        mov byte [broken], 3
.state_done:
        movzx esp, sp            ;the state-call IRET leaves high ESP undefined
        mov edi, cbpacket        ;packet address required by the outer copy
%endif
        cmp byte [cbdepth], 1
        jne .nested
        pushad
        sub esp, 52
        mov ecx, 50
        xor ebx, ebx
.save:  mov al, [es:edi+ebx]
        mov [ss:esp+ebx], al
        inc ebx
        loop .save
        mov word [rmregs+46], 1000h
        mov ax, [realalt]
        mov [rmregs+48], ax
        mov edi, rmregs
        xor ecx, ecx
        mov eax, 301h
        mov bh, 1                ;A20 reset must survive nested callbacks
        int 31h
        jnc .restore
        mov byte [broken], 1
.restore:
%if !CLIENT32
        and esp, 0FFFFh
%endif
        mov edi, [ss:esp+52]
        mov ecx, 50
        xor ebx, ebx
.copy:  mov al, [ss:esp+ebx]
        mov [es:edi+ebx], al
        inc ebx
        loop .copy
        add esp, 52
        popad
.nested:
        dec byte [cbdepth]
        inc byte [cbseen]
        pop ds
%if CLIENT32
        iretd
%else
        iret
%endif
realcall:
        mov ax, ss
        cmp ax, [cs:realalt]
        mov ax, 1357h
        jne .mark
        mov ax, 2468h
.mark:  mov [ss:20h], ax
        call far [cs:cbaddr]
        retf
rmseg: dw 0
realstack: dw 0
realalt: dw 0
realsel: dw 0
alias: dw 0
outerstate: times 16 db 0
innerstate: times 16 db 0
saveddata: dw 0
rounds: db 16
cbseen: db 0
cbdepth: db 0
cbaddr: dd 0
cbpacket: times 50 db 0
rmregs: times 50 db 0
pmstate: dd 0
         dw 0
statebuf: times 20 db 0A5h
entry: dd 0
oldexc: dd 0
        dw 0
oldirq16: dd 0
oldirq: dd 0
        dw 0
original: dw 0
excseen: db 0
irqseen: db 0
broken: db 0
%if CLIENT32
ok: db 'NATLOCK32 locked events: ok',13,10,0
%else
ok: db 'NATL16 locked events: ok',13,10,0
%endif
        times 1024 db 0
stacktop:
