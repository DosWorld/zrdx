; Shared-VM parent/child ownership, not a fresh host per child.
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 1
%endif
%ifndef FATAL
%define FATAL 0
%endif
%ifndef IRQEXEC
%define IRQEXEC 0
%endif
%if IRQEXEC
%ifndef KERNELIRET
%error IRQEXEC needs the actual kernel IRQ trampoline offset
%endif
%endif
start:
        cld
        push cs
        pop ds
        push cs
        pop es
        mov sp, stacktop
        mov bx, (stacktop - $$ + 100h + 15) / 16
        mov ah, 4Ah
        int 21h
        jc near bad
        mov ax, cs
        mov [rmseg], ax
        mov [block+4], ax
        mov [block+8], ax
        mov [block+12], ax
        mov [regs+34], ax
        mov [regs+36], ax
        mov [regs+44], ax
        mov ax, [2Ch]
        mov [block], ax             ;real environment, not the converted PSP word
        movzx eax, word [rmseg]
        shl eax, 4
        add eax, watched
        mov [linear], eax
        mov ax, 1687h
        int 2Fh
        test ax, ax
        jnz near bad
        mov [entry], di
        mov [entry+2], es
        mov bx, si
        mov ah, 48h
        int 21h
        jc near bad
        mov es, ax
        mov ax, CLIENT32
        call far [entry]
        jc near bad
        push ds
        pop es
        mov ecx, 1
        xor eax, eax
        int 31h
        jc near bad
        mov [flat], ax
        mov bx, ax
        mov ecx, 0ffffh
        mov edx, ecx
        mov eax, 8
        int 31h
        jc near bad
        mov ecx, 0c0f2h
        mov eax, 9
        int 31h
        jc near bad
        mov fs, [flat]
        mov ebx, 10h
        xor ecx, ecx
        mov eax, 501h
        int 31h
        jc near bad
        shl ebx, 16
        mov bx, cx
        mov [normal], ebx
        mov ebx, 1001h
        mov eax, 100h
        int 31h
        jc near bad
        mov [dosmem], dx
        mov es, dx
        mov dword [es:0], 87654321h
        push ds
        pop es
        mov ebx, [normal]
        mov dword [fs:ebx], 12345678h
        call alloccb
        jc near bad
        call alloccb
        jc near bad
        mov [freecb], dx
        mov [freecb+2], cx
        mov eax, 304h
        int 31h
        jc near bad
        mov ebx, [linear]
        mov ecx, ebx
        shr ebx, 16
        mov edx, 0101h
        mov eax, 0B00h
        int 31h
        jc near bad
        mov [debug], bx
        mov ebx, 10h
        xor ecx, ecx
        xor esi, esi
        mov edi, 4096
        mov eax, 800h
        int 31h
        jc near bad
        shl ebx, 16
        mov bx, cx
        mov [mapped], ebx
        mov edi, before
        mov eax, 500h
        int 31h
        jc near bad
        mov word [runs], 4
%if IRQEXEC
        mov [irqdata], ds
        mov bx, 8
        mov eax, 204h
        int 31h
        jc near bad
%if CLIENT32
        mov [oldirq], edx
        mov [oldirq+4], cx
%else
        mov [oldirq], dx
        mov [oldirq+2], cx
%endif
        mov cx, cs
        mov edx, irqexec
        mov eax, 205h
        int 31h
        jc near bad
%endif
.child:
%if IRQEXEC
        mov word [attempts], 256
        mov byte [irqdone], 0
        mov byte [irqbusy], 1
.allocate:
        sti
        mov ebx, 80h
        xor ecx, ecx             ;8 MB: IRQ may interrupt the existing allocator
        mov eax, 501h
        int 31h
        jc near bad
        shl ebx, 16
        mov bx, cx
        mov ecx, 2048
.touch: mov dword [fs:ebx], 0ABCD1234h ;all recycled pages must be user accessible
        add ebx, 4096
        loop .touch
        mov eax, 502h            ;SI:DI is the returned handle
        int 31h
        jc near bad
        cmp byte [irqdone], 0
        jne .completed
        dec word [attempts]
        jnz .allocate
        jmp near bad
.completed:
        cmp byte [irqdone], 1
        jne near bad
        mov byte [irqbusy], 0
%else
        mov edi, regs
%if !CLIENT32
        or edi, 0dead0000h
%endif
        xor ecx, ecx
        mov eax, 301h
        int 31h
        jc near bad
%if !CLIENT32
        cmp edi, 0dead0000h + regs
        jne near bad
%endif
%endif
%if FATAL
        cmp word [regs+28], 0FFh
%else
        cmp word [regs+28], 0
%endif
        jne near bad
        mov edi, after
        mov eax, 500h
        int 31h
        jc near bad
        mov si, before
        mov di, after
        mov cx, 12
        cld
        repe cmpsd
        jne near bad
        mov es, [dosmem]
        cmp dword [es:0], 87654321h
        jne near bad
        push ds
        pop es
        mov ebx, [normal]
        cmp dword [fs:ebx], 12345678h
        jne near bad
        mov ebx, [mapped]
        mov eax, [fs:ebx]            ;parent mapping must still be present
        mov bx, [debug]
        mov eax, 0B02h
        int 31h
        jc near bad
        test ax, ax
        jnz near bad
        call alloccb
        jc near bad
        cmp dx, [freecb]
        jne near bad
        cmp cx, [freecb+2]
        jne near bad
        mov eax, 304h
        int 31h
        jc near bad
        dec word [runs]
        jnz .child
%if IRQEXEC
        mov bx, 8
%if CLIENT32
        mov edx, [oldirq]
        mov cx, [oldirq+4]
%else
        movzx edx, word [oldirq]
        mov cx, [oldirq+2]
%endif
        mov eax, 205h
        int 31h
        jc near bad
%endif
        mov si, ok
        xor bx, bx
        jmp short print
bad:    mov si, fail
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
alloccb:
        push ds
        push cs
        pop ds
        mov esi, callback
        mov edi, cbregs
        mov eax, 303h
        int 31h
        pop ds
        ret
callback:
%if CLIENT32
        iretd
%else
        iret
%endif
%if IRQEXEC
; Launch only when the timer interrupted CPL1, identified by the actual
; asynchronous kernel-return trampoline. A user-mode IRQ does not count.
irqexec:
        pushad
        push ds
        push es
        push fs
        push gs
        mov bx, sp
        cmp word [ss:bx+40], KERNELIRET
        jne .chain
        mov ax, [cs:irqdata]
        mov ds, ax
        push ds
        pop es
        cmp byte [irqbusy], 1
        jne .chain
        mov byte [irqbusy], 0
        mov edi, regs
        xor ebx, ebx             ;no reserved flags or A20-reset request
        xor ecx, ecx
        mov eax, 301h
        int 31h
        jc .broken
        mov byte [irqdone], 1
        jmp .chain
.broken:
        mov byte [irqdone], 2
.chain:
        pop gs
        pop fs
        pop es
        pop ds
        popad
%if CLIENT32
        jmp dword far [cs:oldirq]
%else
        jmp far [cs:oldirq]
%endif
irqdata: dw 0
attempts: dw 0
irqdone: db 0
irqbusy: db 0
oldirq: dd 0
        dw 0
%endif
rmexec: mov ax, 4B00h
        mov bx, block
        mov dx, child
        int 21h
        jc .return
        mov ah, 4Dh
        int 21h
.return:
        retf
entry:  dd 0
linear: dd 0
normal: dd 0
mapped: dd 0
freecb: dd 0
dosmem: dw 0
flat:   dw 0
debug:  dw 0
rmseg:  dw 0
runs:   dw 0
watched: db 0
cbregs: times 50 db 0
regs:   times 42 db 0
        dw rmexec, 0, 0, 0
block:  dw 0, tail, 0, 5Ch, 0, 6Ch, 0
tail:   db 0, 13
before: times 48 db 0
after:  times 48 db 0
%if FATAL = 2
%if CLIENT32
child:  db 'FRC32.COM', 0
ok:     db 'FRT32 invalid return cleanup: ok', 13, 10, 0
fail:   db 'FRT32 invalid return cleanup: FAIL', 13, 10, 0
%else
child:  db 'FRC16.COM', 0
ok:     db 'FRT16 invalid return cleanup: ok', 13, 10, 0
fail:   db 'FRT16 invalid return cleanup: FAIL', 13, 10, 0
%endif
%elif FATAL
%if CLIENT32
child:  db 'FCH32.COM', 0
ok:     db 'FEX32 fatal cleanup: ok', 13, 10, 0
fail:   db 'FEX32 fatal cleanup: FAIL', 13, 10, 0
%else
child:  db 'FCH16.COM', 0
ok:     db 'FEX16 fatal cleanup: ok', 13, 10, 0
fail:   db 'FEX16 fatal cleanup: FAIL', 13, 10, 0
%endif
%else
%if CLIENT32
child:  db 'OWNC32.COM', 0
%if IRQEXEC
ok:     db 'IRQ32 suspended kernel EXEC: ok', 13, 10, 0
%else
ok:     db 'NEST32 owned cleanup: ok', 13, 10, 0
%endif
fail:   db 'NEST32 owned cleanup: FAIL', 13, 10, 0
%else
child:  db 'OWNC16.COM', 0
%if IRQEXEC
ok:     db 'IRQ16 suspended kernel EXEC: ok', 13, 10, 0
%else
ok:     db 'NEST16 owned cleanup: ok', 13, 10, 0
%endif
fail:   db 'NEST16 owned cleanup: FAIL', 13, 10, 0
%endif
%endif
        times 2048 db 0
stacktop:
