; Real default exceptions and the separate software-interrupt vectors.
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 0
%endif
%ifndef ACTUALNM
%define ACTUALNM 0
%endif
start:  cld
        mov sp, stacktop
        push cs
        pop ds
        push cs
        pop es
        mov [rmseg], cs
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
        push ds
        push ds
        pop es
        push cs
        pop ds
        mov esi, clear_ts
        mov edi, cbpacket
        mov eax, 303h
        int 31h
        pop ds
        jc bad
        mov [cbrm], dx
        mov [cbrm+2], cx
        xor esi, esi
.hook:  cmp si, 6
        je .next
        mov bx, si
        mov eax, 200h
        int 31h
        jc bad
        mov [oldrm+esi*4], dx
        mov [oldrm+esi*4+2], cx
        mov cx, [rmseg]
        mov dx, real_handler
        mov eax, 201h
        int 31h
        jc bad
.next:  inc si
        cmp si, 8
        jb .hook
        mov byte [step], 1
        mov byte [which], 0
        mov eax, 1000h
        xor edx, edx
        xor ebx, ebx
        div ebx                 ;#DE must reflect and resume with BX fixed
        cmp ebx, 1
        jne bad
        mov byte [which], 1
        pushf
        pop ax
        or ah, 1
        push ax
        popf
        nop                     ;#DB, real hook clears TF in its IRET frame
        mov byte [which], 2
        int 2                   ;software NMI vector default, not physical NMI
        mov byte [which], 3
        int3
        mov byte [which], 4
        mov al, 7Fh
        add al, 1
        into
        mov byte [which], 5
        mov eax, 2
        bound eax, [bounds]     ;#BR real hook fixes EAX before retry
        mov byte [which], 7
%if ACTUALNM
        mov eax, cr0
        or al, 8
        mov cr0, eax
        fnop                    ;actual #NM, protected callback clears TS
%endif
        int 7                   ;also verify the separate software default
        cmp byte [hits], 7 + ACTUALNM
        jne bad
        mov byte [step], 2
        mov bx, 3
        mov eax, 202h
        int 31h
        jc bad
%if CLIENT32
        mov [oldexc], edx
        mov [oldexc+4], cx
%else
        mov [oldexc], dx
        mov [oldexc+2], cx
%endif
        mov cx, cs
        mov edx, chain
        mov eax, 203h
        int 31h
        jc bad
        mov byte [which], 3
        int3                    ;client handler chains to its saved default
        cmp byte [hits], 8 + ACTUALNM
        jne bad
        call restore_exc
        mov byte [step], 3
        xor bx, bx
        mov eax, 202h
        int 31h
%if CLIENT32
        mov [cpu0], edx
        mov [cpu0+4], cx
%else
        mov [cpu0], dx
        mov [cpu0+2], cx
%endif
        mov cx, cs
        mov edx, cpu_handler
        mov eax, 203h
        int 31h
        jc bad
        mov eax, 204h
        int 31h
%if CLIENT32
        mov [soft0], edx
        mov [soft0+4], cx
%else
        mov [soft0], dx
        mov [soft0+2], cx
%endif
        mov cx, cs
        mov edx, software_handler
        mov eax, 205h
        int 31h
        jc bad
        int 0                   ;CD 00 must use ClientIDT, not ClientExc
        cmp byte [softseen], 1
        jne bad
        cmp byte [cpuseen], 0
        jne bad
%if CLIENT32
        mov edx, [cpu0]
        mov cx, [cpu0+4]
%else
        movzx edx, word [cpu0]
        mov cx, [cpu0+2]
%endif
        xor bx, bx
        mov eax, 203h
        int 31h
        jc bad
        mov byte [step], 4
        mov eax, 1000h
        xor edx, edx
        xor ebx, ebx
        div ebx                 ;CPU default must follow hooked ClientIDT[0]
        cmp ebx, 1
        jne bad
        cmp byte [softseen], 2
        jne bad
%if CLIENT32
        mov edx, [soft0]
        mov cx, [soft0+4]
%else
        movzx edx, word [soft0]
        mov cx, [soft0+2]
%endif
        mov eax, 205h
        int 31h
        jc bad
        xor si, si
.unhook:
        cmp si, 6
        je .skip
        mov dx, [oldrm+esi*4]
        mov cx, [oldrm+esi*4+2]
        mov bx, si
        mov eax, 201h
        int 31h
        jc bad
.skip:  inc si
        cmp si, 8
        jb .unhook
        mov cx, [cbrm+2]
        mov dx, [cbrm]
        mov eax, 304h
        int 31h
        jc bad
        mov si, ok
        xor bx, bx
        jmp print
restore_exc:
%if CLIENT32
        mov edx, [oldexc]
        mov cx, [oldexc+4]
%else
        movzx edx, word [oldexc]
        mov cx, [oldexc+2]
%endif
        mov bx, 3
        mov eax, 203h
        int 31h
        jc bad
        ret
chain:
%if CLIENT32
        jmp dword far [cs:oldexc]
%else
        jmp far [cs:oldexc]
%endif
cpu_handler:
        inc byte [cpuseen]
%if CLIENT32
        o32 retf
%else
        retf
%endif
software_handler:
        push ax
        mov ax, ss
        cmp byte [step], 4
        je .cpu
        cmp ax, [original]
        jne .wrong
        jmp .count
.cpu:   mov ebx, 1               ;retry DIV even if a stack check fails
        cmp ax, [original]
        je .wrong               ;CPU default must use the locked stack
.count: inc byte [softseen]
.wrong: pop ax
%if CLIENT32
        iretd
%else
        iret
%endif
clear_ts:
        mov ax, [esi]
        mov [es:edi+42], ax
        mov ax, [esi+2]
        mov [es:edi+44], ax
        add word [es:edi+46], 4
        mov eax, cr0
        and al, ~8
        mov cr0, eax
%if CLIENT32
        iretd
%else
        iret
%endif
real_handler:
        inc byte [cs:hits]
        cmp byte [cs:which], 0
        jne .debug
        mov ebx, 1
.debug: cmp byte [cs:which], 1
        jne .bound
        push bp
        mov bp, sp
        and word [ss:bp+6], ~100h
        pop bp
.bound: cmp byte [cs:which], 5
        jne .fpu
        xor eax, eax
.fpu:   cmp byte [cs:which], 7
        jne .done
        call far [cs:cbrm]
.done:  iret
bad:    mov dl, [step]
        add dl, '0'
        mov ah, 2
        int 21h
        mov dl, [which]
        add dl, '0'
        mov ah, 2
        int 21h
        mov dl, [hits]
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
cbrm: dd 0
cbpacket: times 50 db 0
entry: dd 0
rmseg: dw 0
original: dw 0
oldrm: times 32 db 0
oldexc: times 6 db 0
cpu0: times 6 db 0
soft0: times 6 db 0
bounds: dd -1, 1
which: db 0
hits: db 0
softseen: db 0
cpuseen: db 0
step: db 0
%if ACTUALNM
%if CLIENT32
ok: db 'NMX32 actual FPU exception: ok',13,10,0
%else
ok: db 'NMX16 actual FPU exception: ok',13,10,0
%endif
%elif CLIENT32
ok: db 'DEX32 default exceptions: ok',13,10,0
%else
ok: db 'DEX16 default exceptions: ok',13,10,0
%endif
fail: db ' default exceptions: FAIL',13,10,0
        times 1024 db 0
stacktop:
