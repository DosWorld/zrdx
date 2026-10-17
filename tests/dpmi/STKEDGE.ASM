; Full-range software-INT stack preflight: faults and valid controls.
bits 16
org 100h
%ifndef CLIENT32
%define CLIENT32 0
%endif
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
        mov [oldss], ss
        mov bx, 12
        mov cx, cs
        mov edx, ssfault
        mov eax, 203h
        int 31h
        jc bad
        mov bx, 14
        mov edx, pfault
        mov eax, 203h
        int 31h
        jc bad
        xor bx, bx
        mov edx, soft
        mov eax, 205h
        int 31h
        jc bad
        mov ebx, 1
        xor ecx, ecx
        mov eax, 501h
        int 31h
        jc bad
        mov [handle], di
        mov [handle+2], si
        shl ebx, 16
        mov bx, cx
        mov [buffer], ebx
        mov ecx, 1
        xor eax, eax
        int 31h
        jc bad
        mov [alias], ax
        mov ax, 1Bh
        mov fs, ax
        ; Each tuple is base delta, limit, rights, SP, exception, error.
 .cycle:mov esi, cases
        mov byte [remaining], (cases_end-cases)/24
.next:  mov bx, [alias]
        mov edx, [esi]
        cmp edx, 80000000h
        jae .absolute
        add edx, [buffer]
.absolute:
        cmp edx, 90000000h
        je .base_ready
        and edx, 7FFFFFFFh
.base_ready:
        mov [base], edx
        mov ecx, edx
        shr ecx, 16
        mov eax, 7
        int 31h
        jc bad
        mov edx, [esi+4]
        mov ecx, edx
        shr ecx, 16
        mov eax, 8
        int 31h
        jc bad
        mov ecx, [esi+8]
        mov eax, 9
        int 31h
        jc bad
        mov eax, [esi+12]
        mov [stackpos], eax
        mov eax, [esi+16]
        mov [expected], al
        mov eax, [esi+20]
        mov [errwant], ax
        ; A cross-page failure must not write the preceding mapped page.
        mov edi, [buffer]
        add edi, 0FFF0h
        mov dword [fs:edi], 12345678h
        mov dword [fs:edi+4], 12345678h
        mov dword [fs:edi+8], 12345678h
        mov dword [fs:edi+12], 12345678h
        call probe
        jc bad
        cmp dword [esi], 0F000h
        jne .advance
        cmp dword [fs:edi], 12345678h
        jne bad
        cmp dword [fs:edi+4], 12345678h
        jne bad
        cmp dword [fs:edi+8], 12345678h
        jne bad
        cmp dword [fs:edi+12], 12345678h
        jne bad
.advance:
        add esi, 24
        dec byte [remaining]
        jnz .next
        dec byte [rounds]
        jnz .cycle
        mov bx, [alias]
        mov eax, 1
        int 31h
        jc bad
        mov di, [handle]
        mov si, [handle+2]
        mov eax, 502h
        int 31h
        jc bad
        mov si, ok
        xor bx, bx
        jmp print
probe:  mov byte [seen], 0
        mov word [errseen], 0
        mov [oldsp], esp
        cli
        mov ss, [alias]
        mov esp, [stackpos]
        std
        stc
fault_ip:
        int 0
resume: mov ss, [oldss]
        mov esp, [oldsp]
        pushfd
        pop dword [retflags]
        cld
        sti
        mov eax, [retflags]
        and eax, 601h
        cmp eax, 401h           ;client CF/DF and disabled IF restored
        jne .bad
        mov al, [expected]
        cmp [seen], al
        jne .bad
        mov ax, [errwant]
        cmp [errseen], ax
        jne .bad
        clc
        ret
.bad:   stc
        ret
soft:   cmp byte [expected], 0
        je .return
        mov byte [seen], 99
.return:
%if CLIENT32
        iretd
%else
        iret
%endif
ssfault:mov byte [seen], 12
        jmp handler
pfault: mov byte [seen], 14
handler:
        push bp
        mov bp, sp
        push eax
%if CLIENT32
%define ERR 10
%define IP 14
%define SPOS 26
%define SSEL 30
%else
%define ERR 6
%define IP 8
%define SPOS 14
%define SSEL 16
%endif
        pushf
        pop ax
        test ah, 2              ;S10.4 exception entry has interrupts disabled
        jnz .wrong
%if CLIENT32
        cmp dword [ss:bp+IP], fault_ip
%else
        cmp word [ss:bp+IP], fault_ip
%endif
        je .checked
.wrong: mov byte [seen], 98
.checked:
        mov ax, [ss:bp+ERR]
        mov [errseen], ax
%if CLIENT32
        mov dword [ss:bp+IP], resume
        mov eax, [oldsp]
        mov [ss:bp+SPOS], eax
%else
        mov word [ss:bp+IP], resume
        mov ax, [oldsp]
        mov [ss:bp+SPOS], ax
%endif
        mov ax, [oldss]
        mov [ss:bp+SSEL], ax
        pop eax
        pop bp
%if CLIENT32
        o32 retf
%else
        retf
%endif
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
cases:
        dd 0,0FFFFh,40F2h,4,12,0          ;underflow
        dd 0,0FFFFh,40F2h,90000004h,12,0 ;outside segment
        dd 0,0FFFFh,0F2h,4,12,0          ;word stack wrap crosses limit
        dd 0,0FFFFh,0F2h,0,0,0           ;word SP=0 exact wrap fits
        dd 0,0FFFFh,40F2h,1004h,0,0      ;valid crossing of mapped pages
        dd 0,0FFFh,40F6h,2000h,0,0       ;valid expand-down
        dd 0,0FFFh,40F6h,1004h,12,0      ;expand-down lower bound
        dd 90000000h,0FFFh,40F2h,1000h,14,6 ;absent PDE
        dd 0F000h,1FFFh,40F2h,1004h,14,6 ;second page absent, no partial stores
        ; KSTACK0 is mapped supervisor-only; use absolute base marker below.
        dd (KSTACK0 & ~0FFFh) | 80000000h,0FFFh,40F2h,1000h,14,7
cases_end:
oldsp: dd stacktop
oldss: dw 0
entry: dd 0
alias: dw 0
handle: dd 0
buffer: dd 0
base: dd 0
stackpos: dd 0
rounds: db 16
retflags: dd 0
remaining: db 0
seen: db 0
expected: db 0
errseen: dw 0
errwant: dw 0
ok: db 'STKEDGE bounds and page permissions: ok',13,10,0
fail: db 'STKEDGE: FAIL',13,10,0
        times 2048 db 0
stacktop:
