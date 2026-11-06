; Demand paging: allocate more than the free pool, touch every
; page, then verify forward and backward. Needs a SWAP build.
%include "../rdf/T.INC"
bits 32
global start
%ifndef TITLE
%define TITLE 'SWAPT demand paging'
%endif
TNAME TITLE
%ifdef LFNCHK
%define RMC
%endif
%ifdef NEST
%define RMC
%ifndef NESTDIV
%define NESTDIV 3
%endif
%endif
EXTRA equ 48
section .data
buf:    times 12 dd 0
total:  dd 0
sel:    dw 0
lin:    dd 0
section .text
start:  BEGIN
        mov ax, 6                  ;lock the stack: a swapped
        mov bx, ss                 ;stack faults at PL0
        int 31h
        jc near bad
        mov ebx, ecx
        shl ebx, 16
        mov bx, dx
        add ebx, esp
        and ebx, ~0FFFh
        sub ebx, 1000h
        mov ecx, ebx
        shr ebx, 16
        xor esi, esi
        mov edi, 2000h
        mov ax, 600h
        int 31h
        jc near bad
        push ds
        pop es
        mov edi, buf
        mov ax, 500h
        int 31h
        mov eax, [buf]
        shr eax, 12
%ifdef NEST
        xor edx, edx                ;parent: a third of the pool
        mov ecx, NESTDIV
        div ecx
%else
        add eax, EXTRA
%endif
        mov [total], eax
%ifdef FAILW
        shl eax, 12                ;the 3rd write fails: refused
        mov ecx, eax
        mov ebx, eax
        shr ebx, 16
        mov ax, 501h
        int 31h
        jnc near bad
        sub dword [total], 32      ;the next one succeeds
        mov eax, [total]
%endif
        shl eax, 12
        mov ecx, eax
        mov ebx, eax
        shr ebx, 16
        mov ax, 501h
        int 31h
        jc near bad
        mov edi, ebx
        shl edi, 16
        mov di, cx
        xor eax, eax
        mov ecx, 1
        int 31h
        jc near bad
        mov [sel], ax
        mov ebx, eax
        mov ecx, edi
        shr ecx, 16
        mov edx, edi
        mov ax, 7
        int 31h
        jc near bad
        mov ecx, [total]
        shl ecx, 12
        dec ecx
        mov edx, ecx
        shr ecx, 16
        movzx ebx, word [sel]
        mov ax, 8
        int 31h
        jc near bad
        mov [lin], edi
        call locks
        mov fs, [sel]
        xor ecx, ecx
.fill:  mov eax, ecx
        xor eax, 0A5A50000h
        mov edx, ecx
        shl edx, 12
        mov [fs:edx], eax
        not eax
        mov [fs:edx + 4092], eax
        inc ecx
        cmp ecx, [total]
        jb .fill
%ifdef NEST
        call spawn
%endif
        xor ecx, ecx
.fwd:   call check
        inc ecx
        cmp ecx, [total]
        jb .fwd
        mov ecx, [total]
.back:  dec ecx
        call check
        or ecx, ecx
        jnz .back
        mov edx, 8000h
        mov ax, 601h
        call lk
        jc near bad
%ifdef CRASH
        ud2
%else
%ifdef LFNCHK
        call lfnchk
%endif
        PASS
%endif
; Page locks: count 0..3, errors, then the first 8 pages
; stay locked (count 1) during the fill and the checks.
locks:  mov edx, 8000h
        mov ax, 600h
        call lk
        jc near bad
        mov ax, 600h
        call lk
        jc near bad
        mov ax, 600h
        call lk
        jc near bad
        mov ax, 600h
        call lk
        jnc near bad
        cmp ax, 8013h
        jne near bad
        mov ax, 601h
        call lk
        jc near bad
        mov ax, 601h
        call lk
        jc near bad
        mov ax, 601h
        call lk
        jc near bad
        mov ax, 601h
        call lk
        jnc near bad
        cmp ax, 8002h
        jne near bad
        xor edx, edx
        mov ax, 600h
        call lk
        jnc near bad
        cmp ax, 8021h
        jne near bad
        mov edx, 1000h
        mov ax, 600h
        mov ebx, 7FFF0000h
        call lkat
        jnc near bad
        cmp ax, 8023h
        jne near bad
        mov edx, 8000h
        mov ax, 600h
        call lk
        jc near bad
        ret
;in: ax = 600h/601h, edx = bytes; locks from [lin]
lk:     mov ebx, [lin]
;in: ebx = linear address
lkat: mov ecx, ebx
        shr ebx, 16
        mov esi, edx
        shr esi, 16
        mov edi, edx
        int 31h
        ret
check:  mov eax, ecx
        xor eax, 0A5A50000h
        mov edx, ecx
        shl edx, 12
        cmp [fs:edx], eax
        jne near bad
        not eax
        cmp [fs:edx + 4092], eax
        jne near bad
        ret
%ifdef LFNCHK
; The swap file must be in the long TEMP directory (made with
; LFN): find it with a real mode INT 21h 714Eh (CF set before).
lfnchk: mov ax, 100h
        mov ebx, 40h               ;1 KB DOS block
        int 31h
        jc near bad
        mov [rseg], ax
        mov [rsel], dx
        mov es, dx
        xor edi, edi
        mov esi, lpat
        mov ecx, lpatn
        cld
        rep movsb
        push ds
        pop es
        mov edi, rmcall
        xor eax, eax
        mov ecx, 50 / 2
        rep stosw
        mov dword [rmcall + 28], 714Eh   ;EAX
        mov dword [rmcall + 24], 7       ;ECX: any attributes
        mov dword [rmcall + 4], 1        ;ESI: DOS date format
        mov dword [rmcall + 0], 100h     ;EDI: find data
        mov word [rmcall + 32], 1        ;flags: CF set
        mov ax, [rseg]
        mov [rmcall + 34], ax            ;ES
        mov [rmcall + 36], ax            ;DS
        mov ax, 300h
        mov bx, 21h
        xor ecx, ecx
        mov edi, rmcall
        int 31h
        jc near bad
        test word [rmcall + 32], 1
        jnz near bad                     ;no file in the long dir
        mov ax, 101h
        mov dx, [rsel]
        int 31h
        ret
section .data
lpat:   db 'C:\Long Swap Directory\ZRDX*.TMP', 0
lpatn   equ $ - lpat
section .text
%endif
%ifdef RMC
section .data
rseg:   dw 0
rsel:   dw 0
rmcall: times 50 db 0
section .text
%endif
%ifdef NEST
; Run SWAPCH.EXE (a second client of this host) with a real
; mode INT 21h 4B00h; its exit code must be 0 and the pool
; must be back (within a few pages) when it returns.
spawn:  mov ax, 6                 ;the register block of a
        mov bx, ds                 ;simulated call is written
        int 31h                    ;at PL0 on return: lock it
        shl ecx, 16
        mov cx, dx
        lea ebx, [ecx + rmcall]
        mov ax, 600h
        mov edx, 64
        call lkat
        jc near bad
        mov edi, buf
        mov ax, 500h
        int 31h
        mov eax, [buf]
        mov [freeb], eax
        mov ax, 100h
        mov ebx, 40h               ;1 KB DOS block
        int 31h
        jc near bad
        mov [rseg], ax
        mov [rsel], dx
        mov es, dx
        xor edi, edi
        mov esi, spimg
        mov ecx, spimgn
        cld
        rep movsb
        mov ax, [rseg]
        mov [es:30h + 4], ax       ;tail, FCB1, FCB2 segments
        mov [es:30h + 8], ax
        mov [es:30h + 12], ax
        push ds
        pop es
        call rmint21
        test word [rmcall + 32], 1
        jnz near bad               ;exec failed
        mov dword [rmcall + 28], 4D00h
        call rmint21               ;exit code of the child
        cmp byte [rmcall + 28], 0
        jne near bad
        mov edi, buf
        mov ax, 500h
        int 31h
        mov eax, [buf]
        add eax, 8 * 4096          ;a few pages of slack
        cmp eax, [freeb]
        jb near bad
        mov ax, 101h
        mov dx, [rsel]
        int 31h
        ret
;real mode INT 21h; EAX is in rmcall[28] (4B00h at first)
rmint21:
        mov edi, rmcall
        xor eax, eax
        mov ecx, 50 / 2
        rep stosw
        cmp dword [mode], 0
        jne .go
        mov dword [mode], 1
        mov dword [rmcall + 28], 4B00h
        jmp short .regs
.go:    mov dword [rmcall + 28], 4D00h
.regs:  mov dword [rmcall + 16], 30h   ;EBX: parameter block
        mov ax, [rseg]
        mov [rmcall + 34], ax          ;ES
        mov [rmcall + 36], ax          ;DS (name at offset 0)
        mov word [rmcall + 32], 1      ;flags: CF set
        mov ax, 300h
        mov bx, 21h
        xor ecx, ecx
        mov edi, rmcall
        int 31h
        jc near bad
        ret
section .data
freeb:  dd 0
mode:   dd 0
; DOS block image: +0 name, +20h command tail, +30h EXEC block
spimg:  db 'SWAPCH.EXE', 0
        times 20h - ($ - spimg) db 0
        db 0, 13
        times 30h - ($ - spimg) db 0
        dw 0, 20h, 0, 40h, 0, 40h, 0
        times 40h - ($ - spimg) db 0
spimgn  equ $ - spimg
section .text
%endif
bad:    FAIL
