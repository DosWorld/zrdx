; Pentium/QEMU timing fixture, not part of the 386 host implementation.
bits 16
org 100h
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
        mov ax, 1
        call far [entry]
        jc bad
        mov [pmds], ds
        push ds
        pop es
        ; Same no-op handler: low INT exercises #GP decoding and preflight;
        ; high INT is the existing gate path. Both use the application stack.
        xor ebx, ebx
        mov cx, cs
        mov edx, soft
        mov eax, 205h
        int 31h
        jc bad
        mov ebx, 60h
        mov eax, 205h
        int 31h
        jc bad
        mov si, metrics
        mov word [function], 3
        call rounds
        mov word [function], 604h
        call rounds
        mov word [function], 6
        call rounds
        mov word [function], 0F000h
        call rounds
        mov word [function], 0F060h
        call rounds
        mov ax, [rmseg]
        mov [packet+36], ax
        mov [packet+44], ax
        mov edi, packet
        xor ebx, ebx
        xor ecx, ecx
        mov eax, 301h
        int 31h
        jc bad
        test word [packet+32], 1
        jnz bad
        mov si, ok
        xor bx, bx
        jmp print
rounds: mov bp, 3
.next:  call measure
        add si, 8
        dec bp
        jnz .next
        ret
measure:
        pushad
        xor eax, eax
        cpuid
        rdtsc
        mov [stamp], eax
        mov [stamp+4], edx
        mov edi, 10000
.loop:  movzx eax, word [function]
        mov bx, [pmds]
        cmp ax, 0F000h
        jb .api
        je .low
        int 60h
        jmp .next
.low:   int 0
        jmp .next
.api:   int 31h
.next:  dec edi
        jnz .loop
        rdtsc
        sub eax, [stamp]
        sbb edx, [stamp+4]
        mov [si], eax
        mov [si+4], edx
        popad
        ret
soft:   iretd
writer: mov dx, filename
        xor cx, cx
        mov ah, 3Ch
        int 21h
        jc .done
        mov bx, ax
        mov cx, 120
        mov dx, metrics
        mov ah, 40h
        int 21h
        pushf
        push ax
        mov ah, 3Eh
        int 21h
        pop ax
        popf
        jc .done
        cmp ax, 120
        je .done
        stc
.done:  retf
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
entry: dd 0
rmseg: dw 0
pmds: dw 0
function: dw 0
stamp: dq 0
metrics: times 120 db 0
packet: times 42 db 0
        dw writer, 0, 0, 0
filename: db 'LATENC.BIN',0
ok: db 'LATENC common calls: ok',13,10,0
fail: db 'LATENC common calls: FAIL',13,10,0
        times 1024 db 0
stacktop:
