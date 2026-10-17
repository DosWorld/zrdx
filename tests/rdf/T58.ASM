%include "t.inc"
bits 32
global start
TNAME 'T58 dpmi real-mode selectors'
section .data
segments: dw 0A000h, 0040h, 0B000h, 0B800h, 0000h, 0FFFFh
pspseg: dw 0
PSPIDX equ (pspseg - segments) / 2
NGEN equ 16
generated: times NGEN dw 0
NSEG equ ($ - segments) / 2
section .bss
selectors: resw NSEG
desc:   resb 8
section .text
start:  BEGIN
        push ds
        pop es
        mov eax, 6200h
        int 21h
        mov eax, 6
        xor ecx, ecx
        xor edx, edx
        int 31h
        jc near bad
        shl ecx, 16
        mov cx, dx
        test ecx, 0Fh
        jnz near bad
        cmp ecx, 0FFFF0h
        ja near bad
        cmp word [ecx], 20CDh
        jne near bad
        shr ecx, 4
        mov [pspseg], cx
        mov eax, ecx
        mov edi, generated
        mov ecx, NGEN
.generate:
        imul eax, eax, 25173
        add eax, 13849
        mov [edi], ax
        add edi, 2
        loop .generate
        xor esi, esi
.map:   movzx ebx, word [segments + esi * 2]
        mov eax, 2
        int 31h
        jc near bad
        mov [selectors + esi * 2], ax
        movzx ebx, ax
        call query
        inc esi
        cmp esi, NSEG
        jb .map

        xor esi, esi
.again: movzx ebx, word [segments + esi * 2]
        mov eax, 2
        int 31h
        jc near bad
        cmp ax, [selectors + esi * 2]
        jne near bad
        movzx ebx, ax
        call query

        mov eax, 0Ah
        int 31h
        jc near bad
        movzx ebx, ax
        call query
        mov eax, 1
        int 31h
        jc near bad
        call invalid_queries

        movzx ebx, word [selectors + esi * 2]
        mov eax, 7
        xor ecx, ecx
        xor edx, edx
        int 31h
        call rejected
        mov eax, 8
        xor ecx, ecx
        mov edx, 0FFFFh
        int 31h
        call rejected
        mov eax, 9
        mov ecx, 0F3h
        int 31h
        call rejected
        mov eax, 0Ch
        mov edi, desc
        int 31h
        call rejected
        mov eax, 1
        int 31h
        call rejected
        call query
        inc esi
        cmp esi, NSEG
        jb near .again

        xor ebx, ebx
        call invalid_queries
        mov ebx, 8
        call invalid_queries
        PASS

query:  mov eax, 6
        xor ecx, ecx
        xor edx, edx
        int 31h
        jc near bad
        shl ecx, 16
        mov cx, dx
        movzx eax, word [segments + esi * 2]
        shl eax, 4
        cmp ecx, eax
        jne near bad
        lsl edx, ebx
        jnz near bad
        cmp edx, 0FFFFh
        jne near bad

        mov eax, 0Bh
        mov edi, desc
        int 31h
        jc near bad
        cmp word [desc], 0FFFFh
        jne near bad
        test byte [desc + 6], 8Fh
        jnz near bad
        movzx eax, byte [desc + 7]
        shl eax, 8
        mov al, [desc + 4]
        shl eax, 16
        mov ax, [desc + 2]
        movzx edx, word [segments + esi * 2]
        shl edx, 4
        cmp eax, edx
        jne near bad
        cmp esi, PSPIDX
        jne .done
        push fs
        mov fs, bx
        cmp word [fs:0], 20CDh
        pop fs
        jne near bad
.done:  ret

invalid_queries:
        mov eax, 6
        int 31h
        call rejected
        mov eax, 0Bh
        mov edi, desc
        int 31h
        call rejected
        ret

rejected:
        jnc bad
        cmp ax, 8022h
        jne bad
        ret
bad:    FAIL
