; PE relocation self-check. Every relocated pointer must equal
; the real address. Build: nasm -f bin -DBASE=<code address>.
; Code page 0, data page 1 (48 table entries, one unaligned,
; the last dword ends the image page).
bits 32
org BASE
%define REL(x) ((x) - start)
COUNT equ 48

start:
        call here
here:   pop ebx
        sub ebx, REL(here)
        lea esi, [ebx + REL(table)]
        lea edi, [ebx + REL(dat)]
        xor ecx, ecx
nxt:    lea eax, [edi + ecx]
        cmp [esi + ecx*4], eax
        jne fail
        inc ecx
        cmp ecx, COUNT
        jb nxt
        mov eax, dat
        cmp eax, edi
        jne fail
        cmp [lastptr], ebx
        jne fail
        mov edx, okmsg
        xor eax, eax
        jmp short done
fail:   mov edx, badmsg
        mov eax, 1
done:   push eax
        mov ah, 9
        int 21h
        pop eax
        mov ah, 4Ch
        int 21h
okmsg:  db 'PE reloc: ok', 13, 10, '$'
badmsg: db 'PE reloc: BAD', 13, 10, '$'
        times 1000h - ($ - $$) db 0
dat:    times 64 db 0
        db 0
table:
%assign i 0
%rep COUNT
        dd dat + i
%assign i i + 1
%endrep
        times 2000h - 4 - ($ - $$) db 0
lastptr: dd start
