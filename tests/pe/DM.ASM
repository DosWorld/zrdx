; DllMain fixture (task 011): prints LETTER on attach and the
; lower case letter on detach, returns RESULT on attach.
bits 32
%ifndef BASE
%define BASE 401000h
%endif
org BASE
%ifndef LETTER
%define LETTER 65
%endif
%ifndef RESULT
%define RESULT 1
%endif
        mov eax, [esp + 8]
        or eax, eax
        jz detach
        cmp eax, 1
        jne done
        mov dl, LETTER
        jmp show
detach: mov dl, LETTER + 20h
show:   mov ah, 2
        int 21h
done:   mov eax, RESULT
        ret
