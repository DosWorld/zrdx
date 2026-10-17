; Isolated native-entry bootstrap experiment. Not part of the ordinary build.
%include "../rdf/T.INC"
bits 32
global start
%ifndef CLIENT32
%define CLIENT32 1
%endif
TNAME 'ENTERH native bootstrap'
section .bss
rootss: resw 1
rootds: resw 1
rmseg: resw 1
rmsel: resw 1
cbaddr: resd 1
old21: resd 1
       resw 1
old31: resd 1
       resw 1
old2f: resd 1
cbregs: resb 50
queryregs: resb 50
execregs: resb 50
active: resd 1
entered: resd 1
mode32: resd 1
cbsp: resd 1
userip: resd 1
usersp: resd 1
usercsrm: resw 1
psprm: resw 1
envrm: resw 1
usercs: resw 1
userds: resw 1
userss: resw 1
useres: resw 1
envsel: resw 1
newsel: resw 1
nsel: resd 1
sels: resw 8
section .text
start: BEGIN
       mov [rootss], ss
       mov [rootds], ds
       mov ebx, (rmend-rmbegin+15)/16
       mov eax, 100h
       int 31h
       jc near bad
       mov [rmseg], ax
       mov [rmsel], dx
       mov es, dx
       xor edi, edi
       mov esi, rmbegin
       mov ecx, rmend-rmbegin
       cld
       rep movsb
       mov bl, 2fh
       mov eax, 200h
       int 31h
       jc near bad
       mov [es:rmold-rmbegin], dx
       mov [es:rmold-rmbegin+2], cx
       mov [old2f], dx
       mov [old2f+2], cx
       push ds
       pop es
       push ds
       push cs
       pop ds
       mov esi, native_entry
       mov edi, cbregs
       mov eax, 303h
       int 31h
       pop ds
       jc near bad
       mov [cbaddr], dx
       mov [cbaddr+2], cx
       mov es, [rmsel]
       mov [es:rmcallback-rmbegin], dx
       mov [es:rmcallback-rmbegin+2], cx
       mov ax, [rmseg]
       mov [es:rmblock-rmbegin+4], ax
       mov [es:rmblock-rmbegin+8], ax
       mov [es:rmblock-rmbegin+12], ax
       mov cx, ax
       mov dx, rmhook-rmbegin
       mov bl, 2fh
       mov eax, 201h
       int 31h
       jc near bad
       mov bl, 21h
       mov eax, 204h
       int 31h
       mov [old21], edx
       mov [old21+4], cx
       mov cx, cs
       mov edx, native21
       mov eax, 205h
       int 31h
       jc near bad
       mov bl, 31h
       mov eax, 204h
       int 31h
       mov [old31], edx
       mov [old31+4], cx
       mov cx, cs
       mov edx, native31
       mov eax, 205h
       int 31h
       jc near bad
       push ds
       pop es
       mov ax, [rmseg]
       mov [execregs+36], ax
       mov [execregs+34], ax
       mov [execregs+44], ax
       mov word [execregs+42], rmexec-rmbegin
       mov edi, execregs
       xor ecx, ecx
       mov eax, 301h
       int 31h
       jc near bad
       cmp word [execregs+28], 0
       jne near bad
       cmp dword [entered], 1
       jne near bad
       mov cx, [old2f+2]
       mov dx, [old2f]
       mov bl, 2fh
       mov eax, 201h
       int 31h
       mov cx, [cbaddr+2]
       mov dx, [cbaddr]
       mov eax, 304h
       int 31h
       mov dx, [rmsel]
       mov eax, 101h
       int 31h
       PASS

; 32-bit gate frames are retained; only pointer arguments are narrowed.
; Full copied-parameter and vector adaptation are separate pending work.
native31:
       cmp dword [cs:active], 0
       je .chain
       cmp dword [cs:mode32], 0
       jne .chain
       cmp ax, 000bh
       je .pointer
       cmp ax, 000ch
       je .pointer
       cmp ax, 0500h
       je .pointer
       cmp ax, 0303h
       je .pointer
       cmp ax, 0302h
       ja .chain
       cmp ax, 0300h
       jb .chain
.pointer:
       push esi
       push edi
       movzx edi, di
       cmp ax, 0303h
       jne .call
       movzx esi, si
.call: pushfd
       call far [cs:old31]
       pop edi
       pop esi
       iretd
.chain:
       jmp far [cs:old31]

native21:
       cmp ah, 4ch
       jne .chain
       cmp dword [cs:active], 0
       jne near native_exit
.chain:
       jmp far [cs:old21]

native_entry:
       cld
       mov [cs:cbsp], esp
       mov eax, [ds:esi]
       mov [es:edi+42], eax
       add word [es:edi+46], 4
       movzx eax, word [ds:esi+4]
       mov [cs:userip], eax
       mov ax, [ds:esi+6]
       mov [cs:usercsrm], ax
       push es
       pop ds
       mov eax, [cbregs+28]
       cmp ax, 1
       ja near entry_bad
       and eax, 1
       mov [mode32], eax
       mov dword [queryregs+28], 5100h
       mov edi, queryregs
       mov bl, 21h
       xor ecx, ecx
       mov eax, 300h
       int 31h
       jc near entry_bad
       mov ax, [queryregs+16]
       mov [psprm], ax
       movzx ebx, ax
       shl ebx, 4
       mov ax, [ebx+2ch]
       mov [envrm], ax
       movzx eax, ax
       mov ecx, 0ffffh
       mov edx, 0f2h
       call make_sel
       mov [envsel], ax
       movzx eax, word [psprm]
       mov ecx, 0ffh
       mov edx, 0f2h
       call make_sel
       mov [useres], ax
       movzx eax, word [usercsrm]
       mov ecx, 0ffffh
       mov edx, 0fah
       call make_sel
       mov [usercs], ax
       movzx eax, word [cbregs+36]
       mov ecx, 0ffffh
       mov edx, [mode32]
       shl edx, 14
       or edx, 0f2h
       call make_sel
       mov [userds], ax
       mov [userss], ax
       movzx eax, word [cbregs+48]
       cmp ax, [cbregs+36]
       je .same_ss
       mov ecx, 0ffffh
       call make_sel
       mov [userss], ax
.same_ss:
       movzx ebx, word [psprm]
       shl ebx, 4
       mov ax, [envsel]
       mov [ebx+2ch], ax
       movzx edi, word [cbregs+46]
       add di, 4             ;remove the original native-entry FAR return
       sub di, 12
       mov [usersp], edi
       movzx ebx, word [cbregs+48]
       shl ebx, 4
       add edi, ebx
       mov eax, [userip]
       mov [edi], eax
       movzx eax, word [usercs]
       mov [edi+4], eax
       movzx eax, word [cbregs+32]
       or eax, 3002h
       and eax, ~4001h
       mov [edi+8], eax
       mov dword [active], 1
       inc dword [entered]
       cli
       mov ax, [userss]
       mov ss, ax
       mov esp, [cs:usersp]
       mov ax, [cs:userds]
       mov ds, ax
       mov es, [cs:useres]
       xor eax, eax
       mov fs, ax
       mov gs, ax
       mov ebx, [cs:cbregs+16]
       mov ecx, [cs:cbregs+24]
       mov edx, [cs:cbregs+20]
       mov edi, [cs:cbregs]
       mov esi, [cs:cbregs+4]
       mov ebp, [cs:cbregs+8]
       mov eax, [cs:cbregs+28]
       iretd

native_exit:
       mov [cs:cbregs+28], al
       cli
       mov ax, [cs:rootss]
       mov ss, ax
       mov esp, [cs:cbsp]
       mov ax, [cs:rootds]
       mov ds, ax
       mov es, ax
       mov dword [active], 0
       movzx ebx, word [psprm]
       shl ebx, 4
       mov ax, [envrm]
       mov [ebx+2ch], ax
       mov cx, [old21+4]
       mov edx, [old21]
       mov bl, 21h
       mov eax, 205h
       int 31h
       mov cx, [old31+4]
       mov edx, [old31]
       mov bl, 31h
       mov eax, 205h
       int 31h
       call free_sels
       mov edi, cbregs
       push ds
       pop es
       and word [cbregs+32], ~1
       iretd

entry_bad:
       call free_sels
       mov edi, cbregs
       or word [cbregs+32], 1
       iretd

; Real segment in EAX, limit in ECX, access flags in EDX.
make_sel:
       pushad
       mov ecx, 1
       xor eax, eax
       int 31h
       jc near bad
       mov [newsel], ax
       mov edi, [nsel]
       mov [sels+edi*2], ax
       inc dword [nsel]
       mov bx, ax
       mov edx, [esp+28]
       shl edx, 4
       mov ecx, edx
       shr ecx, 16
       mov eax, 7
       int 31h
       jc near bad
       mov edx, [esp+24]
       mov ecx, edx
       shr ecx, 16
       mov eax, 8
       int 31h
       jc near bad
       mov ecx, [esp+20]
       mov eax, 9
       int 31h
       jc near bad
       popad
       mov ax, [newsel]
       ret

free_sels:
       mov esi, [nsel]
.next: test esi, esi
       jz .done
       dec esi
       mov bx, [sels+esi*2]
       mov eax, 1
       int 31h
       jmp .next
.done: mov [nsel], esi
       ret

bad:   FAIL

section .data
bits 16
rmbegin:
rmhook:
       cmp ax, 1687h
       jne .old
       xor ax, ax
       mov bx, 1
       mov cl, 3
       mov dx, 9
       xor si, si
       push cs
       pop es
       mov di, rmentry-rmbegin
       iret
.old:  jmp far [cs:rmold-rmbegin]
rmentry:
       call far [cs:rmcallback-rmbegin]
       jc .failed
       mov ah, 4ch
       int 21h
.failed:
       retf
rmexec:
       mov ax, 4b00h
       mov bx, rmblock-rmbegin
       mov dx, rmfile-rmbegin
       int 21h
       jc .return
       mov ah, 4dh
       int 21h
.return:
       retf
rmold: dd 0
rmcallback: dd 0
rmblock:
       dw 0, rmtail-rmbegin, 0, rmfcb-rmbegin, 0, rmfcb-rmbegin, 0
rmtail: db 0, 13
rmfcb: times 37 db 0
%if CLIENT32
rmfile: db 'ENTRY32.COM', 0
%else
rmfile: db 'ENTRY16.COM', 0
%endif
rmend:
