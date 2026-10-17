%include "../rdf/T.INC"
bits 32
global start
TNAME 'CBACK callback lifecycle'
section .bss
slots: resd 20
count: resd 1
round: resd 1
hits: resd 1
cbregs: resb 50
rmregs: resb 50
section .text
start: BEGIN
       push ds
       pop es
.cycle: mov dword [count],0
.alloc: push ds
       push cs
       pop ds
       mov esi,handler
       mov edi,cbregs
       mov eax,303h
       int 31h
       pop ds
       jc near .full
       mov eax,[count]
       cmp eax,20
       jae near bad
       mov [slots+eax*4],dx
       mov [slots+eax*4+2],cx
       inc dword [count]
       jmp .alloc
.full: cmp dword [count],19 ;the extender owns one of the 20 slots
       jne near bad
       mov dx,[slots]
       mov cx,[slots+2]
       mov [rmregs+42],dx
       mov [rmregs+44],cx
       mov dword [rmregs+46],0
       mov edi,rmregs
       xor ecx,ecx
       mov eax,301h
       int 31h
       jc near bad
       mov eax,[round]
       inc eax
       cmp [hits],eax
       jne near bad
.free: dec dword [count]
       mov eax,[count]
       mov dx,[slots+eax*4]
       mov cx,[slots+eax*4+2]
       mov eax,304h
       int 31h
       jc near bad
       mov eax,304h
       int 31h
       jnc near bad
       cmp dword [count],0
       jne near .free
       inc dx
       mov eax,304h
       int 31h
       jnc near bad
       inc cx
       mov eax,304h
       int 31h
       jnc near bad
       inc dword [round]
       cmp dword [round],3
       jb near .cycle
       PASS
handler:
       mov ax,[esi]
       mov [es:edi+42],ax
       mov ax,[esi+2]
       mov [es:edi+44],ax
       add word [es:edi+46],4
       push ds
       push es
       pop ds
       inc dword [hits]
       pop ds
       iretd
bad:   FAIL
