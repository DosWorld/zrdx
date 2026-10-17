%include "../rdf/T.INC"
bits 32
global start
TNAME 'PHYSMAP ranges'
section .text
start: BEGIN
       xor ebx,ebx
       xor ecx,ecx
       xor esi,esi
       mov edi,4096
       call reject
       mov ebx,10h
       xor edi,edi
       call reject
       mov ebx,0ffffh
       mov ecx,0fff0h
       mov edi,32
       call reject
       mov ebx,0ffffh
       mov ecx,0ffffh
       mov edi,2
       call reject
       mov ebx,10h
       xor ecx,ecx
       mov esi,0fff0h
       xor edi,edi
       mov eax,800h
       int 31h
       jnc near bad
       xor esi,esi
       mov ebx,0ffffh
       mov ecx,0f001h
       mov edi,1
       mov eax,800h
       int 31h
       jc near bad
       test cx,0fffh
       jz near bad
       mov ebx,0ffffh
       mov ecx,0f000h
       mov edi,1000h
       mov eax,800h
       int 31h
       jc near bad
       test cx,0fffh
       jnz near bad
       PASS
reject:
       mov eax,800h
       int 31h
       jnc near bad
       cmp ax,8021h
       jne near bad
       ret
bad:   FAIL
