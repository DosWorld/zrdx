bad:   mov ebx, eax
       mov ecx, 8
.hex:  rol ebx, 4
       mov dl, bl
       and dl, 15
       add dl, '0'
       cmp dl, '9'
       jbe .print
       add dl, 7
.print:
       call .put
       loop .hex
       mov dl, '/'
       call .put
       mov dl, [step]
       add dl, '0'
       call .put
       mov dl, ' '
       call .put
       FAIL
.put:  pushad
       mov ah, 2
       int 21h
       popad
       ret
