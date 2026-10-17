%include "../rdf/T.INC"
bits 32
global start
section .data
m1: db '1686 protected detection: $'
m2: db '0A00 unknown vendor rejected: $'
m3: db '0303/0304 callback round trip: $'
yes: db 'PASS',13,10,'$'
no: db 'FAIL',13,10,'$'
vendor: db 'NO_SUCH_ZRDX_VENDOR',0
section .bss
rmregs: resb 50
failures: resb 1
section .text
start:
PRINT m1
mov eax,1686h
int 2fh
test ax,ax
call result
PRINT m2
mov esi,vendor
mov eax,0a00h
int 31h
cmc
push ds
pop es
call cfresult
PRINT m3
push ds
push cs
pop ds
mov esi,callback
mov edi,rmregs
mov eax,0303h
int 31h
pop ds
jc .bad
mov eax,0304h
int 31h
jmp .cbdone
.bad: stc
.cbdone:
call cfresult
movzx eax,byte [failures]
mov ah,4ch
int 21h
callback: iretd
result:
mov edx,yes
jz .show
mov edx,no
inc byte [failures]
.show: mov ah,9
int 21h
ret
cfresult:
mov edx,yes
jnc .show
mov edx,no
inc byte [failures]
.show: mov ah,9
int 21h
ret
