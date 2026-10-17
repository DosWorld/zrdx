bits 16
org 100h
start:
mov ax,1687h
int 2fh
test ax,ax
mov dx,nohost
jnz .say
mov dx,host
.say: mov ah,9
int 21h
mov ax,3567h
int 21h
mov ax,es
or ax,bx
jz .novcpi
mov ax,0de00h
int 67h
test ah,ah
mov dx,novcpi
jnz .say2
mov dx,vcpi
jmp .say2
.novcpi: mov dx,novcpi
.say2: mov ah,9
int 21h
mov ax,4300h
int 2fh
cmp al,80h
mov dx,noxms
jne .say3
mov dx,xms
.say3: mov ah,9
int 21h
mov ax,4c00h
int 21h
nohost: db 'External DPMI 1687: absent',13,10,'$'
host: db 'External DPMI 1687: present',13,10,'$'
vcpi: db 'VCPI DE00: present',13,10,'$'
novcpi: db 'VCPI DE00: absent',13,10,'$'
xms: db 'XMS 4300: present',13,10,'$'
noxms: db 'XMS 4300: absent',13,10,'$'
