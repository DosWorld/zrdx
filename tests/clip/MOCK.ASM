; Deterministic real-mode clipboard fixture, not a production driver.
bits 16
org 100h
        jmp install
oldvec: dd 0
length: dw 0
format: dw 1
handler:
        cmp ax,1700h
        jb near chain
        cmp ax,170ah
        ja near chain
        push ax
        push bx
        push cx
        push dx
        push si
        push di
        push ds
        push es
        push bp
        mov bp,sp
        push cs
        pop ds
        cld
        cmp ax,1700h
        je near identify
        cmp ax,1702h
        je near empty
        cmp ax,1703h
        je near put
        cmp ax,1704h
        je near size
        cmp ax,1705h
        je near get
        cmp ax,1709h
        je near compact
        cmp ax,170ah
        je near fail
ok:     mov ax,1
        jmp finish
identify:
        mov ax,434dh
        jmp finish
empty:  mov word [length],0
        jmp ok
compact:
        mov word [ss:bp+10],0
        mov ax,40002
        jmp finish
put:    test si,si
        jnz near fail
        cmp cx,40002
        ja near fail
        cmp dx,2
        je near checkbm
        cmp dx,82h
        jne near copyin
checkbm:
        cmp cx,12h
        jb near fail
        lea ax,[bx+12h]
        cmp [es:bx+0ah],ax
        jne near fail
        mov ax,es
        cmp [es:bx+0ch],ax
        jne near fail
copyin: mov [length],cx
        mov [format],dx
        mov si,bx
        mov di,buffer
        push ds
        push es
        push es
        pop ds
        push cs
        pop es
        rep movsb
        pop es
        pop ds
        jmp ok
size:   mov word [ss:bp+10],0
        cmp dx,[format]
        jne near fail
        mov ax,[length]
        jmp finish
get:    cmp dx,[format]
        jne near fail
        mov cx,[length]
        jcxz fail
        mov si,buffer
        mov di,bx
        rep movsb
        cmp dx,2
        je near fixbm
        cmp dx,82h
        jne near ok
fixbm:  lea ax,[bx+12h]
        mov [es:bx+0ah],ax
        mov ax,es
        mov [es:bx+0ch],ax
        jmp ok
fail:   xor ax,ax
finish: mov [ss:bp+16],ax
        pop bp
        pop es
        pop ds
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
        iret
chain:  jmp far [cs:oldvec]
install:
        mov ax,352fh
        int 21h
        mov [oldvec],bx
        mov [oldvec+2],es
        mov dx,handler
        mov ax,252fh
        int 21h
        mov dx,(endmem-$$+100h+15)/16
        mov ax,3100h
        int 21h
buffer: resb 40002
endmem:
