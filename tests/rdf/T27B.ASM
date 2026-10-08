bits 32
global fb
extern fc
section .text
fb:     call fc
        call fc
        ret
