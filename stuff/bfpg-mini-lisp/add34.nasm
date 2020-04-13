; $ nasm -f elf64 -o add34.o add34.nasm && ld -o add34 add34.o && ./add34 ; echo $?
; 7

global _start

section .text
_start:
    ; a + b
    mov ebx, [a]    ; ebx = a
    add ebx, [b]    ; ebx += b

    ; exit; return value is ebx
    mov eax, 1
    int 80h

section .data
    a: db 0x3   ; a = 0x3
    b: db 0x4   ; b = 0x4
