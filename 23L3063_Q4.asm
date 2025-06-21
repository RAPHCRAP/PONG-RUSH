[org 0x0100]
jmp start

tickCount: dw 0

calDec:

push bp
mov bp,sp

pusha


shr word[bp+4],16
shl word[bp+6],16

mov ax,[bp+8]
mov bx,0xA

mov si,0

cdl1:

mov dx,0

div bx

mov cx,0

cdl2:

shr dx,1
rcr word[bp+4],1
rcr word[bp+6],1

inc cx 

cmp cx,4
jnz cdl2

inc si

cmp ax,0
jnz cdl1


shl si,2



cdl3:

mov ax,[bp+6]
shl ax,1

rcl word[bp+4],1
rcl word[bp+6],1

dec si

cmp si,0
jnz cdl3

popa

pop bp

ret

print:

push bp
mov bp,sp

pusha

mov ax,0xb800
mov es,ax

mov bx,[bp+6]
mov si,0x8

mov ax,[bp+4]

pl1:


mov cx,ax
and cx,0xf

shr ax,4

add cx,0x30

mov ch,0x57

sub si,2
mov [es:bx+si],cx

cmp si,0
jnz pl1

popa

pop bp


ret 4

delay :
push cx
mov cx, 3 ; change the values to increase delay time
delay_loop1 :
push cx
mov cx, 0xFFFF
delay_loop2 :
	loop delay_loop2
	pop cx
	loop delay_loop1
	pop cx
	ret

disp:

sub sp,4

push bp
mov bp,sp


pusha


push word[bp+8]
push word[bp+2]
push word[bp+4]

call calDec

pop word[bp+4]
pop word[bp+2]
pop word[bp+8]


mov ax,0xf90

push ax
push word[bp+4]

call print


mov ax,0xf98

push ax
push word[bp+2]

call print


popa

pop bp

add sp,4

ret 2

timer:
    push ax
    inc word [cs:tickCount]
    push word [cs:tickCount]
    call disp
    mov al, 0x20
    out 0x20, al
    pop ax
    iret

keyboardHandler:
    push ax
    in al, 0x60
    cmp al, 0x10
    je disableTimer
    cmp al, 0x90
    je enableTimer
    jmp endKeyboard

disableTimer:
    in al, 0x21
    or al, 0x01
    out 0x21, al
    jmp endKeyboard

enableTimer:
    in al, 0x21
    and al, 0xFE
    out 0x21, al

endKeyboard:
    mov al, 0x20
    out 0x20, al
    pop ax
    iret

start:
    xor ax, ax
    mov es, ax
    cli

    mov word [es:8*4], timer
    mov [es:8*4+2], cs

    mov word [es:9*4], keyboardHandler
    mov [es:9*4+2], cs

    sti

    mov dx, start
    add dx, 15
    mov cl, 4
    shr dx, cl
    mov ax, 0x3100
    int 0x21

oldHandler dd 0
programEnd:
