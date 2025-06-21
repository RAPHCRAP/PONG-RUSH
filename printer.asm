[org 0x0100]
jmp start

calDec:

push bp
mov bp,sp

push ax
push bx
push cx
push dx
push si


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

pop si
pop dx
pop cx
pop bx
pop ax

pop bp

ret

print:

push bp
mov bp,sp

push ax
push bx
push dx
push cx
push si

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

pop si
pop cx
pop dx
pop bx
pop ax

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


push ax


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


pop ax

pop bp

add sp,4

ret 2

start:

mov cx,0xffff

l1:

push cx
call disp

call delay
 
loop l1


mov ax,0x4c00
int 0x21