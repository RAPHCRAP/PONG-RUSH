[org 0x0100]



jmp start

dec1:dw 0
dec2: dw 0

tick: dw 0

hr: dw 0
min: dw 0
sec: dw 0
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

star:
pusha

mov ax,0xb800
mov es,ax

cmp word[tick],18
jnz skip

mov word[tick],0
inc word[sec]

cmp word[sec],60
jnz skip

mov word[sec],0
inc word[min]

cmp word[min],60
jnz skip

mov word[min],0
inc word[hr]

skip:

mov ax,0x073a
mov [es:8],ax
mov [es:18],ax

push word [sec]
push word[dec1]
push word[dec2]

call calDec

pop word[dec2]
pop word[dec1]
pop word [sec]


mov ax,20

push ax
push word[dec1]

call print

push word [min]
push word[dec1]
push word[dec2]

call calDec

pop word[dec2]
pop word[dec1]
pop word [min]


mov ax,10

push ax
push word[dec1]

call print

push word [hr]
push word[dec1]
push word[dec2]

call calDec

pop word[dec2]
pop word[dec1]
pop word [hr]


mov ax,0

push ax
push word[dec1]

call print


inc word[tick]

mov al,0x20
out 0x20,al

popa
iret


start:



cli
xor ax,ax
mov es,ax
mov word [es:8*4],star
mov word [es:8*4+2],cs
sti


jmp $

mov ax,0x4c00
int 21h
