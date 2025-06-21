[org 0x0100]


jmp start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|||||||> VARIABLES <||||||;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; // STORE DEFAULT TIMER

timerOffset: dw 0x0

timerSegment: dw 0x0

;; // FLOAT PING PONG

fd: dw 0x0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; // TERMINATE LOADER

boolTmnt: dw 0x0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCREEEN MAPS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; // LOAD LOADING SCREEN

ping: db '       ]]]]   ]  ]    ]   ]]]]]            ]]]]   ]]]]]]   ]    ]   ]]]]]       ', 
     db '      ]]]]]] ]] ]]]   ]] ]]]]]]]          ]]]]]] ]]]]]]]] ]]]   ]] ]]]]]]]      ',
     db '      ]]  ]] ]] ]]]]  ]] ]]]              ]]  ]] ]]]  ]]] ]]]]  ]] ]]]          ',
     db '      ]]  ]] ]] ]]]]] ]] ]]               ]]  ]] ]]    ]] ]]]]] ]] ]]           ',
     db '      ]]]]]] ]] ]] ]]]]] ]]  ]]]          ]]]]]] ]]    ]] ]] ]]]]] ]]  ]]]      ',
     db '      ]]]]]  ]] ]]  ]]]] ]] ]]]]          ]]]]]  ]]    ]] ]]  ]]]] ]] ]]]]      ',
     db '      ]]     ]] ]]   ]]] ]]   ]]          ]]     ]]    ]] ]]   ]]] ]]   ]]      ',
     db '      ]]     ]] ]]    ]] ]]]  ]]          ]]     ]]]  ]]] ]]    ]] ]]]  ]]      ',
     db '      ]]     ]] ]]    ]] ]]]]]]]          ]]     ]]]]]]]] ]]    ]] ]]]]]]]      ',
     db '       ]     ]   ]    ]   ]]]]]            ]      ]]]]]]   ]    ]   ]]]]]       '



;; // EXIT LOAD SCREEN

curtain: db '65000000000000000000000000000000000000000000000000000000000000000000000000000057', 
     db '21550000000000000000000000000000000000000000000000000000000000000000000000005512',
     db '22115500000000000000000000000000000000000000000000000000000000000000000000551122',
     db '32221155000000000000000000000000000000000000000000000000000000000000000055112223',
     db ' 322221155000000000000000000000000000000000000000000000000000000000000551122223 ',
     db '  3322221155000000000000000000000000000000000000000000000000000000005511222233  ',
     db '    332222115550000000000000000000000000000000000000000000000000055511222233    ',
     db '      33222211155500000000000000000000000000000000000000000000555111222233      ',
     db '        3322222111555000000000000000000000000000000000000005551112222233        ',
     db '          332222221115550000000000000000000000000000000055511122222233          ',
     db '            33322222211155550000000000000000000000005555111222222333            ',
     db '               33322222211115555000000000000000055551111222222333               ',
     db '                  33322222221111555500000000555511112222222333                  ',
     db '                     33322222222111155555555111122222222333                     ',
     db '                        33332222222211111111222222223333                        ',
     db '                            333322222222222222223333                            ',
     db '                                3333222222223333                                ',
     db '                                    33333333                                    ',
     db '                                                                                '
    

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CLEAR SCREEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

clrscr:
    pusha                   ; Save registers
    mov ax, 0xB800          ; Load video memory segment
    mov es, ax              ; Set extra segment to video memory
    xor di, di              ; Start at offset 0 in video memory
    mov cx, 2000            ; 80x25 = 2000 characters to clear
    mov al, 0x20            ; Space character (ASCII 32)
    mov ah, 0x07            ; Attribute byte (white text on black background)

clear_loop:
    stosw                   ; Store AX (character + attribute) and increment DI
    loop clear_loop         ; Repeat 2000 times

    popa                    ; Restore registers
    ret                     ; Return to caller

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DELAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

delay :
push bp
mov bp,sp
push cx
mov cx,[bp+4]
delay_loop1 :
push cx
mov cx, 0xFFFF
delay_loop2 :
	loop delay_loop2
	pop cx
	loop delay_loop1
	pop cx
    pop bp
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WRITE PING PONG SCREEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pong:
    push bp
    mov bp, sp

    pusha

    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax

    mov di, [bp+4]
    mov si, ping

    mov cx, 800
p1:
    mov ah, 0x46
    mov al, [si]

    cmp al,']'
    jnz pskip

    mov al,0xDB

    pskip:

    mov [es:di], ax

    add di, 2
    inc si

    loop p1

    mov [bp+4],di

    popa
   

    pop bp

    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOAD LOADING SCREEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

load_page:
    pusha

    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax

    xor di, di

    mov ax, 0x46b2

    mov cx, 80
    rep stosw

    mov ax, 0x46e8

    mov cx, 80
    rep stosw

    mov ax, 0x46b1

    mov cx, 80
    rep stosw

    mov ax, 0x46b0

    mov cx, 80
    rep stosw

    mov ax, 0x4600

    mov cx, 320
    rep stosw

    push di
    call pong
    pop di

    

    mov ax, 0x4600

    mov cx, 240
    rep stosw

    mov ax, 0x46b0

    mov cx, 80
    rep stosw

    mov ax, 0x46b1

    mov cx, 80
    rep stosw

    mov ax, 0x46e8

    mov cx, 80
    rep stosw

    mov ax, 0x46b2

    mov cx, 80
    rep stosw

    popa
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FLOAT PING PONG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

float:
cli

push bp
mov bp,sp

pusha

push cs
pop ds

mov ax,0xb800
mov es,ax
push es
pop ds

mov di,[bp+4]

mov ax,0x4600

cmp word[cs:fd],0x0
jz up

cmp word[cs:fd],0x1
jz down

up:
cmp di,960
jz setdown

mov dx,di
sub dx,160

mov si,di
sub di,160

cld


jmp fsink
setdown:

mov word[cs:fd],1

jmp fend


down:
cmp di,1440
jz setup

mov dx,di
add dx,160

mov si,di
add si,1598
mov di,si
add di,160

std

jmp fsink
setup:

mov word[cs:fd],0

jmp fend

fsink:

mov cx,800
rep movsw


mov di,dx

cmp word[cs:fd],0
jz clup

cmp word[cs:fd],1
jz cldown

clup:

add di,1600

jmp fein

cldown:

sub di,160

jmp fein


fein:
cld
mov cx,80
rep stosw

mov [bp+4],dx

fend:

popa
pop bp

sti

ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXIT LOAD SCREEN ARROW  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


exitLoad:

push bp

mov bp,sp

pusha

push cs
pop ds

mov ax,0xb800
mov es,ax

xor dx,dx
mov ax,[bp+4]

mov bx,160

mul bx

mov di,ax


mov si,curtain

mov cx,1520

elsc:

mov ax,word[es:di]

cmp byte[si],'0'
jnz ar1

mov ax,0x0720

ar1:
cmp byte[si],'1'
jnz ar2

mov ax,0x46b0

ar2:
cmp byte[si],'2'
jnz ar3

mov ax,0x46b1

ar3:
cmp byte[si],'3'
jnz ar4

mov ax,0x46b2

ar4:
cmp byte[si],'5'
jnz ar5

mov ax,0x0720

ar5:
cmp byte[si],'6'
jnz ar6

mov ax,0x4611


ar6:
cmp byte[si],'7'
jnz endar

mov ax,0x4610




endar:

mov word[es:di],ax

inc si

add di,2

loop elsc



popa

pop bp

ret 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CALCULATE DECIMAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DISPLAY PRINT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

print:

push bp
mov bp,sp

pusha

push cs
pop ds

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ABSOLUTE DELAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


absDelay :
push cx
mov cx, 3 ; change the values to increase delay time
absdelay_loop1 :
push cx
mov cx, 0xFFFF
absdelay_loop2 :
	loop absdelay_loop2
	pop cx
	loop absdelay_loop1
	pop cx
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECIMAL DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    STORE DEFAULT TIMER    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


storeTDefault:

pusha

push cs
pop ds

xor ax,ax
push ax
pop es

mov ax,[es:0x8*4]

mov word [timerOffset],ax

mov bx,[es:0x8*4+2]

mov word [timerSegment],bx


popa

ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    RESTORE DEFAULT TIMER    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


restoreTDefault:

pusha

push cs
pop ds

xor ax,ax
push ax
pop es

cli
mov ax, [timerOffset]
mov word[es:0x8*4], ax

mov bx, [timerSegment]
mov word[es:0x8*4+2], bx
sti

popa

ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;    LOADER PRESS HOOK     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; <HOOK>

LoaderPressHook:

pusha

push cs
pop ds

xor ax,ax
mov es,ax

cli

mov word [es:8*4],terminateEnter

mov word [es:8*4+2],cs

sti


popa
ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;     LOADING TERMINATE   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  <ISR>


terminateEnter:

pusha




push cs
pop ds



in al,0x60
cmp al,0x36

jnz teskip




mov word[boolTmnt],0x1



teskip:


mov al,0x20
out 0x20,al

popa

iret




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;          [MAIN]          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:

    call storeTDefault

   

    call load_page


    call LoaderPressHook


    mov di,1280

    f1:
 
    mov cx,12
    push cx
    call delay

    cli
    push di
    call float
    pop di

    sti

    mov bx,0xfa0
    push bx
    push word[boolTmnt]
    call print

    cmp word[boolTmnt],1
    jz efi

    
    

    jmp f1

    efi:

        mov di,-18

 a1:
 
    mov cx,6
    push cx
    call delay

    push di
    call exitLoad
    
    inc di


    cmp di,25
    jnz a1

    call restoreTDefault

    jmp efi

    mov ax, 0x4c00
    int 0x21
