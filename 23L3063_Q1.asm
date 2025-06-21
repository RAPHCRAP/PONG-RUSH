[ORG 0x0100]
jmp start

promptMsg: db : 'PROVIDE NAME : $'
successMsg: db : 'PRESS LEFT SHIFT $'
nameBuf: times 51 db 0
nameLen: db 0
prevInt9Offset: dw 0
prevInt9Segment: dw 0
residentFlag: db 0

start:
    cmp byte [residentFlag], 1
    je alreadyLoaded

    mov ah, 0x09
    mov dx, promptMsg
    int 0x21

    xor cx, cx
    mov di, nameBuf

getNameChar:
    mov ah, 0x01
    int 0x21
    cmp al, 0x0D
    je endInput
    mov [di], al
    inc di
    inc cx
    cmp cx, 50
    jb getNameChar

endInput:
    mov byte [di], 0
    mov [nameLen], cl

    mov ax, 0x3509
    int 0x21
    mov [prevInt9Offset], bx
    mov [prevInt9Segment], es

    mov ax, 0x2509
    mov dx, keyHandler
    int 0x21

    mov ah, 0x09
    mov dx, successMsg
    int 0x21

    mov byte [residentFlag], 1
    mov dx, progEnd
    add dx, 15
    shr dx, 4
    mov ax, 0x3100
    int 0x21

alreadyLoaded:
    mov ax, 0x4C00
    int 0x21

keyHandler:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push ds
    push es

    in al, 0x60
    cmp al, 0x2A
    je showName
    cmp al, 0xAA
    je clearScreen

    pushf
    call far [cs:prevInt9Offset]
    jmp handlerExit

showName:
    mov ax, 0xB800
    mov es, ax
    mov ax, cs
    mov ds, ax
    xor di, di
    mov si, nameBuf
    mov cl, [nameLen]
    xor ch, ch

printNameLoop:
    lodsb
    mov ah, 0x57
    stosw
    loop printNameLoop

    mov al, 0x20
    out 0x20, al
    jmp handlerExit

clearScreen:
    mov ax, 0x0003
    int 0x10
    mov al, 0x20
    out 0x20, al

handlerExit:
    pop es
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    iret

progEnd:
