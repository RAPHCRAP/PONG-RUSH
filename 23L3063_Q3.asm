[ORG 0x0100]
jmp start

msg db 'PROGRAM IS NOW RESIDENT. PRESS B TO SAVE/CLEAR SCREEN...$'
buffer times 4000 db 0
keyState db 0

start:
    mov ah, 0x09
    mov dx, msg
    int 0x21

    mov ax, 0x3509
    int 0x21
    mov [oldHandler], bx
    mov [oldHandler + 2], es

    cli
    mov ax, 0x2509
    mov dx, kbisr
    push cs
    pop ds
    int 0x21
    sti

    mov dx, (programEnd - start + 0x0100 + 15) >> 4
    mov ax, 0x3100
    int 0x21

kbisr:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    in al, 0x60
    cmp al, 0x30
    je checkAndSave
    cmp al, 0xB0
    je restoreScreen

    pop ds
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    jmp far [cs:oldHandler]

checkAndSave:
    push cs
    pop ds
    cmp byte [keyState], 1
    je skipSave
    mov byte [keyState], 1
    call saveScreen
    mov ax, 0x0003
    int 0x10

skipSave:
    mov al, 0x20
    out 0x20, al

    pop ds
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    iret

restoreScreen:
    push cs
    pop ds
    mov byte [keyState], 0
    call restoreScreen
    mov al, 0x20
    out 0x20, al

    pop ds
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    iret

saveScreen:
    pusha
    mov cx, 4000
    mov ax, 0xB800
    mov ds, ax
    push cs
    pop es
    mov si, 0
    mov di, buffer
    cld
    rep movsb
    popa
    ret

restoreScreen:
    pusha
    mov cx, 4000
    mov ax, 0xB800
    mov es, ax
    push cs
    pop ds
    mov si, buffer
    mov di, 0
    cld
    rep movsb
    popa
    ret

oldHandler dd 0
programEnd:
