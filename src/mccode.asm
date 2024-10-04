;This driver is designed to work in the MDOS environment. 

strategy:
;DOS calls this function with rbx=Ptr to request header.
;Rather than saving the 
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    push r8 ;Use r8 as the static pointer to the request packet!
    mov r8, rbx 
    mov word [r8 + drvReqHdr.status], 0    ;Ensure status clear (should be!)
    movzx eax, byte [r8 + drvReqHdr.cmdcde]
    cmp eax, funcTblE   
    jae short unkExit       ;If cmdcde is past the end of the table, error!
    lea rbx, funcTbl        ;Else get pointer to function
    movzx edx, word [rbx + 2*rax]   
    add rbx, rdx
    call rbx        
exit:
    or word [r8 + drvReqHdr.status], drvDonStatus    ;Set done bit!
.err:
    pop r8
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
noOp:
    return

unkExit:
    mov al, drvBadCmd
errorExit:
;Jump to with al=Standard Error code
    mov ax, drvErrStatus    ;Set error bit, and NOT done bit!
    mov word [r8 + drvReqHdr.status], ax
    jmp short exit.err

read:    ;Read Chars
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], ioReqPkt_size
    jne errorExit

    mov rdi, qword [r8 + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.cre1:
    cmp ecx, dword [r8 + ioReqPkt.tfrlen]
    je .cre2
    cmp byte [conBuf], 0   ;Does the buffer contain a zero?
    jnz .cre3   ;No, get the buffer value
    xor eax, eax
    int 36h
    cmp ax, 7200h   ;CTRL + PrnScr? 
    jne .cre11
    mov al, 10h     ;Store ^P in al!
.cre11:
    stosb
    test al, al ;Was the ascii code stored 0?
    jnz .cre12  ;No, skip storing scancode in buffer
    mov byte [conBuf], ah  ;Save scancode
.cre12:
    inc ecx ;Inc chars stored in buffer
    jmp short .cre1
.cre2:
    mov dword [r8 + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    return
.cre3:
    mov al, byte [conBuf]  ;Get the buffer value
    mov byte [conBuf], 0   ;Reset the buffer value
    jmp short .cre11

ndRead:  ;Non destructive read chars
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], ndInNoWaitPkt_size
    jne errorExit
    cmp byte [conBuf], 0
    jnz .cnr2
    mov ah, 01h     ;Get key if exists
    int 36h
    jz .cnr1        ;If zero clear => no key, go forwards
    ;Keystroke available
    cmp ax, 7200h   ;CTRL + PrnScr?
    jne .cnr0
    mov al, 10h     ;Report ^P
.cnr0:
    mov byte [r8 + ndInNoWaitPkt.retbyt], al   ;Move char in al
    return
.cnr1: ;No keystroke available
    mov word [r8 + ndInNoWaitPkt.status], drvBsyStatus   ;Set busy bit
    return
.cnr2:
    mov al, byte [conBuf]  ;Copy scancode but dont reset it
    jmp short .cnr0   ;Keystroke is available clearly

inStatus:         ;Get Input Status
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], statusReqPkt_size
    jne errorExit
    return ;Exit, device ready

flushInBuf:   ;Flush Input Buffers
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], statusReqPkt_size
    jne errorExit
    mov byte [conBuf], 0   ;Clear buffer
.cfib0:
    mov ah, 01      ;Get buffer status
    int 36h
    retz            ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short .cfib0

write:   ;Write Chars
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], ioReqPkt_size
    jne errorExit

    mov rsi, qword [r8 + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
.cw1: 
    cmp ecx, dword [r8 + ioReqPkt.tfrlen]
    je .cw2
    lodsb   ;Get char into al, and inc rsi
    int 29h ;Fast print char
    inc ecx
    jmp short .cw1 ;keep printing until all chars printed
.cw2:
    mov dword [r8 + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    return
; -------------------- NEW FUNCTIONS -------------------- 
genIOCTL:
;Only one function, MScrCap.
;Do all checks to ensure not an accidental call.
    mov rsi, qword [r8 + ioctlReqPkt.rsival]
    mov rdi, qword [r8 + ioctlReqPkt.rdival]
    mov rdx, qword [r8 + ioctlReqPkt.ctlptr]
    cmp cx, 0310h   ;Get MScrCap?
    jne .exitBad
    test rsi, rsi
    jnz .exitBad
    test rdi, rdi
    jnz .exitBad
    cmp word [rdx + mScrCap.wVer], 0100h
    jne .exitBad
    cmp word [rdx + mScrCap.wLen], mScrCap_size
    jne .exitBad
    ;Here we have verified we are ok! Proceed.
    ;This IOCTL is equivalent to in MTDOS the CON reporting caps during driver init.
    movzx eax, word [rdx + mScrCap.wMagic]
    mov word [magicKey], ax
    mov byte [rdx + mScrCap.bScrNum], (maxScr + 1)
    lea rax, mConHlp
    xchg qword [rdx + mScrCap.qHlpPtr], rax  ;Swap pointers
    mov qword [hlpPtr], rax   ;Store this as the help pointer
    return
.exitBad:
    mov word [r8 + drvReqHdr.status], drvErrStatus | drvBadCmd
    stc
    return
;-------------------------------------------
;       mConHlp dispatch and routines
;-------------------------------------------

mConHlp:
;AL = 0: Get current screen number
;AL = 1: Set new screen number
;AL = 2: Reset the screen (CLS)
;Al = 3: Cancel help pointer support
    test eax, eax
    jz getScreen
    dec eax
    jz swapScreen
    dec eax
    jz resetScreen
    dec eax
    jz delHlp
    stc
    return
delHlp:
;If the Session Manager has to de-install itself, call this 
; to indicate that the hlpPtr is no longer valid!
    lea rax, noOp
    mov qword [hlpPtr], rax
    return
getScreen:
    push rbx
    mov eax, 0F00h
    int 30h
    movzx eax, bh
    pop rbx
    return
swapScreen:
;If the screen number is above the max screen number we return error!
;This routine is signalled by the task swapping routine.
    movzx eax, bl ;Get the screen number
    cmp eax, maxScr
    ja .err
    or eax, 0500h   
    int 30h         ;Swap active page on the VGA!
    return
.err:
    stc
    return
resetScreen:
;Resets the currently active screen!
    mov ah, 0Bh  ; Set overscan to black (when Graphics becomes supported)
    xor ebx, ebx
    int 30h
    mov ah, 0Fh ;Get screen mode
    int 30h
    push rbx    ;Save the screen number on stack
    movzx edx, ah   ;Get number of columns in dl
    dec dl
    mov dh, 25  ;Number of rows is standard
    xor eax, eax
    mov ecx, eax
    mov bh, 7   ;Screen attributes
    mov ah, 6   ;Scroll
    int 30h
    xor edx, edx    ;Set cursor coordinates to top left of screen
    pop rbx     ;Get back the screen number
    mov ah, 2
    int 30h
    return
    
;------------- Driver built-in Interrupt Routines -------------
fastOutput:         ;This CON driver supports Int 29h
;Called with char to transfer in al
    push rax
    mov ah, 0Eh
    int 30h
    pop rax
    iretq
ctrlBreak:
;CON Int 3Bh handler to detect CTRL+BREAK
    mov byte [conBuf], 03h    ;Place a ^C in buffer
    iretq

newKeybIntr:        ;New Keyboard Interrupt Hdlr
    push rax        ;Save RAX as a trashed reg
    push rdx
;Simulate an IRQ entry to old kbdHldr
    mov rdx, rsp
    xor eax, eax
    mov ax, ss
    push rax
    push rdx
    pushfq
    mov ax, cs
    push rax
    call qword [oldKbdHdlr]

    test byte [inHdlr], -1      ;If set, we are reentering this. Exit!!
    jnz .exit
    cmp word [magicKey], -1     ;If no magic key to check, exit!
    je .exit
    mov eax, 0100h  ;Now we NDlook to see what was placed in.
    int 36h         
    jz .exit        ;If ZF=ZE, no keystroke available (should never happen)
    cmp ax, word [magicKey] ;Did we receive the magic key
    jne .exit
;Else, we now pull the magic key and signal SM!
    mov byte [inHdlr], -1   ;Set reentrancy flag!
;Pull the magic char out of the buffer!
    xor eax, eax
    int 36h
;Now pass this information to SM
    mov edx, eax    ;Pass sc/ASCII pair in edx
    mov eax, 1      ;Call function 1
    cli ;Pause interrupts
    mov byte [inHdlr], 0    ;Clear reentrancy flag!
    call qword [hlpPtr]     ;Call SM
    sti
.exit:
    pop rdx
    pop rax
    iretq


;------------------ EJECT POINT ------------------

init:
;Start by hooking int 3Bh, int 29h and 0F1h (IRQ1) as part of the CON driver
;DO FASTOUT
    lea rdx, fastOutput
    mov eax, 29h
    call installInterrupt
;DO CTRL+BREAK
    lea rdx, ctrlBreak
    mov eax, 3Bh
    call installInterrupt
;DO IRQ1
    mov eax, 0F1h   ;Get IRQ1 handler in rbx
    call getIntHdlr
    mov qword [oldKbdHdlr], rbx
    lea rdx, newKeybIntr
    mov eax, 0F1h
    call installInterrupt

.ci0:
    mov ah, 01      ;Get buffer status
    int 36h
    jz .ci1      ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short .ci0
.ci1:
    mov eax, 0500h  ;Set page zero as the default page
    int 30h
    mov ah, 02h
    xor edx, edx    ;Set screen cursor to top right corner
    mov bh, dl      ;Set cursor for page 0
    int 30h
    mov bh, 07h     ;Grey/Black attribs
    mov eax, 0600h  ;Clear whole screen
    int 30h

    lea rax, init   ;Eject init
    mov qword [r8 + initReqPkt.endptr], rax
    return

installInterrupt:
;Writes the interrupt in the right place in the table
    ;al = Interrupt number
    ;rdx -> Handler to install
    sidt [myIdt]
    movzx eax, al
    xchg rdx, rax
    shl rdx, 4h     ;Multiply IDT entry number by 16
    add rdx, qword [myIdt.base]    
    mov word [rdx], ax  ;Get low word into offset 15...0
    shr rax, 10h    ;Bring next word low
    mov word [rdx + 6], ax  ;Get low word into offset 31...16
    shr rax, 10h    ;Bring last dword low
    mov dword [rdx + 8], eax
    ret
getIntHdlr:
;Gets an interrupt value. 
;Input: al = Interrupt number
;Output: rbx = Interrupt Vector
    push rax
    mov eax, 1202h  ;Get Interrupt handler in rbx
    int 2Fh
    pop rax
    return
    ;sidt [myIdt]
    ;movzx eax, al
    ;shl rax, 4h     ;Multiply IDT entry number by 16 (Size of IDT entry)
    ;add rax, qword [myIdt.base]    
    ;xor ebx, ebx
    ;mov ebx, dword [rax + 8]    ;Get bits 63...32
    ;shl rbx, 10h    ;Push the high dword high
    ;mov bx, word [rax + 6]      ;Get bits 31...16
    ;shl rbx, 10h    ;Push word 2 into posiiton
    ;mov bx, word [rax]          ;Get bits 15...0
    ;return
myIdt:
.limit  dw 0
.base   dq 0