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
    cmp byte [bConBuf], 0   ;Does the buffer contain a zero?
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
    mov byte [bConBuf], ah  ;Save scancode
.cre12:
    inc ecx ;Inc chars stored in buffer
    jmp short .cre1
.cre2:
    mov dword [r8 + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    return
.cre3:
    mov al, byte [bConBuf]  ;Get the buffer value
    mov byte [bConBuf], 0   ;Reset the buffer value
    jmp short .cre11

ndRead:  ;Non destructive read chars
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], ndInNoWaitPkt_size
    jne errorExit
    cmp byte [bConBuf], 0
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
    mov al, byte [bConBuf]  ;Copy scancode but dont reset it
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
    mov byte [bConBuf], 0   ;Clear buffer
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
;Only one function, MScrCap. Declares the presence of a multitasker
; to the driver!
;Do all checks to ensure not an accidental call.
    mov rdx, qword [r8 + ioctlReqPkt.ctlptr]
    cmp ch, 03h     ;Sent to the CON? (Formally, SCR$)?
    jne .exitBad
    cmp cl, 40h     ;Get Multitasking Screen Capacities/init multitaskting?
    je .iosc_init
    cmp cl, 41h     ;Locate SIB
    je .iosc_ls
    cmp cl, 42h     ;Save Segment
    je .iosc_ss
    cmp cl, 43h     ;Restore Segment
    je .iosc_rs
    cmp cl, 44h     ;Enable IO
    je .iosc_ei
    cmp cl, 45h     ;Initialise Screen
    je .iosc_is    
    cmp cl, 46h     ;Deactivate Multitasking capabilities
    je .iosc_deinst
.exitBad:
    mov word [r8 + drvReqHdr.status], drvErrStatus | drvBadCmd
    stc
    return
.iosc_deinst:
;Reset the internal vars to set CON back to single tasking mode!
    cli
    mov rdx, qword [pOldKbdIntr]
    mov eax, 0F1h
    call installInterrupt
    mov rdx, qword [pOldKbdHdlr]
    mov eax, 036h
    call installInterrupt
    lea rax, noOp
    mov qword [pDevHlp], rax    ;Restore the do nothing function!
    sti
    return
.iosc_ls:
;Locate SIB
.iosc_ss:
;Save Segment
.iosc_rs:
;Restore Segment
.iosc_ei:
;Renable IO
.iosc_is:
;Initialise screen
    return
.iosc_init:
    cmp word [rdx + mScrCap.wVer], 0100h
    jne .exitBad
    cmp word [rdx + mScrCap.wLen], mScrCap_size
    jne .exitBad
;Here we have verified we are ok! Proceed.
;This IOCTL is equivalent to in MTDOS the CON reporting caps during driver init.
    mov byte [rdx + mScrCap.bScrNum], (maxScr + 1)
    lea rax, mConHlp
    xchg rax, qword [rdx + mScrCap.pDevHlp] ;For now, still provide this iface
    ;mov rax, qword [rdx + mScrCap.pDevHlp]  ;Get the devHlp pointer
    mov qword [pDevHlp], rax

;Now setup the pointer to the DOSMGR ScrIoOk byte
    xor eax, eax    ;Get var 0 (ScrIoOk)
    mov ecx, 1      ;Length 1
    mov edx, DevHlp_GetDOSVar     ;Get ScrIoOk Var
    call qword [pDevHlp]
    mov qword [pScrIoOk], rax   ;Store the pointer now!

;Now install the multitasking keyboard interrupt routines!
;DO IRQ1
    mov eax, 0F1h   ;Get IRQ1 handler in rbx
    call getIntHdlr
    mov qword [pOldKbdIntr], rbx
    lea rdx, keybIntr
    mov eax, 0F1h
    call installInterrupt
;DO Int 36h
    mov eax, 036h   ;Get Int 36h handler in rbx
    call getIntHdlr
    mov qword [pOldKbdHdlr], rbx
    lea rdx, keybHdlr
    mov eax, 036h
    call installInterrupt

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
    jz genIOCTL.iosc_deinst ;Cancel help pointer support
    stc
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
;CON Int 3Bh handler to detect CTRL+BREAK.
    push rax
    push rdx
;Simulate an interrupt call to the BIOS to pull the key out
; from the buffer.
    mov rdx, rsp
    xor eax, eax
    mov ax, ss
    push rax
    push rdx
    pushfq
    mov ax, cs
    push rax
    xor eax, eax    ;Getch, BIOS Places a zero word in the keyboard buffer
    call qword [pOldKbdHdlr]    ;Pull the zero word out of the keyb buffer
    mov eax, 03h                ;Replace it with a ^C char
    mov edx, DevHlp_ConsInputFilter
    call qword [pDevHlp]        ;Ask DOS if it wants to eat the ^C
    jz .exit                    ;Jump if DOS ate ^C :(
    mov byte [bConBuf], 03h     ;Place a ^C in buffer
.exit:
    pop rdx
    pop rax
    iretq

keybIntr:        ;New Keyboard Interrupt Hdlr
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
    call qword [pOldKbdIntr]    ;Do the SCP/BIOS kbd handler code!

    mov rdx, rsp
    xor eax, eax
    mov ax, ss
    push rax
    push rdx
    pushfq
    mov ax, cs
    push rax
    mov eax, 0100h              ;Now read ahead, under our handler
    call qword [pOldKbdHdlr]    ;Gets the SC/ASCII pair in ax
    mov edx, DevHlp_ConsInputFilter
    call qword [pDevHlp]
    jnz .keepChar
    ;Else remove the char from the buffer
    mov rdx, rsp
    xor eax, eax
    mov ax, ss
    push rax
    push rdx
    pushfq
    mov ax, cs
    push rax
    xor eax, eax
    call qword [pOldKbdHdlr]    ;Gets the SC/ASCII pair in ax
    jmp short .exit
.keepChar:
    cli
    test byte [bKeybWait], -1  
    jz .exit
    push rbx
    push rcx
    lea rbx, bKeybWait  ;Run all processes with this identifier
    mov byte [rbx], 0   ;Clear the flag first
    mov edx, DevHlp_ProcRun  ;ProcRun
    call qword [pDevHlp]
    pop rcx
    pop rbx
.exit:
    pop rdx
    pop rax
    iretq


keybHdlr:   ;Int 36h
    test ah, ah
    je .readChar
    cmp ah, 1
    je .lookahead
    jmp qword [pOldKbdHdlr]
.readChar:
    push rax    ;Push original function number on the stack
    push rbx
    push rcx
    push rdx
.readChLp:
    mov rbx, qword [pScrIoOk]
    test byte [rbx], -1
    jnz .okToRead
    xor ecx, ecx
    mov edx, DevHlp_ProcBlock  ;Proc block, non-interruptable
    call qword [pDevHlp]    ;Use this var as identifier.
    jmp short .readChLp ;Check again!
.okToRead:
;Now we simulate a call into 36h/AH=01h - Get keyboard buffer status
    cli
    mov rdx, rsp
    xor eax, eax
    mov ax, ss
    push rax
    push rdx
    pushfq
    mov ax, cs
    push rax
    mov eax, 0100h  ;Get the keyb status
    call qword [pOldKbdHdlr]    ;Interrupts remain set on return
    jnz .doCharRead
;If the char isn't there, we gotta pblock until it is.
    lea rbx, bKeybWait
    mov byte [rbx], -1
    or ecx, ecx
    mov edx, DevHlp_ProcBlock  ;Proc block, non-interruptable
    call qword [pDevHlp]    ;Use this var as identifier.
    jmp short .readChLp ;Check again with CLI set!
.doCharRead:
    pop rdx
    pop rcx
    pop rbx
    pop rax
.goKbd:
    jmp qword [pOldKbdHdlr]
.lookahead:
    push rbx
    mov rbx, qword [pScrIoOk]   ;Can we check?
    test byte [rbx], -1
    pop rbx
    jnz .goKbd  ;If we can, do it!
    and byte [rsp + 2*8], ~1    ;Clear CF of flags
    iretq

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

    mov eax, 5100h
    int 21h             ;Get current PSP ptr
    cmp rbx, 9          ;If we are being used as a Kernel driver, no msg!
    je skipMsg
;Else, print message!
    lea rdx, helloStr   ;Print install string
    mov eax, 0900h
    int 21h
skipMsg:
    lea rax, init   ;Eject init
    mov qword [r8 + initReqPkt.endptr], rax
    return

helloStr    db  "--- Installing MCON Device Driver V"
            db  majVers+"0",".",minVers/10+"0"
            db (minVers-minVers/10*10)+"0", " ---", 10,13,"$"
