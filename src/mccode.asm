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
    jae short unkCmd       ;If cmdcde is past the end of the table, error!
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

unkCmd:
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
    cmp dword [r8 + ioReqPkt.tfrlen], 0
    je .exit
    movzx edx, byte [r8 + ioReqPkt.strtsc]  ;Get the screen number
    cmp edx, maxScr
    jbe .okScrnNum
    mov eax, drvReadFault
    jmp short errorExit
.okScrnNum:
    mov rdi, qword [r8 + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.readLp:
    cmp dl, byte [bCurScr]
    je .getch   ;If the current screen is not the one requesting, freeze!
    call getSIB ;Get the ptr to the SIB for the screen number in dl.
    call procBlock  ;Lock using this SIB ptr as the identifier.
    jmp short .readLp
.getch:
;    cli
    cmp byte [bConBuf], 0   ;Does the buffer contain a zero?
    jnz .getScCde   ;No, get the buffer value

;Do a simulated 36h/00h call!
    call simulBIOSRead

    ;xor eax, eax
    ;int 36h
    test ax, ax ;If we read a null, read again! Go through screen check though!
    jz .readLp
    cmp ax, 7200h   ;CTRL + PrnScr? 
    jne .savChr
    mov al, 10h     ;Store ^P in al!
.savChr:
    stosb
    test al, al ;Was the ascii code stored 0?
    jnz .savScCde  ;No, skip storing scancode in buffer
    mov byte [bConBuf], ah  ;Save scancode
.savScCde:
;    sti
    inc ecx ;Inc chars stored in buffer
    cmp ecx, dword [r8 + ioReqPkt.tfrlen]
    jne .readLp
.exit:
    mov dword [r8 + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    return
.getScCde:
    mov al, byte [bConBuf]  ;Get the buffer value
    mov byte [bConBuf], 0   ;Reset the buffer value
    jmp short .savChr

ndRead:  ;Non destructive read chars
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], ndInNoWaitPkt_size
    jne errorExit
    movzx edx, byte [r8 + ioReqPkt.strtsc]  ;Get the screen number
    cmp edx, maxScr
    jbe .okScrnNum
    mov eax, drvReadFault
    jmp errorExit
.okScrnNum:
    cmp dl, byte [bCurScr]  ;If not current screen, no char available!
    jne .noChar
    mov al, byte [bConBuf]
    test al, al ;If this is not 0, there is a char in the buffer!
    jnz .charFnd
    call simulBIOSNDRead
    jz .noChar          ;If zero clear => no key in buffer
    ;Else, Keystroke available
    test ax, ax         ;If this is null, pull from the buffer
    jnz .notNul         
    cmp dl, byte [bCurScr]  ;If no longer on current screen, no char available!
    jne .noChar
    call simulBIOSRead
    jmp short ndRead    ;Now go again...
.notNul:
    cmp ax, 7200h   ;CTRL + PrnScr?
    jne .charFnd
    mov al, 10h     ;Report ^P
.charFnd:
    mov byte [r8 + ndInNoWaitPkt.retbyt], al   ;Move char in al
    return
.noChar: ;No keystroke available
    mov word [r8 + ndInNoWaitPkt.status], drvBsyStatus   ;Set busy bit
    return

inStatus:         ;Get Input Status
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], statusReqPkt_size
    jne errorExit
    return ;Exit, device ready

flushInBuf:   ;Flush Input Buffers
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], statusReqPkt_size
    jne errorExit
    movzx edx, byte [r8 + ioReqPkt.strtsc]  ;Get the screen number
    cmp edx, maxScr
    jbe .okScrnNum
    mov eax, drvReadFault
    jmp errorExit
.okScrnNum:
    cmp dl, byte [bCurScr]
    je .cleanBuf   ;If the current screen is not the one requesting, freeze!
.block:
    call getSIB ;Get the ptr to the SIB for the screen number in dl.
    call procBlock  ;Lock using this SIB ptr as the identifier.
    jmp short .okScrnNum
.cleanBuf:
    mov byte [bConBuf], 0   ;Clear buffer
    call simulBIOSNDRead
    retz            ;If zero clear => no more keys to read
    cmp dl, byte [bCurScr]  ;If screen has changed, block on SIB!
    jne .block
    call simulBIOSRead  ;Char pulled, check if screen number still ok!
    jmp short .okScrnNum    

write:   ;Write Chars
    mov al, 05h ;Bad request structure length?
    cmp byte [r8 + drvReqHdr.hdrlen], ioReqPkt_size
    jne errorExit
    cmp dword [r8 + ioReqPkt.tfrlen], 0
    je .exit
    movzx edx, byte [r8 + ioReqPkt.strtsc]  ;Get the screen number
    cmp edx, maxScr
    jbe .okScrnNum
    mov eax, drvWriteFault
    jmp errorExit
.okScrnNum:
    mov rsi, qword [r8 + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
.writeLp:
    cmp dl, byte [bCurScr]
    je .chkScrn ;If the current screen is not the one requesting, freeze!
    call getSIB ;Get the ptr to the SIB for the screen number in dl.
.block:
    call procBlock  ;Lock using this SIB ptr as the identifier.
    jmp short .writeLp
.chkScrn:   ;Check if the screen is frozen!
    mov rbx, qword [pCurSib]
    test byte [rbx + sib.bFrozenFlg], -1
    jnz .block  ;Cant output if this flag is 0! Block on the SIB ptr.
    test byte [bSavScr], -1 ;If set, we are in the middle of a save!
    jz .outch
    lea rbx, bSavScr    ;Block on the bSavScr byte
    jmp short .block
.outch: 
    lodsb   ;Get char into al, and inc rsi
    call outch ;Fast print char
    inc ecx
    cmp ecx, dword [r8 + ioReqPkt.tfrlen]
    jne .writeLp  ;keep printing until all chars printed
.exit:
    mov dword [r8 + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    return
outch:
;Prints the char passed in al, with 
    push rax
    push rbx
    mov ah, 0Eh
    int 30h
    pop rbx
    pop rax
    return
; -------------------- NEW IOCTL FUNCTIONS -------------------- 
ioctl:
    cmp ch, 03h     ;Is this a CON IOCTL request?
    jne unkCmd
    mov rsi, qword [r8 + ioctlReqPkt.rsival]
    cmp cl, 40h
    jb unkCmd
    cmp cl, 49h
    jae unkCmd
    movzx ecx, cl
    sub ecx, 40h     ;Get table offset
    lea rbx, ioctlTbl
    mov rcx, qword [rbx + 2*rcx]    ;Get the offset from ioctlTbl
    add rbx, rcx    ;Add to the table base address
    jmp rbx ;Jump to the ptr in rbx and return to the main dispatcher!

;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
; NOTE FOR FUTURE! WE WILL NEED TO SAVE SOME INFORMATION IN THE DATA AREA |
; EVEN IF USING BIOS SINCE WE NEED TO ALLOW ALL SCREENS TO HAVE SEPARATE  |
; CURSOR SHAPES! AS THINGS STAND, ALL SCREENS SHARE THE SAME CURSOR SHAPE.|
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

ioctl_ls:
;Locate SIB. 
;By locating the SIB, we are selecting the screen!
    cmp rsi, maxScr 
    ja badSIBNum
    mov edx, esi    ;Get the screen number in al
    cmp byte [bCurScr], dl
    je .curScr
    mov byte [bCurScr], dl
    call getSIB     ;Get sib ptr in rbx for screen number in dl.
    mov qword [pCurSib], rbx
    jmp short .exit
.curScr:
;Here we get a pointer to the segment itself, not just the SIB
    mov byte [bSavScr], -1
    xor eax, eax    ;Segment to get (Segment 0, only segment!)
    call getSibSeg  ;Get rbx -> SIB, rdi -> Segment     
    xor eax, eax
    mov word [rdi + sibSeg.wSegSize], ax    ;We don't need a segment!
.exit:
    mov qword [r8 + ioctlReqPkt.ctlptr], rbx    ;Return SIB ptr here!
    mov qword [r8 + ioctlReqPkt.rsival], 0  ;Indicate success!
    return

ioctl_ss:
;Save Segment of current SIB.
;Input: rsi(sil) = ZX segment number to operate on into current SIB.
    cmp rsi, maxSibSeg  ;Is this equal to 1?
    ja badSIBNum
;Since we are saving NO data, we simply return success!
    mov qword [r8 + ioctlReqPkt.rsival], 0  ;Indicate success!
    return

ioctl_rs:
;Restore Segment (Actually, set segment data!)
;Input: rsi(sil) = ZX segment number to operate on into current SIB.
;
;We don't per-se restore data from the SIB instead getting BIOS to do
; the screen swap for us. We change the screen number to the current 
; screen number. 
;If we send a SIB segment number of 0, we are resetting the screen.
;Else, we are simply swapping to the screen.
    cmp rsi, maxSibSeg  ;Is this equal to 1?
    ja badSIBNum

;Start by swapping the screen to the SIB number!
    mov al, byte [bCurScr]    ;Move the screen number into al
    mov ah, 05h     ;Set active page to al 
    int 30h         ;Swap active page on the VGA!

    test esi, esi   ;If Segment 0, reset the screen totally!
    jnz .exit       ;Else we just wanted to swap to this screen (segment bzw.)
;Here we reset the screen!
    call resetScreen
.exit:
    mov qword [r8 + ioctlReqPkt.rsival], 0  ;Indicate success!
    return

ioctl_ei:
;Renable IO
;Input: rsi(sil) = Zero extended screen number to operate on.
    mov byte [bSavScr], 0   ;We are done saving the screen!
    lea rbx, bSavScr    ;Thaw threads frozen during screen save!
    call procRun
    mov rbx, qword [pCurSib]    ;Thaw threads frozen while CurSib not current
    return

ioctl_is:
;Initialise screen
;Input: rsi(sil) = Zero extended screen number to operate on.
    cmp rsi, maxScr 
    ja badSIBNum
    mov edx, esi
    mov byte [bCurScr], dl
    call getSIB ;Get ptr to this sib in rdi
    mov qword [pCurSib], rdi
    mov byte [rdi + sib.bFrozenFlg], 0  ;Screen not frozen!
;Reset the screen now
    call resetScreen
    mov qword [r8 + ioctlReqPkt.rsival], 0  ;Indicate success!
    return

ioctl_strt:
;Start (continue) screen output
;Input: rsi(sil) = Zero extended screen number to operate on.
    mov rbx, qword [pCurSib]
    test byte [rbx + sib.bFrozenFlg], -1 ;Set the freeze flag in SIB
    retz    ;If the current screen is already thawed, just return!
    mov byte [rbx + sib.bFrozenFlg], 0  ;Thaw this SIB now
    call procRun    ;And thaw any threads blocked on this SIB!
    return

ioctl_stop:
;Stop (freeze) current screen output
;Input: rsi(sil) = Zero extended screen number to operate on.
    mov rbx, qword [pCurSib]
    mov byte [rbx + sib.bFrozenFlg], -1 ;Set the freeze flag in SIB
    return

ioctl_init:
;Multitasking initialisation function. Passes a datapkt ptr
; in ctlptr field.
    test byte [bInMulti], -1 ;Have we already run this function?
    je unkCmd
    mov rdx, qword [r8 + ioctlReqPkt.ctlptr]    ;Get pktptr
    cmp word [rdx + mScrCap.wVer], 0100h
    jne unkCmd
    cmp word [rdx + mScrCap.wLen], mScrCap_size
    jne unkCmd
;Here we have verified we are ok! Proceed.
    mov byte [bInMulti], -1     ;Now we are entering MT! Set the lock!

;This IOCTL is equivalent to how in MTDOS, CON reports caps during drvinit.
    mov byte [rdx + mScrCap.bScrNum], (maxScr + 1)
    mov rax, qword [rdx + mScrCap.pDevHlp]  ;Get the devHlp pointer
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

ioctl_deinst:
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
    mov byte [bInMulti], 0  ;Back out of Multitasking
    return

badSIBNum:
;Jumped to to indicate a bad SIB or segment number!
    mov qword [r8 + ioctlReqPkt.rsival], 1 
    return

procRun:
;Input: rbx = qword identifier for the block tasks to unblock.
    push rbx
    push rdx
    mov edx, DevHlp_ProcRun
    call qword [pDevHlp]
    pop rdx
    pop rbx
    return

procBlock:
;Input: rbx = qword identifier to block the thread on.
    push rbx
    push rcx
    push rdx
    mov edx, DevHlp_ProcBlock   ;ProcBlock, Sleep is not interruptable
    xor ecx, ecx    ;No timeout!
    cli         ;Stop Interrupts to prevent race conditions
    call qword [pDevHlp]
    pop rdx
    pop rcx
    pop rbx
    return

simulBIOSRead:
;Simulates a BIOS call of 36h/00h bypassing any hooks.
    push rax
    push rbx
    xor eax, eax
    mov ax, ss
    mov rbx, rsp
    push rax    ;Push SS
    push rbx    ;Push RSP
    pushfq      ;Push RFLAGS
    mov ax, cs
    push rax    ;Push CS
    xor eax, eax  ;Get the keyb status
    jmp short simulBIOS
simulBIOSNDRead:
;Simulates a BIOS call of 36h/01h bypassing any hooks.
    push rax
    push rbx
    xor eax, eax
    mov ax, ss
    mov rbx, rsp
    push rax    ;Push SS
    push rbx    ;Push RSP
    pushfq      ;Push RFLAGS
    mov ax, cs
    push rax    ;Push CS
    mov eax, 0100h  ;Get the keyb status
simulBIOS:
;Calls the interrupt handler directly, bypassing any hooks.
;Prevents trapping and redirecting from hurting the integrity of the system.
;Follows the advice of the documentation. Lets see how well it works...
;We check we should be getting the characters just before we start our 
; read/ndread. After each call, if we are to make another call to the BIOS
; we check again to ensure that the screen state hasn't changed.
    ;call keybHdlr   ;Call int hdlr, if active add a cli to start of keybHdlr.
    call qword [pOldKbdHdlr]   ;Call original int hdlr
    pop rbx
    pop rax
    return
;----------------------------------------------------------
; Internal utility functions
;----------------------------------------------------------

getSIB:
;Input: edx (zx from dl) = Screen number to get SIB for
;Output: rbx -> SIB for that screen number
    lea rbx, sibArray
    test edx, edx
    retz
    push rax
    push rcx
    push rdx
    mov eax, edx
    mov ecx, screenSib_size ;Get screen Sib size
    mul ecx ;eax <- eax*ecx
    add rbx, rax
    pop rdx
    pop rcx
    pop rax
    return

getSibSeg:
;Get the chosen segment from the current SIB
;Input: eax (zx from ax) = Segment number to get
;Output: rbx -> Current SIB
;        rdi -> Chosen SIB segment
    mov rbx, qword [pCurSib]
    mov edi, sibSeg_size ;Get the size of the sibseg
    mul edi     ;Get the segment offset in the segment block in eax
    movzx edi, word [rbx + sib.wOffSeg] ;
    add rdi, rax    ;Turn into an offset into the SIB
    add rdi, rbx    ;Turn into a proper pointer
    return


resetScreen:
;Resets the current screen (Blanks).
    movzx ebx, byte [bCurScr]    ;Move the screen number into ebx
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
    mov ah, 2   ;Set cursor position!
    int 30h
    return
;--------------------------------------------------------------
;------------- Driver built-in Interrupt Routines -------------
;--------------------------------------------------------------
fastOutput:         ;This CON driver supports Int 29h
;Called with char to transfer in al
    call outch
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
    call procBlock
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
    call procBlock
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
    and byte [rsp + 2*8], ~40h    ;Clear ZF of flags
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
