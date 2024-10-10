
;Driver version number
majVers equ 0
minVers equ 2

conHdr:
    dq -1
    dw devDrvChar | devDrvIOCTL | devDrvMulti | devDrvFastOut | devDrvConOut | devDrvConIn
    dq strategy
    dq noOp    ;We don't use the interrupt endpoint.
    db "CON     "

funcTbl:
    dw init - funcTbl       ;Init function
    dw noOp - funcTbl       ;Media Check
    dw noOp - funcTbl       ;Build BPB
    dw unkCmd - funcTbl    ;IOCTL Input
    dw read - funcTbl       ;Read
    dw ndRead - funcTbl     ;Non-destructive read
    dw inStatus - funcTbl   ;Input Status
    dw flushInBuf - funcTbl ;Input flush
    dw write - funcTbl      ;Write
    dw write - funcTbl      ;Write with verify
    dw noOp - funcTbl       ;Output Status
    dw noOp - funcTbl       ;Output Flush
    dw noOp - funcTbl       ;IOCTL Output
    dw noOp - funcTbl       ;Device Open
    dw noOp - funcTbl       ;Device Close
    dw noOp - funcTbl       ;Removable Media
    dw unkCmd - funcTbl    ;Reserved function
    dw unkCmd - funcTbl    ;Reserved function
    dw unkCmd - funcTbl    ;Reserved function
; New functions
    dw ioctl - funcTbl      ;Generic IOCTL

funcTblE equ ($ - funcTbl)/2    ;Compute number of entries

ioctlTbl:
    dw ioctl_init - ioctlTbl   ;Init multitasking capabilities 
    dw ioctl_ls   - ioctlTbl   ;Locate Segment Information Block
    dw ioctl_ss   - ioctlTbl   ;Save Segment
    dw ioctl_rs   - ioctlTbl   ;Restore Segment
    dw ioctl_ei   - ioctlTbl   ;Enable IO
    dw ioctl_is   - ioctlTbl   ;Initialise Screen
    dw ioctl_strt - ioctlTbl   ;Start (continue) console output (ex driver call)
    dw ioctl_stop - ioctlTbl   ;Stop (freeze) console output (ex driver call)
    dw ioctl_deinst - ioctlTbl ;Deactivate Multitasking capabilities


bConBuf     db 0    ;Single byte input buffer
bKeybWait   db 0    ;Set if we are waiting on Int 36h for a keystroke
bInMulti    db 0    ;If set, in multitasking.

pOldKbdIntr dq 0    ;Ptr to the Keyboard interrupt 
pOldKbdHdlr dq 0    ;Ptr to the Keyboard service routine
pDevHlp     dq noOp ;Ptr to the DOS session help interface. Default to NOP
pScrIoOk    dq -1   ;Ptr to the DOS variable that is set if screen IO is ok


;Screen Vars
bCurScr     db 0    ;Current screen number
pCurSib     dq 0    ;Pointer to the current screen SIB
bSavScr     db 0    ;Set if we are in the screen saving procedure

sibArray    db maxSib*screenSib_size dup (0)   ;Set our screenSIB array


myIdt:
.limit  dw 0
.base   dq 0