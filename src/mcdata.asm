
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
    dw unkExit - funcTbl    ;IOCTL Input
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
    dw unkExit - funcTbl    ;Reserved function
    dw unkExit - funcTbl    ;Reserved function
    dw unkExit - funcTbl    ;Reserved function
; New functions
    dw genIOCTL - funcTbl   ;Generic IOCTL

funcTblE equ ($ - funcTbl)/2    ;Compute number of entries

bConBuf     db 0    ;Single byte input buffer
bSTMode     db -1   ;This value stays -1 whilst we are in singletasking mode
wMagicKey   dw -1   ;Scancode/ASCII pair to search for. -1 means no search.
bKeybWait   db 0    ;Set if we are waiting on Int 36h for a keystroke

pOldKbdIntr dq 0    ;Ptr to the Keyboard interrupt 
pOldKbdHdlr dq 0    ;Ptr to the Keyboard service routine
pDevHlp     dq noOp ;Ptr to the DOS session help interface. Default to NOP
pScrIoOk    dq -1   ;Ptr to the DOS variable that is set if screen IO is ok
