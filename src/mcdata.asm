
;Driver version number
vers equ 0
rev  equ 1

conHdr:
    dq -1
    dw devDrvChar | devDrvIOCTL | devDrvMulti | devDrvFastOut | devDrvConOut | devDrvConIn
    dq strategy
    dq noOp    ;We don't use the interrupt endpoint.
    db "CON     "
conBuf  db 0    ;Single byte input buffer
hlpPtr  dq noOp ;Ptr to the DOS session help interface. Default to NOP

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

oldKbdHdlr  dq 0    ;Ptr to the BIOS IRQ1 handler
magicKey    dw -1   ;Scancode/ASCII pair to search for. -1 means no search.
inHdlr      db 0    ;Set if we are in the getchar portion of IRQ1 handler.