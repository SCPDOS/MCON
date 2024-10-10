;MCON screen capabilities

struc mScrCap
    .wVer       dw ?    ;Upper byte, Major num (1). Lower byte, minor num (0)
    .wLen       dw ?    ;Length of structure (15 bytes)
    .bScrNum    db ?    ;Number of screens supported by driver
    .pDevHlp    dq ?    ;Pointer to the mCon help routine
endstruc

maxScr  equ 07h ;Screens 0 - 7 (so 8 screens)

DevHlp_ConsInputFilter  equ 5
DevHlp_Signal_SM        equ 7
DevHlp_ProcBlock        equ 9
DevHlp_ProcRun          equ 10
DevHlp_GetDOSVar        equ 16


maxSib      equ maxScr + 1   
maxSibSeg   equ 1   ;There is only one (empty) segment per SIB.

%macro sibM 0 ;SIB macro for expanding
    .bFrozenFlg db ?    ;0 means NOT frozen!
    .wOffSeg    dw ?    ;Offset from the start of the SIB to first segment
    .wSegCnt    dw ?    ;Number of segments
    .wSibLen    dw ?    ;Total length of the SIB
%endmacro

%macro sibSegM 0    ;SIB Segment macro for expanding
    .wSegSize   dw ?    ;Size of the segment. 0 = Free. Driver maintains.
    .wMemFlag   dw ?    ;Clear if seg in mem. Else swapped. DOSMGR maintains.
    .pMemPtr    dq ?    ;Pointer to the segment in memory. DOSMGR maintains.
%endmacro

struc sib
    sibM
endstruc

struc sibSeg
    sibSegM
endstruc

struc screenSib 
;Our screen sib has one segment only and it comes immediately after the sib
    sibM
    sibSegM
endstruc