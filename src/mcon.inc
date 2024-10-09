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