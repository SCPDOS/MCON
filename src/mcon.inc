;MCON screen capabilities

struc mScrCap
    .wVer       dw ?    ;Upper byte, Major num (1). Lower byte, minor num (0)
    .wLen       dw ?    ;Length of structure (15 bytes)
    .wMagic     dw ?    ;Magic key combo (scancode/ASCII)
    .bScrNum    db ?    ;Number of screens supported by driver
    .pDevHlp    dq ?    ;Pointer to the mCon help routine
endstruc

maxScr  equ 07h ;Screens 0 - 7 (so 8 screens)