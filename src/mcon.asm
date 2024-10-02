[map all ./lst/mcon.map]
DEFAULT REL
BITS 64

%include "./src/drvStruc.inc"
%include "./src/mcon.inc"
%include "./src/dosMacro.mac"

Segment cseg start=0 vstart=0
%include "./src/mcdata.asm" ;Cannot put this in a separate segment due to reloc issues
%include "./src/mccode.asm"
