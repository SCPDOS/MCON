#!/bin/sh

NAME	:= mcon
ASM     := nasm

sys:
	nasm ./src/${NAME}.asm -o ./bin/${NAME}.sys -f bin -l ./lst/${NAME}.lst -O0v
