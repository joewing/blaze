#!/bin/sh

OPTS="-O1 -mxl-barrel-shift -mno-xl-soft-mul -mno-xl-soft-div -mhard-float -mxl-float-convert -mxl-float-sqrt"

mb-as -c -o mbmon.o mbmon.s
mb-gcc -c -nostdlib $OPTS $1 -o t.o
mb-gcc -nostdlib -O1 -Tlink.ls mbmon.o t.o
mb-objcopy -O binary a.out
hexdump -e '/1 "%02x" /1 "%02x" /1 "%02x" /1 "%02x"' -v a.out
echo

