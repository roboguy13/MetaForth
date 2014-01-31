#!/bin/sh
./dist/build/mfc/mfc < tests/$1.4th > tests/$1.asm && nasm -f macho64 tests/$1.asm && ld -e _start -lc dist/build/mfc/mfc-tmp/src/rts.o tests/$1.o

