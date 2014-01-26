#!/bin/sh
./dist/build/MetaForth/MetaForth < tests/$1.4th > tests/$1.asm && nasm -f macho64 tests/$1.asm && ld -e _start -lc dist/build/MetaForth/MetaForth-tmp/src/rts.o tests/$1.o

