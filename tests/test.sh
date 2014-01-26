#!/bin/sh
cabal build && ./dist/build/MetaForth/MetaForth < tests/$1.4th > tests/$1.asm && nasm -Wall -f macho64 -g tests/$1.asm && ld -lc dist/build/MetaForth/MetaForth-tmp/src/rts.o tests/$1.o

