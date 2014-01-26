#!/bin/sh
ghc MetaForth && ./MetaForth < $1.4th > $1.asm && nasm -Wall -f macho64 -g $1.asm && gcc -c rts.c && ld -lc ../dist/build/MetaForth/MetaForth-tmp/src/rts.o $1.o

