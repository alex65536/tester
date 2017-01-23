#!/bin/bash
# Builds timerlib for linux as a static library.
rm -f ./*.o
rm -f ./libtimer-linux.a
gcc -c ./src/linux/*.c
ar rcs libtimer-linux.a ./*.o
rm -f ./*.o

