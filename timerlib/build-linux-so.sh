#!/bin/bash
# Builds timerlib for linux as a shared library.
rm -f ./*.o
rm -f ./libtimer.so
gcc -DLINK_TO_DLL -shared ./src/linux/*.c -o ./libtimer.so -fPIC
rm -f ./*.o

