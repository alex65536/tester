#!/bin/bash
# Builds timerlib for linux as a shared library.
rm -f ./*.o
rm -f ./libtimer.so.0.1.2
gcc -DLINK_TO_DLL -shared ./src/linux/*.c -o ./libtimer.so.0.1.2 -fPIC
rm -f ./*.o

