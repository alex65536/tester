#!/bin/bash
# Builds timerlib for linux as a shared library.
rm -f ./*.o
rm -f ./libtimer-0.1.2.so
gcc -DLINK_TO_DLL -shared ./src/linux/*.c -o ./libtimer-0.1.2.so -fPIC
rm -f ./*.o
cp ./libtimer-0.1.2.so ../src
cp ./libtimer-0.1.2.so ../tsrun
