#!/bin/bash
rm -f ./*.o
rm -f ./libtimer-linux.a
gcc -c ./linux/*.c 
ar rcs libtimer-linux.a ./*.o
rm -f ./*.o
