@echo off
del /S *.o >>nul
del libtimer-win32.a
mingw32-gcc -c src\windows\*.c -m32
ar rcs libtimer-win32.a *.o
del /S *.o >>nul
