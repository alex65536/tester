@echo off
rem Builds timerlib for win32 as a static library.
del *.o >>nul
del libtimer-win32.a
mingw32-gcc -c src\windows\*.c -m32
ar rcs libtimer-win32.a *.o
del *.o >>nul
