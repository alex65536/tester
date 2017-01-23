@echo off
rem Builds timerlib for win64 as a static library.
del /S *.o >>nul
del libtimer-win64.a
gcc -c src\windows\*.c -m64
ar rcs libtimer-win64.a *.o
del /S *.o >>nul
