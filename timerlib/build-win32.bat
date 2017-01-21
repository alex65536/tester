@echo off
del /S *.o >>nul
del libtimer-win32.a
for %%i in (src\windows\*.c) do mingw32-gcc -c %%i -m32
for %%i in (*.o) do ar rcs libtimer-win32.a %%i
del /S *.o >>nul
