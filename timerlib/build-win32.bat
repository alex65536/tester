@echo off
del /S *.o
del libtimer-win32.a
for %%i in (windows\*.c) do gcc -c %%i -m32
for %%i in (*.o) do ar rcs libtimer-win32.a %%i
del /S *.o
