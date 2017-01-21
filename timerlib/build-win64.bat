@echo off
del /S *.o >>nul
del libtimer-win64.a
for %%i in (src\windows\*.c) do gcc -c %%i -m64
for %%i in (*.o) do ar rcs libtimer-win64.a %%i
del /S *.o >>nul
