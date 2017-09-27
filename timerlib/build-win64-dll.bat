@echo off
rem Builds timerlib for win64 as a shared library.
del *.o >>nul
del libtimer64.dll
gcc -DLINK_TO_DLL -shared -o libtimer64.dll src\windows\*.c -m64
copy libtimer64.dll ..\src\libtimer64.dll
del *.o >>nul
