@echo off
rem Builds timerlib for win32 as a shared library.
del *.o >>nul
del libtimer32.dll
mingw32-gcc -DLINK_TO_DLL -shared -o libtimer32.dll src\windows\*.c -m32
copy libtimer32.dll ..\src\libtimer32.dll
del *.o >>nul
