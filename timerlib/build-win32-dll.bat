@echo off
rem Builds timerlib for win32 as a shared library.
del *.o >>nul
del libtimer0.1.2-32.dll
mingw32-gcc -DLINK_TO_DLL -shared -o libtimer0.1.2-32.dll src\windows\*.c -m32
copy libtimer0.1.2-32.dll ..\src\libtimer0.1.2-32.dll
del *.o >>nul
