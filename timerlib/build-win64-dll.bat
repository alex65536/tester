@echo off
rem Builds timerlib for win64 as a shared library.
del *.o >>nul
del libtimer0.1.2-64.dll
gcc -DLINK_TO_DLL -shared -o libtimer0.1.2-64.dll src\windows\*.c -m64
copy libtimer0.1.2-64.dll ..\src\libtimer0.1.2-64.dll
del *.o >>nul
