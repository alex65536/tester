@echo off
cd ..\timerlib
call build-win32.bat
call build-win32dll.bat
cd ..\src
lazbuild tester.lpi --os=win32 --cpu=i386
cd ..\build
pause