@echo off
rem Builds tester for win32.
cd ..\timerlib
call build-win32.bat
call build-win32-dll.bat
cd ..\src
lazbuild tester.lpi --build-all --os=win32 --cpu=i386
cd ..\build
pause
