@echo off
rem Builds tester for win64.
cd ..\timerlib
call build-win64.bat
call build-win64-dll.bat
cd ..\src
lazbuild tester.lpi --build-all --os=win64 --cpu=x86_64
cd ..\build
pause
