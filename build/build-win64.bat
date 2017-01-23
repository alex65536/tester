@echo off
cd ..\timerlib
call build-win64.bat
call build-win64dll.bat
cd ..\src
lazbuild tester.lpi --os=win64 --cpu=x86_64
cd ..\build
pause