@echo off
rem Cleans the directories.
cd ..

del /S /Q src\*.exe

del /S /Q timerlib\*.a
del /S /Q timerlib\*.dll

rmdir /S /Q src\lib
rmdir /S /Q src\backup

for /D %%i in (src\*) do (
	rmdir /S /Q "%%i\lib"
	rmdir /S /Q "%%i\backup"
)

cd build

pause
