@echo off
rem Cleans the directories.
cd ..

del /S /Q src\*.exe

del /S /Q timerlib\*.a
del /S /Q timerlib\*.dll

del /S /Q src\lib
del /S /Q src\backup

for /D %%i in (src\*) do (
	del /S /Q "%%i\lib" 
	del /S /Q "%%i\backup"
)

cd build
