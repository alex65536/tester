cd ..
call build-win32.bat
cd test
del hello.exe
del timerTest.exe
copy ..\timerlib.dll timerlib.dll 
fpc src\hello.pas -XX -Xs -O3 -o.\hello.exe
fpc -Fu.. -Fl.. -Fo.. -XX -Xs -O3 src\timerTest.pas -o.\timerTest.exe
del /S ..\*.ppu
del /S *.ppu
del /S ..\*.o
del /S *.o
del /S *.a
