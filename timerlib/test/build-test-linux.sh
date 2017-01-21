#!/bin/bash
cd ..
./build-linux.sh
cd ./test
rm -f ./hello 
rm -f ./timerTest
fpc src/hello.pas -XX -Xs -O3 -o./hello
fpc -Fu../src -Fl.. -XX -Xs -O3 src/timerTest.pas -o./timerTest
rm -rf *.ppu
rm -rf *.o
