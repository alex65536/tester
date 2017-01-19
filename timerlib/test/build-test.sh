#!/bin/bash
cd ..
./build-linux.sh
cd ./test
rm -f ./hello 
rm -f ./timerTest
fpc src/hello.pas -XX -Xs -O3 -o./hello
fpc -Fu.. -XX -Xs -O3 src/timerTest.pas -o./timerTest
rm -f ../*.ppu
rm -f ./*.ppu
rm -f ../*.o
rm -f ./*.o
