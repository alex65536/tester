#!/bin/bash
cd ..
rm -rf src/lib/*
rm -rf src/backup/*
rm -f src/tester-x86_64-linux
rm -f src/tester-x86_64-linux
rm -f timerlib/*.a
rm -f timerlib/*.so
cd build
echo "Press enter to exit"
read
