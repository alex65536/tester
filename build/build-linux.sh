#!/bin/bash
# Builds tester for linux.
cd ../timerlib
./build-linux.sh
./build-linux-so.sh
cd ../src
lazbuild tester.lpi
cd ../build
echo "Press enter to exit"
read

