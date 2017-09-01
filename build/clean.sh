#!/bin/bash
# Cleans the directories.
cd ..

rm -f src/tester-x86_64-linux
rm -f src/tester-i386-linux

rm -f timerlib/*.a
rm -f timerlib/*.so

rm -rf src/lib/
rm -rf src/backup/
rm -rf timerlib/src/lib/
rm -rf timerlib/src/backup/

for i in src/* ; do
	rm -rf "$i/lib"
	rm -rf "$i/backup"
done

cd build

