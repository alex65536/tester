# TODO : Test on Windows!

default: build

include ./tools/os-detect.mak

export FORCE_WIN32

ifeq ($(OS_NAME),Windows)
 	GOOD := 0
 	
 	ifeq ($(WIN_BITS),32)
 		TARGET_OS := --os=win32
 		TARGET_CPU := --cpu=i386
 		GOOD := 1
 	endif
 	
 	ifeq ($(WIN_BITS),64)
 		TARGET_OS := --os=win64
 		TARGET_CPU := --cpu=x86_64
 		GOOD := 1
 	endif
 	
 	ifeq ($(GOOD),0)
 		$(error WIN_BITS must be either 32 or 64)
 	endif
endif

build-timerlib:
	+cd timerlib && make build

build: build-timerlib
	cd src && lazbuild tester.lpi $(TARGET_OS) $(TARGET_CPU)

GOOD := 0

ifeq ($(OS_NAME),Windows)
clean:
	del /S /Q src\*.exe
	del /S /Q src\*.dll
	
	del /S /Q timerlib\*.a
	del /S /Q timerlib\*.dll
	
	rmdir /S /Q src\lib
	rmdir /S /Q src\backup
	rmdir /S /Q timerlib\src\lib
	rmdir /S /Q timerlib\src\backup
	
	for /D %%i in (src\*) do ( \
		rmdir /S /Q "%%i\lib" & \
		rmdir /S /Q "%%i\backup" \
	)
	
	echo OK
GOOD := 1
endif

ifeq ($(OS_NAME),Linux)
clean:
	rm -f src/tester-x86_64-linux
	rm -f src/tester-i386-linux
	rm -f *.so
	
	rm -f timerlib/*.a
	rm -f timerlib/*.so
	
	rm -rf src/lib/
	rm -rf src/backup/
	rm -rf timerlib/src/lib/
	rm -rf timerlib/src/backup/
	
	for i in src/*; do \
		echo $$i; \
		[ -d "$$i/lib" ] && rm -rf "$$i/lib"; \
		[ -d "$$i/backup" ] && rm -rf "$$i/backup"; \
	done || true
	
	echo OK
GOOD := 1
endif

ifeq ($(GOOD),0)
 	$(error OS $(OS_NAME) not supported)
endif

help:
	@echo "This Makefile builds Tester"
	@echo ""
	@echo "Supported targets:"
	@echo "    build: Builds Timerlib"
	@echo "    clean: Cleans the directory"
	@echo ""
	@echo "Variables:"
	@echo "   FORCE_WIN32: if set to 1, force building 32-bit version on Win64"

.PHONY: default build-timerlib build clean help
