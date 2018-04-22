SHARED_SHORT_NAME := libtimer$(TIMERLIB_VERSION)
SHARED_NAME := $(SHARED_SHORT_NAME)-$(WIN_BITS).dll

STATIC_NAME := libtimer-win$(WIN_BITS).a

COMPILE_TARGET := -m$(WIN_BITS)

ifeq ($(WIN_BITS),32)
 	CC := mingw32-gcc
else
 	ifeq ($(WIN_BITS),64)
 		CC := gcc
 	else
 		$(error WIN_BITS must be either 32 or 64)
 	endif
endif

build-static: clean-$(WIN_BITS)
	$(CC) -c $(wildcard src/windows/*.c) $(COMPILE_TARGET)
	ar rcs $(STATIC_NAME) *.o
	del /Q *.o >>nul

build-shared: clean-$(WIN_BITS)
	$(CC) -DLINK_TO_DLL -shared -o $(SHARED_NAME) $(wildcard src/windows/*.c) $(COMPILE_TARGET)
	del /Q *.o >>nul
	copy $(SHARED_NAME) ..\src\$(SHARED_NAME)
	copy $(SHARED_NAME) ..\tsrun\$(SHARED_NAME)

clean-32:
	del /Q *.o >>nul
	del /Q libtimer-win32.a >>nul
	del /Q $(SHARED_SHORT_NAME)-32.dll >>nul

clean-64:
	del /Q *.o >>nul
	del /Q libtimer-win64.a >>nul
	del /Q $(SHARED_SHORT_NAME)-64.dll >>nul

clean: clean-32 clean-64

.PHONY: clean-32 clean-64
