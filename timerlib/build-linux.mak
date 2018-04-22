SHARED_NAME := libtimer-$(TIMERLIB_VERSION).so
INSTALL_PREFIX := /usr/local/lib

build-static: clean
	gcc -c ./src/linux/*.c
	ar rcs libtimer-linux.a ./*.o
	rm -f ./*.o

build-shared: clean
	gcc -DLINK_TO_DLL -shared ./src/linux/*.c -o ./$(SHARED_NAME) -fPIC
	rm -f ./*.o

clean:
	rm -f ./*.o
	rm -f ./libtimer-linux.a
	rm -f ./$(SHARED_NAME)

install:
	install ./$(SHARED_NAME) "$(INSTALL_PREFIX)"

.PHONY: install
