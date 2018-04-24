.PHONY: build clean install
.PRECIOUS: logo.ico

PREFIX ?= $(DESTDIR)/usr/local

logo.ico: logo.svg
	./build_icon.sh

build: logo.ico

clean:
	$(RM) logo*.png logo.ico

install:
	./install_icon.sh $(PREFIX)/share
