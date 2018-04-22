# TODO : Write help for this Makefile!
# TODO : Test on Windows!

default: build

STATIC_BUILD ?= 1
SHARED_BUILD ?= 1

TIMERLIB_VERSION := 0.1.3

include ../tools/os-detect.mak

GOOD := 0

ifeq ($(OS_NAME),Windows)
 	GOOD := 1
 	include build-win.mak
endif

ifeq ($(OS_NAME),Linux)
 	GOOD := 1
 	include build-linux.mak
endif

ifneq ($(GOOD), 1)
 	$(error Unknown OS $(OS_NAME))
endif

ifneq ($(STATIC_BUILD),0)
 	STATIC_BUILD_TARGET := build-static
endif

ifneq ($(SHARED_BUILD),0)
 	SHARED_BUILD_TARGET := build-shared
endif

build: $(STATIC_BUILD_TARGET) $(SHARED_BUILD_TARGET)

.PHONY: build-static build-shared build default clean
