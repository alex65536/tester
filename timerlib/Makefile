# TODO : Write help for this Makefile!
# TODO : Test on Windows!

default: build

STATIC_BUILD ?= 1
SHARED_BUILD ?= 1

FORCE_WIN32 ?= 0

TIMERLIB_VERSION := 0.1.3

ifeq ($(OS),Windows_NT)
 	WIN_BITS := 0
 	ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
 		WIN_BITS := 64
 	else
 		ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
 			WIN_BITS := 64
 		endif
 		ifeq ($(PROCESSOR_ARCHITECTURE),x86)
 			WIN_BITS := 32
 		endif
 	endif
 	ifeq ($(WIN_BITS),0)
		$(error Cannot determine target CPU)
 	endif
 	ifneq ($(FORCE_WIN32),0)
		WIN_BITS := 32
 	endif
 	include build-win.mak
else
 	OS_NAME := $(shell uname -s)
 	ifneq ($(OS_NAME),Linux)
 		$(error Only GNU/Linux and Windows are supported)
 	endif
 	include build-linux.mak
endif

ifneq ($(STATIC_BUILD),0)
 	STATIC_BUILD_TARGET := build-static
endif

ifneq ($(SHARED_BUILD),0)
 	SHARED_BUILD_TARGET := build-shared
endif

build: $(STATIC_BUILD_TARGET) $(SHARED_BUILD_TARGET)

.PHONY: build-static build-shared build default clean
