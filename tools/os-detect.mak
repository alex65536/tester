# Detects operating system
# 
# Environment:
#   FORCE_WIN32: set to 1 if you want to build 32-bit version on Win64
# 
# Outcome:
#  OS_TYPE: Windows or Linux
#  WIN_BITS: 32 if it is Win32, 64 if it is Win64. FORCE_WIN32 sets this to 32 even on Win64.

FORCE_WIN32 ?= 0

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
 	OS_NAME := Windows
else
 	OS_NAME := $(shell uname -s)
 	ifneq ($(OS_NAME),Linux)
 		$(error Only GNU/Linux and Windows are supported)
 	endif
endif
