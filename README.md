# Table of contents

* [What is _Tester_?](#what-is-tester)
* [Why use _Tester_?](#why-use-tester)
* [Pre-requisites](#pre-requisites)
* [Downloading latest release](#downloading-latest-release)
* [Building from sources](#building-from-sources)
* [Documentation](#documentation)
* [License](#license)
* [Libraries used by _Tester_](#libraries-used-by-tester)

# What is _Tester_?

* _Tester_ is an application that can test competitive programming problems. It is very easy to use. No Internet connection required to test the problems -- you'll just need the tests. It can import archives like [Polygon](https://polygon.codeforces.com)'s. You can also create a problem in it -- _Tester_ has its own problem properties editor.

# Why use _Tester_?

* It's a native, lightweight application.

* It doesn't require much space and can be distributed in a single executable.

* It's cross-platform and available on _Windows_ and _GNU/Linux_

* No more batch scripts like `All_Test.bat`! _Tester_ provides an intuitive graphical interface.

* It can automatially import problems.

* _Tester_ uses _JSON_ to save problems' properties, so they can be easily parsed.

* Testing results can be saved to _JSON_ and can be easily parsed, too.

# Pre-requisites

## Common

* At first, you will need a computer 😃

* 1 GB RAM (recommended 2 GB)

* [_Free Pascal Compiler_](https://freepascal.org/) (for Pascal) and [_GNU GCC Compiler_](https://gcc.gnu.org) (for C/C++) installed. For _Windows_, paths to the compiler binaries must be set in `PATH` environment variable.

## Windows

* _Windows 7, 8, 8.1 or 10_ (32- or 64-bit). Should work on _Windows XP_, but didn't tested on it.

## GNU/Linux

* Tested on _Ubuntu 16.04_ (32- and 64-bit), but should work on other popular _GNU/Linux_ distros.

* [_GTK+_](https://www.gtk.org/) installed.

* Some checkers for problems may come only in EXE file, so you should install [_Wine_](https://winehq.org) to launch them.

# Downloading latest release

To download latest release of _Tester_, go to the following [link](https://github.com/alex65536/tester/releases/latest).

# Building from sources

## Common

* To build _tester_ from sources, you will need [_Lazarus IDE_](https://www.lazarus-ide.org) (recommended version is 1.8.0RC5) with [_Free Pascal Compiler_](https://freepascal.org/) (recommended version is 3.0.4) and [_GNU GCC Compiler_](https://gcc.gnu.org).

* You can choose the way you link _timerlib_: statically or dynamically. If you want to link _timerlib_ dynamically, uncomment `{$Define TimerlibDyn}` in `timerlib/src/timerlib.pas`.

* If you link _timerlib_ dynamically, you'll be have to distribute the shared library (`libtimer0.1.2-32.dll` or `libtimer0.1.2-64.dll` for _Windows_ and `libtimer-0.1.2.so` for _GNU/Linux_) with the executable. For _GNU/Linux_, you must install the shared library. To do this, use `timerlib/install-linux.sh` (root privileges may be required). For _Windows_, you must put the shared library to the same directory with the executable.

## Windows

* Use the environment variable `PATH` to specify paths to [_Lazarus IDE_](https://www.lazarus-ide.org) directory and to [_GNU GCC_](https://gcc.gnu.org)'s `bin` directory.

* If you link _timerlib_ statically, specify paths to the _C_ libraries in the project or in your `fp.cfg` file (you can find that file in the [_Free Pascal Compiler_](https://freepascal.org/)'s directory.  

* Use the following batch scripts: `build/build-win32.bat` (for _Win32_) and `build/build-win64.bat` (for _Win64_). They will build _Tester_ automatically.

* There can be linking problems on _Windows_ when you link _timerlib_ statically. Due to _MinGW_ bugs, linking may fail with the message:  
  `Undefined symbol: __ms_vsnprintf`  
  To workaround this issue, I built the timerlib with [_MinGW32_](https://sourceforge.net/projects/mingw/) and in _FPC_ I used libraries from the 32-bit version of [_MinGW-w64_](https://sourceforge.net/projects/mingw-w64/).
  
* Still have no idea how to link _timerlib_ statically on _Win64_, so _timerlib_ will be linked dynamically **even if you didn't uncommented `{$Define TimerlibDyn}`**.

* If you have problems with static linking, do build _timerlib_ dynamically.

## GNU/Linux

* Use `build/build-linux.sh` shell script. It will build _Tester_ automatically.

* Better use static linking (you won't have to install the shared library).

# Documentation

Coming soon...

# License

_Tester_ is free software; you can redistribute it and/or modify it under the terms of the [GNU General Public License](https://github.com/alex65536/tester/blob/master/LICENSE) as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the [GNU General Public License](https://github.com/alex65536/tester/blob/master/LICENSE) for more details.

Note that some of the _timerlib_ files may be distributed under the terms of [GNU Lesser General Public License](https://github.com/alex65536/tester/blob/master/LICENSE.lgpl). See the _timerlib_ files for more details.
  
# Libraries used by _Tester_

* _LCL_, which provides GUI for _Tester_.

* _SynEdit_, to view the sources.

* _exec.h_, _exec.c_ and some other source files from [_ejudge_ project](https://ejudge.ru/), which provides routines to run the submissions and check them for time and memory limits.

