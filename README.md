# Contents

* [Why use **Tester**?](#why-use-tester)
* [Pre-requisites](#pre-requisites)
* [Downloading latest release](#downloading-latest-release)
* [Building from sources](#building-from-sources)
* [Libraries used by **Tester**](#libraries-used-by-tester)

# Why use **Tester**?

* It's a native, lightweight application.

* It's cross-platform and available on _Windows_ and _GNU/Linux_

* No more batch scripts like _All_Test.bat_! **Tester** provides an intuitive graphical interface.

* With adding tests from template, it's easy to import a problem to **Tester**.

* **Tester** uses _JSON_ to save problems' properties, so they can be easily parsed.

* Testing results can be saved to _JSON_ and can be easily parsed, too.

# Pre-requisites

## Common

* At first, you will need a computer 😃

* 1 GB RAM (recommended 2 GB)

* _Free Pascal_ (for Pascal) and _GNU GCC_ (for C/C++) installed. For _Windows_, paths to the compiler binaries must be set in PATH environment variable.

## Windows

* _Windows 7, 8, 8.1 or 10_ (32- or 64-bit). Should work on _Windows XP_, but didn't tested on it.

## GNU/Linux

* Tested on _Ubuntu 16.04_ (64-bit), but should work on other popular _GNU/Linux_ distros.

* _GTK_ installed.

* Some checkers for problems may come only in EXE file, so you should install _Wine_ to launch them.

# Downloading latest release

To download latest release of **Tester**, go to the following [link](https://github.com/alex65536/tester/releases/latest).

# Building from sources

## Common

* To build _tester_ from sources, you will need _Lazarus IDE_ (recommended version is 1.6.2) with _Free Pascal_ compiler (recommended version is 3.0.0) and _GNU GCC_ compiler.

* You can choose the way you link _timerlib_: statically or dynamically. If you want to link _timerlib_ dynamically, uncomment `{$Define TimerlibDyn}` in `timerlib/src/timerlib.pas`.

* If you link _timerlib_ dynamically, you'll be have to distribute the shared library (_libtimer32.dll_ or _libtimer64.dll_ for _Windows_ and _libtimer.so_ for _GNU/Linux_). **Note:** For _GNU/Linux_, you must install the shared library. To do this, use `timerlib\install-linux.sh`.

## Windows

* Use the following batch scripts: `build/build-win32.bat` (for _Win32_) and `build/build-win64.bat` (for _Win64_). They will build _tester_ automatically.

* There can be linking problems on _Windows_ when you link _timerlib_ statically. Due to _MinGW_ bugs, linking may fail with the message:  
  `Undefined symbol: __ms_vsnprintf`  
  To workaround this issue, I built the timerlib with _MinGW32_ and in _FPC_ I used libraries from the 32-bit version of _MinGW-w64_.
  
* Still have no idea how to link _timerlib_ statically on _Win64_, so _timerlib_ will be linked dynamically **even if you didn't uncommented `{$Define TimerlibDyn}`**.

* If you have problems with static linking, do build _timerlib_ dynamically.

## GNU/Linux

* Use `build/build-linux.sh` shell script. It will build _tester_ automatically.

* Better use static linking (you won't have to install the shared library).
  
# Libraries used by **Tester**

* _LCL_, which provides GUI for **Tester**.

* _SynEdit_, to view the sources.

* _exec.h_, _exec.c_ and some other source files from [_ejudge_ project](https://ejudge.ru/), which provides routines to run the submissions and check them for time and memory limits.

