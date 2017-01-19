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

* Currenty only 64-bit version is available, so 32-bit _GNU/Linuxes_ are not supported now.  
  But you can build a 32-bit version from sources. See [_Building from sources_](#building-from-sources)

* _GTK_ installed.

* Some checkers for problems may come only in EXE file, so you should instal _Wine_ to launch them.

# Downloading latest release

To download latest release of **Tester**, go to the following [link](https://github.com/alex65536/tester/releases/latest).

# Building from sources

* You will need _Lazarus IDE_ (recommended version is 1.6.2) with _Free Pascal_ compiler (recommended version is 3.0.0) and _GNU GCC_ compiler.

* To build from sources, do the following steps:
 
  * Download the project  
    You can use the following command in terminal: _git clone https://github.com/alex65536/tester_
  
  * Build _timerlib_. Use _timerlib/build-win32.bat_ for _Windows_ and _timerlib/build-linux.bat_ for _GNU/Linux_.
  
  * Open the project inside _Lazarus_.
  
  * Click _Run > Build_.
  
  * That's all! 😃
  
  * **Important note:** On _Windows_, you'll need some magic skills to link the project with _timerlib_. You can try to change libraries in _timerlib/timerlib.pas_ or paths to them in the project options.  
    I've spent about 4 hours to link the project on _Windows_ for the first time. To make building less painful, I'll add a possiblity to build _timerlib_ as a dynamic library (which won't require linking the project with C libraries). 
  
# Libraries used by **Tester**

* _LCL_, which provides GUI for **Tester**.

* _SynEdit_, to view the sources.

* _exec.h_, _exec.c_ and some other source files from [_ejudge_ project](https://ejudge.ru/), which provides routines to run the submissions and check them for time and memory limits.
