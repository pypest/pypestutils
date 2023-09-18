# PyPestUtils

Suite of Python tools based on PEST utilities.

This package is currently in pre-alpha development, and is not suitable for use, but early adopters are welcome to have a go.

## Installation
### Easy way

Use `pip` to install a built distribution for Windows, Linux or macOS:

    pip install pypestutils

to also include optional requirements use:

    pip install pypestutils[optional]

### From source

Installation from source requires a Fortran compiler and meson. Use the following sections to build a shared library of pestutils, which then needs to be copied into the module path `pypestutils/lib/` to enable it to be found.

#### Build pestutils on Linux / macOS

A script that can be used to configure build and install the shared library is `./scripts/build_lib.sh`.

Or manually:
```bash
meson setup builddir --prefix=$(pwd)/inst
meson compile -C builddir

# copy for Python module
mkdir pypestutils/lib
# for Linux
cp builddir/pestutils/libpestutils.so pypestutils/lib/
# for macOS
cp builddir/pestutils/libpestutils.dylib pypestutils/lib/
# for Windows via MSYS2
cp builddir/pestutils/pestutils.dll pypestutils/lib/
```

#### Build pestutils on Windows

There are two methods to compile Fortran code on Windows, using MSYS2 via bash or a conda environment via cmd. 

1. MSYS2 can use `./scripts/build_lib.sh` similar to Linux.

2. To use a conda, install `m2w64-gcc-fortran` and `meson` in a conda environment. With the environment active, call `.\scripts\build_lib.bat`. This will compile the library, writting `pestutils.dll` in the `.\inst\bin\` folder. Copy `pestutils.dll` into the module path `pypestutils\lib\` to enable it to be found. 

Then, install the local version of this package with pip.

#### Install pypestutils

After a shared library of pestutils is compiled and placed into `pypestutils/lib/` install an "editable" version of this Python package:
```
pip install -e .
```
