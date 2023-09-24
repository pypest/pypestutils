# Building

Pypestutils requires a `pestutils` shared library, which needs to be compiled with the following requirements:

 - Fortran compiler - tested successfully with GNU Fortran (gfortran) and Intel Fortran (ifort)
 - [Meson](https://mesonbuild.com/) >=1.1.0
 - [Ninja](https://ninja-build.org/)

Recent versions of meson and ninja can be installed via Python pip (if available) using `pip install meson ninja`.

The following sections describe how to build a shared library of `pestutils`, which then needs to be copied into the module path `pypestutils/lib/` to enable it to be found by the Python module via ctypes.

Then to install a development version of pypestutils use:
```bash
pip install -e .
```

## Build pestutils on Linux / macOS / Windows (MSYS2)

The easiest method is to run a bash script to configure build and install the shared library:
```bash
 ./scripts/build_lib.sh
```

Or manually with these steps:
```bash
meson setup builddir  # options
meson compile -C builddir
```

After it is compiled, install to the Python module using:
```bash
mkdir pypestutils/lib
# for Linux
cp builddir/pestutils/libpestutils.so pypestutils/lib/
# for macOS
cp builddir/pestutils/libpestutils.dylib pypestutils/lib/
# for Windows via MSYS2
cp builddir/pestutils/pestutils.dll pypestutils/lib/
```

## Build pestutils on Windows

There are a few methods to compile the Fortran library on Windows.

### MSYS2

Set-up a development environment with these tools:
```bash
pacman -S mingw-w64-x86_64-meson mingw-w64-x86_64-ninja mingw-w64-x86_64-gcc-fortran
```
To build a shared library without other dependencies, use the following linker flags:
```bash
export LDFLAGS="-static-libgcc -static-libgfortran -static-libquadmath -Wl,-Bstatic,--whole-archive -lwinpthread -Wl,--no-whole-archive"
```
Then refer to the above section, as the steps are similar to Linux and macOS via MSYS2 bash.

### CMD.EXE

This method uses Windows' classic CMD.EXE command prompt, which can be configured according to the next sections.

The easy method to configure, build and install the library is to run:
```
CALL scripts\build_lib.bat
```

Or manually with these steps:
```
meson setup builddir  # options
meson compile -C builddir
```

After it is compiled, install using:
```
MD pypestutils\lib
COPY /B inst\bin\pestutils.dll pypestutils\lib
```

#### Conda

Conda (or miniconda, miniforge, mambaforge, etc.) is a popular tool for development, particularly for Windows. Install the requirements using conda or mamba:
```
mamba install m2w64-gcc-fortran meson ninja
```

#### Intel oneAPI

Intel oneAPI can be installed with a Fortran compiler. After it is installed, open an Intel oneAPI Tools command prompt with ifort, and ensure meson and ninja are also available too.
