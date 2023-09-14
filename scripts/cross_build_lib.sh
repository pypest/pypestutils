#!/usr/bin/env bash
set -e

# always run from top of repo
cd $(dirname $0)/..

# clean previous attempts
rm -rf builddir
rm -rf inst
rm -rf pypestutils/lib

# setup, compile and install
meson setup builddir --prefix=$(pwd)/inst --cross-file etc/linux-mingw-w64-64bit.txt
meson compile -C builddir
meson install -C builddir

# copy lib files to Python module
mkdir pypestutils/lib
cp inst/bin/pestutils.dll pypestutils/lib/
