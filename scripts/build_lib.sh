#!/bin/sh
set -e

# always run from top of repo
cd $(dirname $0)/..

# clean previous attempts
rm -rf builddir
rm -rf inst
rm -rf pypestutils/lib

# setup, compile and install
meson setup builddir --prefix=$(pwd)/inst --libdir=lib
meson compile -C builddir
meson install -C builddir

# copy lib files to Python module
mkdir pypestutils/lib
if [ "$MSYSTEM" = "MSYS" ]; then
  cp inst/bin/pestutils.dll pypestutils/lib/
elif [ "$(uname)" = "Darwin" ]; then
  cp inst/lib/libpestutils.dylib pypestutils/lib/
elif [ "$(uname)" = "Linux" ]; then
  cp inst/lib/libpestutils.so pypestutils/lib/
fi
