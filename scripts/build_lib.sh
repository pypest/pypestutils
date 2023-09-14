#!/usr/bin/env bash
set -e

# always run from top of repo
cd $(dirname $0)/..

# this needs bash
case "$OSTYPE" in
  darwin*)  libname=lib/libpestutils.dylib ;;
  linux*)   libname=lib/libpestutils.so ;;
  msys* )   libname=bin/pestutils.dll ;;
  *) echo "unknown \$OSTYPE: $OSTYPE" && exit 1 ;;
esac

# clean previous attempts
rm -rf builddir
rm -rf inst
rm -rf pypestutils/lib

# setup, compile and install
meson setup builddir --prefix=$(pwd)/inst --libdir=lib
meson compile -C builddir
meson install -C builddir

# copy lib file to Python module
mkdir pypestutils/lib
echo "Copying $libname to pypestutils/lib/"
cp inst/$libname pypestutils/lib/
