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

# on macOS add "-ld_classic" to LDFLAGS if ld-classic is present and not deprecated
if [[ $OSTYPE == darwin* ]]; then
  XCODE_PATH=$(/usr/bin/xcode-select -p 2> /dev/null)
  if [ -n $XCODE_PATH ]; then
    if [ -x "$XCODE_PATH/usr/bin/ld-classic" -o \
         -x "$XCODE_PATH/Toolchains/XcodeDefault.xctoolchain/usr/bin/ld-classic" ]; then
      if [ -z "$(ld -ld_classic 2>&1 | grep 'ld_classic is deprecated')" ]; then
        export LDFLAGS="$LDFLAGS -Wl,-ld_classic"
      fi
    fi
  fi
fi

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
