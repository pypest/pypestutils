#!/usr/bin/env bash
set -e

# always run from top of repo
cd $(dirname $0)/..

# clean previous attempts
rm -rf builddir
rm -rf inst

# setup, compile and install
meson setup builddir --prefix=$(pwd)/inst --libdir=lib
meson compile -C builddir
meson install -C builddir
