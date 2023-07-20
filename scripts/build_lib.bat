:: always run from top of repo
cd %~dp0\..

:: clean previous attempts
RD /S /Q builddir
RD /S /Q inst

:: setup, compile and install
meson setup builddir --prefix=%cd%\inst --libdir=lib
meson compile -C builddir
meson install -C builddir
