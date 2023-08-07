:: always run from top of repo
cd %~dp0\..

:: clean previous attempts
RD /S /Q builddir
RD /S /Q inst
RD /S /Q pypestutils\lib

:: setup, compile and install
meson setup builddir --prefix=%cd%\inst
meson compile -C builddir
meson install -C builddir

::copy lib files to Python module
MD pypestutils\lib
COPY /B inst\bin\libppu.dll pypestutils\lib
