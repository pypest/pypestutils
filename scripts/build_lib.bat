:: always run from top of repo
cd %~dp0\..

:: clean previous attempts
RD /S /Q builddir 2> NUL
RD /S /Q inst 2> NUL
RD /S /Q pypestutils\lib 2> NUL

:: setup, compile and install
meson setup builddir --prefix=%cd%\inst
meson compile -C builddir
meson install -C builddir

::copy lib files to Python module
MD pypestutils\lib
COPY /B inst\bin\pestutils.dll pypestutils\lib
