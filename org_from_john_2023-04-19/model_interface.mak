F90=ifort

FLAGS= /c /check:all /traceback /fpe:0
#FLAGS= /c /check:all /traceback /fpe:0  /Qfp-stack-check /RTCu
#FLAGS= /c /traceback /fpe:0 /RTCu /CU /O2

LD=ifort
#LDFLAGS= /check:all /traceback /dll
LDFLAGS= /dll

######################################################################
# DON'T EDIT BELOW THIS LINE
######################################################################

all :   model_interface.lib

model_interface.lib :    funcproc1.obj dimvar.obj deftypes.obj utl.obj utl_high.obj
        del model_interface.lib
        lib funcproc1.obj deftypes.obj utl.obj utl_high.obj /out:model_interface.lib

################################################################
# Visible functions
################################################################

funcproc1.obj :      funcproc1.f90 dimvar.obj utl.obj deftypes.obj utl_high.obj
        $(F90) $(FLAGS) funcproc1.f90

################################################################
# Lower level functions
################################################################

dimvar.obj : dimvar.f90
        $(F90) $(FLAGS) dimvar.f90

deftypes.obj : deftypes.f90 dimvar.obj
        $(F90) $(FLAGS) deftypes.f90

utl.obj :      utl.f90 dimvar.obj
        $(F90) $(FLAGS) utl.f90

################################################################
# Higher level functions
################################################################

utl_high.obj :      utl_high.f90 dimvar.obj deftypes.obj utl.obj
        $(F90) $(FLAGS) utl_high.f90

################################################################
# CLEANING UP
################################################################

clean :
        del *.lib
        del *.dll
        del *.mod
        del *.obj



