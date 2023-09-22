F90=ifort

FLAGS= /c /check:all /traceback /fpe:0
#FLAGS= /c /check:all /traceback /fpe:0  /Qfp-stack-check /RTCu
#FLAGS= /c /traceback /fpe:0 /RTCu /CU /O2
FLAGS_SGSIM= /c /traceback /fpe:0 /RTCu /CU /O2
FLAGS_LAPACK= /c /O2 /fixed /traceback

LD=ifort
#LDFLAGS= /check:all /traceback /dll
LDFLAGS= /dll

######################################################################
# DON'T EDIT BELOW THIS LINE
######################################################################

all :	function_interfaces.mod model_interface.lib

model_interface.lib :    funcproc1.obj funcproc2.obj dimvar.obj deftypes.obj utl.obj utl_high.obj sgsim_code.obj \
                         lapack1.obj
        del model_interface.lib
        lib funcproc1.obj funcproc2.obj deftypes.obj utl.obj utl_high.obj sgsim_code.obj lapack1.obj /out:model_interface.lib

function_interfaces.mod : function_interfaces.f90 dimvar.obj
	$(F90) $(FLAGS) function_interfaces.f90

################################################################
# Visible functions
################################################################

funcproc1.obj :      funcproc1.f90 dimvar.obj utl.obj deftypes.obj utl_high.obj
        $(F90) $(FLAGS) funcproc1.f90

funcproc2.obj :      funcproc2.f90 dimvar.obj utl.obj deftypes.obj utl_high.obj sgsim_code.obj
        $(F90) $(FLAGS) funcproc2.f90

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
# Other functions
################################################################

sgsim_code.obj :      sgsim_code.f90
        $(F90) $(FLAGS_SGSIM) sgsim_code.f90

lapack1.obj :	lapack1.f
	$(F90) $(FLAGS_LAPACK) lapack1.f

################################################################
# CLEANING UP
################################################################

clean :
        del *.lib
        del *.dll
        del *.mod
        del *.obj



