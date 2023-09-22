F90=ifort

FLAGS= /c /check:all /traceback /fpe:0
FLAGS_DRIVER_LAPACK= /c
#LD=ifort /F100000000
LD=ifort

all :   driver1.exe driver2.exe driver3.exe driver4.exe driver5.exe \
        driver6.exe driver7.exe driver8.exe driver9.exe driver10.exe \
        driver11.exe driver12.exe driver13.exe driver14.exe driver15.exe \
        driver16.exe driver17.exe

clean :
	del *.obj
	del *.lib
	del *.mod

clean_all :
	del *.obj
	del *.lib
	del *.mod
	del *.exe

driver1.exe :	driver1.obj driversubs.obj model_interface.lib
	$(LD) driver1.obj driversubs.obj model_interface.lib

driver1.obj :	driver1.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver1.f90

driver2.exe :	driver2.obj driversubs.obj model_interface.lib
	$(LD) driver2.obj driversubs.obj model_interface.lib

driver2.obj :	driver2.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver2.f90

driver3.exe :	driver3.obj driversubs.obj model_interface.lib
	$(LD) driver3.obj driversubs.obj model_interface.lib

driver3.obj :	driver3.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver3.f90

driver4.exe :	driver4.obj driversubs.obj model_interface.lib
	$(LD) driver4.obj driversubs.obj model_interface.lib

driver4.obj :	driver4.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver4.f90

driver5.exe :	driver5.obj driversubs.obj model_interface.lib
	$(LD) driver5.obj driversubs.obj model_interface.lib

driver5.obj :	driver5.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver5.f90

driver6.exe :	driver6.obj driversubs.obj model_interface.lib sgsim_code.obj funcproc2.obj
	$(LD) driver6.obj driversubs.obj model_interface.lib

driver6.obj :	driver6.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver6.f90

driver7.exe :	driver7.obj driversubs.obj model_interface.lib sgsim_code.obj funcproc2.obj
	$(LD) driver7.obj driversubs.obj model_interface.lib

driver7.obj :	driver7.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver7.f90

driver8.exe :	driver8.obj driversubs.obj model_interface.lib sgsim_code.obj funcproc2.obj
	$(LD) driver8.obj driversubs.obj model_interface.lib

driver8.obj :	driver8.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver8.f90

driver9.exe :	driver9.obj driversubs.obj model_interface.lib sgsim_code.obj lapack1.obj funcproc2.obj
	$(LD) driver9.obj driversubs.obj model_interface.lib

driver9.obj :	driver9.f90 function_interfaces.mod
	$(F90) $(FLAGS_DRIVER_LAPACK) driver9.f90

driver10.exe :	driver10.obj driversubs.obj model_interface.lib sgsim_code.obj lapack1.obj funcproc2.obj
	$(LD) driver10.obj driversubs.obj model_interface.lib

driver10.obj :	driver10.f90 function_interfaces.mod
	$(F90) $(FLAGS_DRIVER_LAPACK) driver10.f90

driver11.exe :	driver11.obj driversubs.obj model_interface.lib funcproc2.obj
	$(LD) driver11.obj driversubs.obj model_interface.lib

driver11.obj :	driver11.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver11.f90

driver12.exe :	driver12.obj driversubs.obj model_interface.lib funcproc2.obj
	$(LD) driver12.obj driversubs.obj model_interface.lib

driver12.obj :	driver12.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver12.f90

driver13.exe :	driver13.obj driversubs.obj model_interface.lib funcproc2.obj
	$(LD) driver13.obj driversubs.obj model_interface.lib

driver13.obj :	driver13.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver13.f90

driver14.exe :	driver14.obj driversubs.obj model_interface.lib funcproc2.obj
	$(LD) driver14.obj driversubs.obj model_interface.lib

driver14.obj :	driver14.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver14.f90

driver15.exe :	driver15.obj driversubs.obj model_interface.lib funcproc2.obj
	$(LD) driver15.obj driversubs.obj model_interface.lib

driver15.obj :	driver15.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver15.f90

driver16.exe :	driver16.obj driversubs.obj model_interface.lib
	$(LD) driver16.obj driversubs.obj model_interface.lib

driver16.obj :	driver16.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver16.f90

driver17.exe :	driver17.obj driversubs.obj model_interface.lib
	$(LD) driver17.obj driversubs.obj model_interface.lib

driver17.obj :	driver17.f90 function_interfaces.mod
	$(F90) $(FLAGS) driver17.f90

driversubs.obj :	driversubs.f90
	$(F90) $(FLAGS) driversubs.f90


function_interfaces.mod : function_interfaces.f90
	$(F90) $(FLAGS) function_interfaces.f90

model_interface.lib :    funcproc1.f90 dimvar.f90 deftypes.f90 utl.f90 utl_high.f90
	make -f model_interface.mak model_interface.lib

sgsim_code.obj :	sgsim_code.f90
	make -f model_interface.mak model_interface.lib

lapack1.obj :	lapack1.f
	make -f model_interface.mak model_interface.lib


funcproc2.obj :	funcproc2.f90
	make -f model_interface.mak model_interface.lib



