F90=ifort

FLAGS= /c /check:all /traceback /fpe:0
#LD=ifort /F100000000
LD=ifort

all :   driver1.exe driver2.exe driver3.exe driver4.exe driver5.exe

driver1.exe :	driver1.obj driversubs.obj model_interface.lib
	$(LD) driver1.obj driversubs.obj model_interface.lib

driver1.obj :	driver1.f90
	$(F90) $(FLAGS) driver1.f90

driver2.exe :	driver2.obj driversubs.obj model_interface.lib
	$(LD) driver2.obj driversubs.obj model_interface.lib

driver2.obj :	driver2.f90
	$(F90) $(FLAGS) driver2.f90

driver3.exe :	driver3.obj driversubs.obj model_interface.lib
	$(LD) driver3.obj driversubs.obj model_interface.lib

driver3.obj :	driver3.f90
	$(F90) $(FLAGS) driver3.f90

driver4.exe :	driver4.obj driversubs.obj model_interface.lib
	$(LD) driver4.obj driversubs.obj model_interface.lib

driver4.obj :	driver4.f90
	$(F90) $(FLAGS) driver4.f90

driver5.exe :	driver5.obj driversubs.obj model_interface.lib
	$(LD) driver5.obj driversubs.obj model_interface.lib

driver5.obj :	driver5.f90
	$(F90) $(FLAGS) driver5.f90

driversubs.obj :	driversubs.f90
	$(F90) $(FLAGS) driversubs.f90


model_interface.lib :    funcproc1.f90 dimvar.f90 deftypes.f90 utl.obj utl_high.f90
	make -f model_interface.mak model_interface.lib
