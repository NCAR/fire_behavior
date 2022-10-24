
PHYSICS_DIR = ../physics
PHYSICS_INCLUDE="-I$(PHYSICS_DIR)"

IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

STATE_DIR = ../state
STATE_INCLUDE="-I$(STATE_DIR)"

all: fire_behavior.exe

objects = fire_behavior.o wrf_fire_test1_mod.o wrf_fire_test2_mod.o wrf_fire_test3_mod.o wrf_fire_test4_mod.o

objects_io = ../io/geogrid_mod.o ../io/netcdf_mod.o

fire_behavior.exe: fire_behavior.o wrf_fire_test1_mod.o wrf_fire_test2_mod.o wrf_fire_test3_mod.o wrf_fire_test4_mod.o \
	../io/geogrid_mod.o ../io/netcdf_mod.o ../io/namelist_mod.o ../state/state_mod.o \
	../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_driver.o ../physics/module_fr_fire_driver_wrf.o ../physics/module_fr_fire_util.o \
	../physics/module_fr_fire_atm.o ../physics/module_fr_fire_model.o ../physics/module_fr_fire_core.o ../physics/module_fr_fire_phys.o
	$(FC) -lnetcdf -lnetcdff $^ -o $@

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(PHYSICS_INCLUDE) $(STATE_INCLUDE) $(IO_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.exe ./*.mod  ./*.f90

  # Dependencies
fire_behavior.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_driver_wrf.o \
	wrf_fire_test1_mod.o wrf_fire_test2_mod.o wrf_fire_test3_mod.o wrf_fire_test4_mod.o

wrf_fire_test1_mod.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_util.o

wrf_fire_test2_mod.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_util.o

wrf_fire_test3_mod.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_util.o

wrf_fire_test4_mod.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_util.o ../io/geogrid_mod.o ../state/state_mod.o ../io/namelist_mod.o

