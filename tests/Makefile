
PHYSICS_DIR = ../physics
PHYSICS_INCLUDE="-I$(PHYSICS_DIR)"

IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

STATE_DIR = ../state
STATE_INCLUDE="-I$(STATE_DIR)"

DRIVER_DIR = ../driver
DRIVER_INCLUDE="-I$(DRIVER_DIR)"

all: fire_behavior.exe

objects = fire_behavior.o

objects_io = ../io/geogrid_mod.o ../io/netcdf_mod.o

fire_behavior.exe: fire_behavior.o \
	../io/geogrid_mod.o ../io/netcdf_mod.o ../io/namelist_mod.o ../state/state_mod.o ../driver/initialize_mod.o ../driver/advance_mod.o \
	../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_driver.o ../physics/module_fr_fire_driver_wrf.o ../physics/module_fr_fire_util.o \
	../physics/module_fr_fire_atm.o ../physics/module_fr_fire_model.o ../physics/module_fr_fire_core.o ../physics/module_fr_fire_phys.o
	$(FC) -lnetcdf -lnetcdff $^ -o $@

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(PHYSICS_INCLUDE) $(STATE_INCLUDE) $(IO_INCLUDE) $(DRIVER_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.exe ./*.mod  ./*.f90

  # Dependencies
fire_behavior.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_driver_wrf.o ../driver/initialize_mod.o ../driver/advance_mod.o

