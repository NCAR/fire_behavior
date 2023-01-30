
PHYSICS_DIR = ../physics
PHYSICS_INCLUDE="-I$(PHYSICS_DIR)"

STATE_DIR = ../state
STATE_INCLUDE="-I$(STATE_DIR)"

IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

fire_behavior.exe: fire_behavior.o \
	../io/geogrid_mod.o ../io/netcdf_mod.o ../io/namelist_mod.o ../state/state_mod.o ../share/proj_lc_mod.o \
	initialize_mod.o advance_mod.o ../share/datetime_mod.o ../io/wrf_mod.o ../share/constants_mod.o \
	../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_driver.o ../physics/module_fr_fire_driver_wrf.o ../physics/module_fr_fire_util.o \
	../physics/module_fr_fire_atm.o ../physics/module_fr_fire_model.o ../physics/module_fr_fire_core.o ../physics/module_fr_fire_phys.o
	$(FC) $^ -o $@ $(NETCDF_LIBS)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(PHYSICS_INCLUDE) $(STATE_INCLUDE) $(IO_INCLUDE) $(NETCDF_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90 ./*.exe

  # Dependencies
fire_behavior.o: ../state/state_mod.o ../io/namelist_mod.o initialize_mod.o advance_mod.o ../io/wrf_mod.o

initialize_mod.o: ../state/state_mod.o ../io/namelist_mod.o ../io/geogrid_mod.o ../physics/module_fr_fire_driver_wrf.o

advance_mod.o: ../state/state_mod.o ../io/namelist_mod.o ../physics/module_fr_fire_driver_wrf.o

