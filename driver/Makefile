
PHYSICS_DIR = ../physics
PHYSICS_INCLUDE="-I$(PHYSICS_DIR)"

STATE_DIR = ../state
STATE_INCLUDE="-I$(STATE_DIR)"

IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

objects = initialize_mod.o advance_mod.o

target: $(objects)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(PHYSICS_INCLUDE) $(STATE_INCLUDE) $(IO_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

initialize_mod.o: ../state/state_mod.o ../io/namelist_mod.o ../io/geogrid_mod.o ../physics/module_fr_fire_driver_wrf.o ../physics/module_fr_fire_util.o

advance_mod.o: ../state/state_mod.o ../io/namelist_mod.o ../physics/module_fr_fire_driver_wrf.o

