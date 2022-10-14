
#FC=gfortran
#FCFLAGS=-std=f2008 -ffree-form -fbacktrace -fcheck=all -finit-real=nan -fall-intrinsics -g -Og
#CPP=cpp
#CPPFLAGS=-nostdinc -C -P -w

objects = wrf_atmosphere_mod.o module_fr_fire_util.o module_fr_fire_phys.o module_fr_fire_core.o \
	 module_fr_fire_model.o module_fr_fire_atm.o module_fr_fire_driver.o module_fr_fire_driver_wrf.o module_fr_fire_util.o

target: $(objects)
%.o: %.F
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) -c $*.f90

clean:
	rm -f ./*.o ./*.exe ./*.mod  ./*.f90

  # Dependencies
module_fr_fire_driver_wrf.o: module_fr_fire_driver.o module_fr_fire_atm.o module_fr_fire_util.o

module_fr_fire_driver.o: wrf_atmosphere_mod.o module_fr_fire_util.o module_fr_fire_phys.o

module_fr_fire_atm.o: wrf_atmosphere_mod.o module_fr_fire_util.o

module_fr_fire_model.o: wrf_atmosphere_mod.o module_fr_fire_core.o module_fr_fire_phys.o module_fr_fire_util.o

module_fr_fire_core.o: wrf_atmosphere_mod.o module_fr_fire_phys.o module_fr_fire_util.o

module_fr_fire_phys.o: wrf_atmosphere_mod.o module_fr_fire_util.o

module_fr_fire_util.o: wrf_atmosphere_mod.o

