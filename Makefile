
FC=gfortran
#FCFLAGS=-ffree-form -std=f2008
FCFLAGS=-std=f2008 -ffree-form -fbacktrace -fcheck=all -finit-real=nan -fall-intrinsics -Wall -g -Og
CPP=cpp
CPPFLAGS=-nostdinc -C -P -w

target: fire_behavior.exe

objects: wrf_atmosphere_mod.o module_fr_fire_util.o module_fr_fire_phys.o module_fr_fire_core.o \
	 module_fr_fire_model.o module_fr_fire_atm.o module_fr_fire_driver.o module_fr_fire_driver_wrf.o module_fr_fire_util.o

  # Executable(s)
fire_behavior.exe: fire_behavior.o wrf_atmosphere_mod.o wrf_fire_test1_mod.o wrf_fire_test2_mod.o \
	module_fr_fire_driver.o	module_fr_fire_driver_wrf.o  module_fr_fire_util.o \
	module_fr_fire_atm.o module_fr_fire_model.o module_fr_fire_core.o module_fr_fire_phys.o
	$(FC) -o $@ $^

fire_behavior.o: fire_behavior.F wrf_atmosphere_mod.o wrf_fire_test1_mod.o wrf_fire_test2_mod.o \
	module_fr_fire_driver.o	module_fr_fire_driver_wrf.o  module_fr_fire_util.o \
	module_fr_fire_atm.o module_fr_fire_model.o module_fr_fire_core.o module_fr_fire_phys.o
	$(CPP) $(CPPFLAGS) $< > fire_behavior.f90
	$(FC) $(FCFLAGS) -c fire_behavior.f90

  # Test cases
wrf_fire_test1_mod.o: wrf_fire_test1_mod.F wrf_atmosphere_mod.o module_fr_fire_util.o
	$(CPP) $(CPPFLAGS) $< > wrf_fire_test1_mod.f90
	$(FC) $(FCFLAGS) -c wrf_fire_test1_mod.f90

wrf_fire_test2_mod.o: wrf_fire_test2_mod.F wrf_atmosphere_mod.o module_fr_fire_util.o
	$(CPP) $(CPPFLAGS) $< > wrf_fire_test2_mod.f90
	$(FC) $(FCFLAGS) -c wrf_fire_test2_mod.f90


  # WRF fire modules
module_fr_fire_driver_wrf.o: module_fr_fire_driver_wrf.F module_fr_fire_driver.o module_fr_fire_atm.o module_fr_fire_util.o
	$(CPP) $(CPPFLAGS) $< > module_fr_fire_driver_wrf.f90
	$(FC) $(FCFLAGS) -c module_fr_fire_driver_wrf.f90

module_fr_fire_driver.o: module_fr_fire_driver.F wrf_atmosphere_mod.o module_fr_fire_util.o module_fr_fire_phys.o \
	 module_fr_fire_core.o module_fr_fire_model.o module_fr_fire_atm.o
	$(CPP) $(CPPFLAGS) $< > module_fr_fire_driver.f90
	$(FC) $(FCFLAGS) -c module_fr_fire_driver.f90

module_fr_fire_atm.o: module_fr_fire_atm.F wrf_atmosphere_mod.o module_fr_fire_util.o
	$(CPP) $(CPPFLAGS) $< > module_fr_fire_atm.f90
	$(FC) $(FCFLAGS) -c module_fr_fire_atm.f90

module_fr_fire_model.o: module_fr_fire_model.F wrf_atmosphere_mod.o module_fr_fire_core.o module_fr_fire_phys.o module_fr_fire_util.o
	$(CPP) $(CPPFLAGS) $< > module_fr_fire_model.f90
	$(FC) $(FCFLAGS) -c module_fr_fire_model.f90

module_fr_fire_core.o: module_fr_fire_core.F wrf_atmosphere_mod.o module_fr_fire_phys.o module_fr_fire_util.o
	$(CPP) $(CPPFLAGS) $< > module_fr_fire_core.f90
	$(FC) $(FCFLAGS) -c module_fr_fire_core.f90

module_fr_fire_phys.o: module_fr_fire_phys.F wrf_atmosphere_mod.o module_fr_fire_util.o
	$(CPP) $(CPPFLAGS) $< > module_fr_fire_phys.f90
	$(FC) $(FCFLAGS) -c module_fr_fire_phys.f90

module_fr_fire_util.o: module_fr_fire_util.F wrf_atmosphere_mod.o
	$(CPP) $(CPPFLAGS) $< > module_fr_fire_util.f90
	$(FC) $(FCFLAGS) -c module_fr_fire_util.f90


  # WRF-Atmosphere module
wrf_atmosphere_mod.o: wrf_atmosphere_mod.F
	$(CPP) $(CPPFLAGS) $< > wrf_atmosphere_mod.f90
	$(FC) $(FCFLAGS) -c wrf_atmosphere_mod.f90


clean:
	rm -f ./*.o ./*.exe ./*.mod  ./*.f90
