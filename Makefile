
FC=gfortran
FCFLAGS=-ffree-form -std=f2008
CPP=cpp
CPPFLAGS=-nostdinc -C -P -w

objects: wrf_atmosphere_mod.o module_fr_fire_util.o module_fr_fire_phys.o module_fr_fire_core.o \
	 module_fr_fire_model.o module_fr_fire_atm.o module_fr_fire_driver.o module_fr_fire_driver_wrf.o

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

wrf_atmosphere_mod.o: wrf_atmosphere_mod.F
	$(CPP) $(CPPFLAGS) $< > wrf_atmosphere_mod.f90
	$(FC) $(FCFLAGS) -c wrf_atmosphere_mod.f90

clean:
	rm -f ./*.o ./*.exe ./*.mod  ./*.f90
