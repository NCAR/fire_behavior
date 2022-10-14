
PHYSICS_DIR = ../physics
PHYSICS_INCLUDE="-I$(PHYSICS_DIR)"

all: fire_behavior.exe

objects = fire_behavior.o wrf_fire_test1_mod.o wrf_fire_test2_mod.o wrf_fire_test3_mod.o
#target: $(objects)

fire_behavior.exe: fire_behavior.o ../physics/wrf_atmosphere_mod.o wrf_fire_test1_mod.o wrf_fire_test2_mod.o wrf_fire_test3_mod.o \
	../physics/module_fr_fire_driver.o ../physics/module_fr_fire_driver_wrf.o ../physics/module_fr_fire_util.o \
	../physics/module_fr_fire_atm.o ../physics/module_fr_fire_model.o ../physics/module_fr_fire_core.o ../physics/module_fr_fire_phys.o
	$(FC) -o $@ $^

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(PHYSICS_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.exe ./*.mod  ./*.f90

  # Dependencies
fire_behavior.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_driver_wrf.o \
	wrf_fire_test1_mod.o wrf_fire_test2_mod.o wrf_fire_test3_mod.o

wrf_fire_test1_mod.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_util.o

wrf_fire_test2_mod.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_util.o

wrf_fire_test3_mod.o: ../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_util.o

