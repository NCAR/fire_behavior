
IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

STATE_DIR = ../state
STATE_INCLUDE="-I$(STATE_DIR)"

SHARE_DIR = ../share
SHARE_INCLUDE="-I$(SHARE_DIR)"

objects = module_fr_fire_util.o module_fr_fire_phys.o module_fr_fire_core.o \
	 module_fr_fire_model.o module_fr_fire_driver.o module_fr_fire_util.o \
         module_fr_fire_fuel_moisture_model.o module_fr_fire_fuel_anderson_mod.o

target: $(objects)
%.o: %.F
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(IO_INCLUDE) $(STATE_INCLUDE) $(SHARE_INCLUDE) $(FCFLAGS) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

  # Dependencies
module_fr_fire_driver.o: module_fr_fire_util.o module_fr_fire_phys.o module_fr_fire_core.o module_fr_fire_model.o \
	module_fr_fire_fuel_moisture_model.o module_fr_fire_fuel_anderson_mod.o ../io/namelist_mod.o ../state/state_mod.o

module_fr_fire_model.o: module_fr_fire_core.o module_fr_fire_phys.o module_fr_fire_util.o

module_fr_fire_core.o: module_fr_fire_phys.o module_fr_fire_util.o

module_fr_fire_phys.o: module_fr_fire_fuel_moisture_model.o module_fr_fire_util.o ../share/constants_mod.o \
        module_fr_fire_fuel_anderson_mod.o

module_fr_fire_fuel_moisture_model.o: module_fr_fire_fuel_anderson_mod.o module_fr_fire_util.o

