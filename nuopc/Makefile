
IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

STATE_DIR = ../state
STATE_INCLUDE="-I$(STATE_DIR)"

DRIVER_DIR = ../driver
DRIVER_INCLUDE="-I$(DRIVER_DIR)"

objects = cap_driver.o

target: $(objects)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(STATE_INCLUDE) $(IO_INCLUDE) $(DRIVER_INCLUDE) $(ESMF_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

  # Dependencies
cap_driver.o: ../state/state_mod.o ../io/namelist_mod.o ../driver/initialize_mod.o ../driver/advance_mod.o
