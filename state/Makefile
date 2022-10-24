
PHYSICS_DIR = ../physics
PHYSICS_INCLUDE="-I$(PHYSICS_DIR)"

IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

objects = state_mod.o

target: $(objects)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(PHYSICS_INCLUDE) $(IO_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

