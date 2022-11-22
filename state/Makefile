
IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

objects = state_mod.o

target: $(objects)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(IO_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

  # Dependencies
state_mod.o: ../io/namelist_mod.o ../io/geogrid_mod.o
