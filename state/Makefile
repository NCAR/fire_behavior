
objects = grid_mod.o

target: $(objects)
%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

