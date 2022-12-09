
objects = datetime_mod.o proj_lc_mod.o

target: $(objects)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(NETCDF_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

