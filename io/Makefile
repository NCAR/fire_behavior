
objects = netcdf_mod.o geogrid_mod.o namelist_mod.o namelist_wrf_mod.o

target: $(objects)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

  # Dependencies
geogrid_mod.o: netcdf_mod.o
