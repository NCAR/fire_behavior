
STATE_DIR = ../state
STATE_INCLUDE="-I$(STATE_DIR)"

objects = netcdf_mod.o geogrid_mod.o

target: $(objects)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(STATE_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

  # Dependencies
geogrid_mod.o: netcdf_mod.o ../state/state_mod.o
