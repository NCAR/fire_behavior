
IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

SHARE_DIR = ../share
SHARE_INCLUDE="-I$(SHARE_DIR)"

objects = state_mod.o

target: $(objects)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(IO_INCLUDE) $(SHARE_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

  # Dependencies
state_mod.o: ../share/datetime_mod.o ../io/namelist_mod.o ../io/geogrid_mod.o ../io/netcdf_mod.o ../io/wrf_mod.o ../share/proj_lc_mod.o ../share/constants_mod.o
