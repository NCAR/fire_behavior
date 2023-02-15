
SHARE_DIR = ../share
SHARE_INCLUDE="-I$(SHARE_DIR)"

objects = netcdf_mod.o geogrid_mod.o namelist_mod.o wrf_mod.o

target: $(objects)

%.o: %.F90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(FC) $(FCFLAGS) $(NETCDF_INCLUDE) $(SHARE_INCLUDE) -c $*.f90

clean:
	rm -f ./*.o ./*.mod  ./*.f90

  # Dependencies
geogrid_mod.o: netcdf_mod.o

wrf_mod.o: ../share/datetime_mod.o ../share/proj_lc_mod.o ../share/constants_mod.o netcdf_mod.o geogrid_mod.o namelist_mod.o
