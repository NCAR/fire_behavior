
IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

STATE_DIR = ../state
STATE_INCLUDE="-I$(STATE_DIR)"

DRIVER_DIR = ../driver
DRIVER_INCLUDE="-I$(DRIVER_DIR)"

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

esmApp.exe: esmApp.o esm.o cap_driver.o \
	../io/geogrid_mod.o ../io/netcdf_mod.o ../io/namelist_mod.o ../state/state_mod.o ../driver/initialize_mod.o ../driver/advance_mod.o \
	../physics/wrf_atmosphere_mod.o ../physics/module_fr_fire_driver.o ../physics/module_fr_fire_driver_wrf.o ../physics/module_fr_fire_util.o \
	../physics/module_fr_fire_atm.o ../physics/module_fr_fire_model.o ../physics/module_fr_fire_core.o ../physics/module_fr_fire_phys.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS) $(NETCDF_LIBS)

#objects = cap_driver.o esm.o
#target: $(objects)

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(FCFLAGS) $(ESMF_F90COMPILEOPTS) $(STATE_INCLUDE) $(IO_INCLUDE) $(DRIVER_INCLUDE) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<

clean:
	rm -f ./*.o ./*.mod ./*.exe

  # Dependencies
cap_driver.o: ../state/state_mod.o ../io/namelist_mod.o ../driver/initialize_mod.o ../driver/advance_mod.o
esmApp.o: esm.o
esm.o: cap_driver.o
