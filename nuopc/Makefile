
IO_DIR = ../io
IO_INCLUDE="-I$(IO_DIR)"

STATE_DIR = ../state
STATE_INCLUDE="-I$(STATE_DIR)"

PHYSICS_DIR = ../physics
PHYSICS_INCLUDE="-I$(PHYSICS_DIR)"

DRIVER_DIR = ../driver
DRIVER_INCLUDE="-I$(DRIVER_DIR)"

SHARE_DIR = ../share
SHARE_INCLUDE="-I$(SHARE_DIR)"

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

esmApp.exe: esmApp.o esm.o fire_behavior_nuopc.o wrf_nuopc.o ../share/proj_lc_mod.o ../share/datetime_mod.o ../share/constants_mod.o \
	../io/geogrid_mod.o ../io/wrf_mod.o ../io/netcdf_mod.o ../io/namelist_mod.o ../state/state_mod.o ../driver/initialize_mod.o ../driver/advance_mod.o \
	../physics/module_fr_fire_driver.o ../physics/module_fr_fire_util.o \
	../physics/module_fr_fire_model.o ../physics/module_fr_fire_core.o ../physics/module_fr_fire_phys.o \
        ../physics/module_fr_fire_fuel_moisture_model.o ../physics/module_fr_fire_fuel_anderson_mod.o \
	../physics/ignition_line_mod.o ../physics/ros_wrffire_mod.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS) $(NETCDF_LIBS)

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(FCFLAGS) $(ESMF_F90COMPILEOPTS) $(STATE_INCLUDE) $(IO_INCLUDE) $(PHYSICS_INCLUDE) $(DRIVER_INCLUDE) $(SHARE_INCLUDE) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<

clean:
	rm -f ./*.o ./*.mod ./*.exe

  # Dependencies
fire_behavior_nuopc.o: ../state/state_mod.o ../io/namelist_mod.o ../driver/initialize_mod.o ../driver/advance_mod.o

wrf_nuopc.o: ../io/wrf_mod.o ../share/datetime_mod.o ../io/namelist_mod.o ../driver/initialize_mod.o

esmApp.o: esm.o

esm.o: fire_behavior_nuopc.o wrf_nuopc.o ../io/namelist_mod.o

