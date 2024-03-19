# fire_behavior

[![Fire Behavior Nightly Test (develop)](https://github.com/NCAR/fire_behavior/actions/workflows/develop_test.yml/badge.svg?branch=develop)](https://github.com/NCAR/fire_behavior/actions/workflows/develop_test.yml)

Developing a fire behavior model based on WRF-Fire for UFS

Load these modules to compile with make
module purge
module load ncarenv/1.3
module load gnu/10.1.0
module load ncarcompilers/0.5.0
module load netcdf/4.8.1
module load esmf_libs/8.2.0

UFS variables availabe to import:
https://github.com/NOAA-EMC/fv3atm/blob/develop/cpl/module_cplfields.F90#L32

Preloaded dictionary:
https://earthsystemmodeling.org/docs/release/latest/NUOPC_refdoc/node3.html#SECTION00032200000000000000

nems dictionary:
https://raw.githubusercontent.com/ufs-community/ufs-weather-model/develop/tests/parm/fd_nems.yaml
https://github.com/ufs-community/ufs-weather-model/blob/develop/tests/parm/fd_nems.yaml

To get the pointer after FieldCreate:
https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/node5.html#SECTION050364600000000000000

ESMF_FieldCreate - Create a Field from Grid and typekind
https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/node5.html#SECTION05036500000000000000

ESMF_FieldCreate - Create a Field from Grid and Fortran array:
https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/node5.html#SECTION05036800000000000000

ESMX.F90 program:
https://github.com/esmf-org/esmf/blob/develop/src/addon/ESMX/esmx.F90

ESMX_driver.F90:
https://github.com/esmf-org/esmf/blob/develop/src/addon/ESMX/Driver/ESMX_Driver.F90

UFS driver:
https://github.com/ufs-community/ufs-weather-model/blob/develop/driver/UFS.F90#L94

lines 673-676 show how to create a 3D field using a 2d grid (vertical levels):
https://github.com/NCAR/wrf_hydro_nwm_public/blob/master/src/CPL/NUOPC_cpl/WRFHydro_NUOPC_Fields.F90#L673

example of a "fake" model to check connectors:
https://github.com/esmf-org/parflow/blob/feature/NUOPC/ESMFAPP-6/pfnuopc/test/src/pf_nuopc_test_lnd.F90

Other coupling software:
https://escomp.github.io/CDEPS/html/index.html

WRF-Hydro:
https://github.com/esmf-org/wrf_hydro_nwm_public/tree/feature/nuopc-add-wtrflx/ESMFAPP-4/trunk/NDHMS/CPL/NUOPC_cpl

