cmake_minimum_required(VERSION 3.10)

project(
  ufs_fire_behavior 
  VERSION 0.0.1
  LANGUAGES Fortran CXX)

# add user defined cmake modules
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules/")
set(CMAKE_Fortran_COMPILER mpif90)

# add ESMF targets
if (NOT TARGET ESMF)
  find_package(ESMF MODULE REQUIRED)
  if (NOT ESMF_FOUND)
    message(FATAL_ERROR "ESMF library not found. Please set environment variable ESMFMKFILE.")
  endif (NOT ESMF_FOUND)
endif (NOT TARGET ESMF)

# find_package(NetCDF REQUIRED)

#------------------------------------------------------------------------------
# Add source files

# State
list(APPEND _state_files state/state_mod.F90)

# IO
list(APPEND _io_files io/namelist_mod.F90
                      io/geogrid_mod.F90
		      io/netcdf_mod.F90)

# WRF Physics
list(APPEND _wrffire_physics physics/module_fr_fire_atm.F
	    		     physics/module_fr_fire_core.F
			     physics/module_fr_fire_driver.F
			     physics/module_fr_fire_driver_wrf.F
			     physics/module_fr_fire_model.F
			     physics/module_fr_fire_phys.F
			     physics/module_fr_fire_util.F
			     physics/wrf_atmosphere_mod.F)

# Driver
list(APPEND _driver_files driver/advance_mod.F90
                          driver/initialize_mod.F90)

# Tests 
# list(APPEND _test_files tests/wrf_fire_test4_mod.F90
# 			tests/wrf_fire_test3_mod.F90
#                         tests/wrf_fire_test2_mod.F90
# 			tests/wrf_fire_test1_mod.F90)


#------------------------------------------------------------------------------

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS_OPT "-ip -fp-model precise -w -ftz -align all -fno-alias -FR -convert big_endian")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fno-inline -no-ip -traceback")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_OPT} ${CMAKE_Fortran_FLAGS_DEBUG}")
# elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
#   set(CMAKE_Fortran_FLAGS_OPT "-std=f2008 -ffree-form -fbacktrace -fcheck=all -finit-real=nan -fall-intrinsics -Wall -g -Og")
#   set(CMAKE_Fortran_FLAGS_DEBUG "")
#   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_OPT} ${CMAKE_Fortran_FLAGS_DEBUG}")

else()
  message(WARNING "Fortran compiler with ID ${CMAKE_Fortran_COMPILER_ID} will be used with CMake default options")
endif()

set(CMAKE_CPP_FLAGS "-nostdinc -C -P -w")

#------------------------------------------------------------------------------

# configure fire behavior library
add_library(firelib STATIC ${_state_files} ${_io_files} ${_wrffire_physics} ${_driver_files})

# configure fire behavior nuopc cap library
add_library(fire_behavior_nuopc cap_driver.F90)
add_dependencies(fire_behavior_nuopc firelib)
target_link_libraries(fire_behavior_nuopc PUBLIC firelib)
target_link_libraries(fire_behavior_nuopc PUBLIC ESMF)

# configure fire behavior executable
add_executable(fire_behavior_standalone driver/fire_behavior.F90)
add_dependencies(fire_behavior_standalone firelib)
target_link_libraries(fire_behavior_standalone PUBLIC firelib)

# install
install(TARGETS firelib fire_behavior_nuopc DESTINATION lib)
install(TARGETS fire_behavior_standalone DESTINATION bin)

