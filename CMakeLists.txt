cmake_minimum_required(VERSION 3.18)

project(
  ufs_fire_behavior 
  VERSION 0.0.1
  LANGUAGES Fortran CXX)

# add user defined cmake modules
list(PREPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules/")

# add ESMF targets
if (NOT TARGET ESMF)
  find_package(ESMF MODULE REQUIRED)
  if (NOT ESMF_FOUND)
    message(FATAL_ERROR "ESMF library not found. Please set environment variable ESMFMKFILE.")
  endif (NOT ESMF_FOUND)
endif (NOT TARGET ESMF)

find_package(NetCDF REQUIRED Fortran)

#------------------------------------------------------------------------------
# Add source files

# State
list(APPEND _state_files state/state_mod.F90)

list(APPEND _share_files share/proj_lc_mod.F90
                         share/datetime_mod.F90
                         share/constants_mod.F90)

# IO
list(APPEND _io_files io/namelist_mod.F90
                      io/geogrid_mod.F90
                      io/wrf_mod.F90
		      io/netcdf_mod.F90)


# WRF Physics
list(APPEND _wrffire_physics physics/module_fr_fire_core.F
                             physics/module_fr_fire_driver.F
			     physics/module_fr_fire_model.F
			     physics/module_fr_fire_phys.F
			     physics/module_fr_fire_fuel_moisture_model.F
			     physics/module_fr_fire_fuel_anderson_mod.F
			     physics/module_fr_fire_util.F)

# Driver
list(APPEND _driver_files driver/advance_mod.F90
                          driver/initialize_mod.F90)
# note that driver/fire_behavior.F90 is binded below in add_executable(...
# that's because these lists will build the library firelib, which is 
# used by both, standalone and exmx.  

#------------------------------------------------------------------------------

set(CMAKE_Fortran_COMPILER mpif90)
# set(CMAKE_Fortran_COMPILER         "${ESMF_F90COMPILER}")
# set(CMAKE_Fortran_FLAGS            "${ESMF_F90COMPILEOPTS}")
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/mod")

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -fno-inline -no-ip -check all -fpe0 -ftrapuv -init=snan,arrays")
  set(CMAKE_Fortran_FLAGS "-ip -fp-model precise -w -ftz -align all -fno-alias -FR -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -fbacktrace -02")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -O0 -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow -finit-real=nan")
  set(CMAKE_Fortran_FLAGS "-std=f2008 -ffree-form -ffree-line-length-none -fall-intrinsics")
else()
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} Fortran compiler will be used with default options")
endif()

set(CMAKE_CPP_FLAGS "-nostdinc -C -P -w")

#------------------------------------------------------------------------------

# configure fire behavior library (used by standalone and esmx)
add_library(firelib STATIC ${_state_files} ${_share_files} ${_io_files} ${_wrffire_physics} ${_driver_files})
target_include_directories(firelib PUBLIC ${NetCDF_INCLUDE_DIRS})
target_link_libraries(firelib PUBLIC $<BUILD_INTERFACE:${NetCDF_LIBRARIES}>)

# configure fire behavior executable (standalone code)
add_executable(fire_behavior_standalone driver/fire_behavior.F90)
add_dependencies(fire_behavior_standalone firelib)
target_link_libraries(fire_behavior_standalone PUBLIC firelib)
target_link_libraries(fire_behavior_standalone PUBLIC NetCDF::NetCDF_Fortran)
install(TARGETS fire_behavior_standalone DESTINATION bin)

# configure fire behavior nuopc cap library (used with esmx)
add_library(fire_behavior_nuopc nuopc/fire_behavior_nuopc.F90)
add_dependencies(fire_behavior_nuopc firelib)
target_link_libraries(fire_behavior_nuopc PUBLIC firelib)
target_link_libraries(fire_behavior_nuopc PUBLIC ESMF)
target_include_directories(fire_behavior_nuopc INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                                         $<INSTALL_INTERFACE:mod>)

# install
install(TARGETS firelib fire_behavior_nuopc 
        EXPORT fire_behavior_nuopc 
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
	INCLUDES DESTINATION mod)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION ${CMAKE_INSTALL_PREFIX})
install(EXPORT fire_behavior_nuopc DESTINATION cmake)
