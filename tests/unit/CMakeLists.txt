# Keep each test's command, labels, and expected diagnostic together.
function(add_cfbm_unit_test)
  cmake_parse_arguments(PARSE_ARGV 0 test "" "NAME;EXPECT" "COMMAND;LABELS")
  add_test(NAME ${test_NAME} COMMAND ${test_COMMAND})
  set_tests_properties(${test_NAME} PROPERTIES LABELS "unit;quick;pr;full;${test_LABELS}")
  if(test_EXPECT)
    # Serial Stop_simulation uses a plain Fortran STOP, which can exit with
    # status zero. Check the expected rejection diagnostic rather than use
    # WILL_FAIL, which requires a nonzero exit status.
    set_tests_properties(${test_NAME} PROPERTIES PASS_REGULAR_EXPRESSION "${test_EXPECT}")
  else()
    # An unexpected serial STOP must not count as successful completion.
    set_tests_properties(${test_NAME} PROPERTIES FAIL_REGULAR_EXPRESSION "STOP:")
  endif()
endfunction()

foreach(test IN ITEMS tiles_unit tile_namelist_unit observed_perimeter_unit
                      namelist_validation_unit time_interval_unit fire_clock_unit
                      coordinate_precision_unit projection_precision_unit bilinear_precision_unit
                      fuel_precision_unit wind_order_unit wind_state_unit fmc_initialization_unit)
  add_executable(${test} ${test}.F90)
  target_link_libraries(${test} PRIVATE firelib)
endforeach()

add_executable(moisture_extinction_unit moisture_extinction_unit.F90)
target_link_libraries(moisture_extinction_unit PRIVATE firelib)
foreach(scenario IN ITEMS below edge at above)
  add_cfbm_unit_test(NAME moisture_extinction_${scenario}_unit
    COMMAND moisture_extinction_unit ${scenario} LABELS fuel moisture ros
    EXPECT "moisture extinction checks passed: ${scenario}")
  if(OPENMP)
    set_tests_properties(moisture_extinction_${scenario}_unit PROPERTIES
      ENVIRONMENT "OMP_NUM_THREADS=2;OMP_DYNAMIC=FALSE")
  endif()
endforeach()
add_cfbm_unit_test(NAME moisture_extinction_negative_moisture_unit
  COMMAND moisture_extinction_unit negative_moisture LABELS fuel moisture ros
  EXPECT "STOP:Restart fmc_g must be nonnegative")
foreach(scenario IN ITEMS nan_moisture inf_moisture neginf_moisture)
  add_cfbm_unit_test(NAME moisture_extinction_${scenario}_unit
    COMMAND moisture_extinction_unit ${scenario} LABELS fuel moisture ros
    EXPECT "STOP:Restart fmc_g must be finite")
endforeach()
foreach(scenario IN ITEMS zero_extinction negative_extinction)
  add_cfbm_unit_test(NAME moisture_extinction_${scenario}_unit
    COMMAND moisture_extinction_unit ${scenario} LABELS fuel moisture ros
    EXPECT "STOP:fuelmce must be positive")
endforeach()
foreach(scenario IN ITEMS nan_extinction inf_extinction neginf_extinction)
  add_cfbm_unit_test(NAME moisture_extinction_${scenario}_unit
    COMMAND moisture_extinction_unit ${scenario} LABELS fuel moisture ros
    EXPECT "STOP:fuelmce must be finite")
endforeach()

add_cfbm_unit_test(NAME namelist_fuelmc_zero_unit
  COMMAND namelist_validation_unit fuelmc_zero LABELS namelist moisture
  EXPECT "namelist validation accepted: fuelmc_zero")
add_cfbm_unit_test(NAME namelist_fuelmc_negative_unit
  COMMAND namelist_validation_unit fuelmc_negative LABELS namelist moisture
  EXPECT "STOP:fuelmc_g must be nonnegative")
foreach(scenario IN ITEMS nan inf neginf)
  add_cfbm_unit_test(NAME namelist_fuelmc_${scenario}_unit
    COMMAND namelist_validation_unit fuelmc_${scenario} LABELS namelist moisture
    EXPECT "STOP:fuelmc_g must be finite")
endforeach()

add_executable(fmc_validation_unit fmc_validation_unit.F90)
target_link_libraries(fmc_validation_unit PRIVATE firelib)
foreach(scenario IN ITEMS valid zero_load restored_zero restored_wet)
  add_cfbm_unit_test(NAME fmc_validation_${scenario}_unit
    COMMAND fmc_validation_unit ${scenario} LABELS fuel moisture
    EXPECT "moisture validation checks passed: ${scenario}")
endforeach()
foreach(quantity IN ITEMS class restored)
  foreach(scenario IN ITEMS negative nan inf neginf)
    if(quantity STREQUAL "class")
      set(field fmc_gc)
      set(source Computed)
    else()
      set(field fmc_g)
      set(source Restart)
    endif()
    if(scenario STREQUAL "negative")
      set(requirement nonnegative)
    else()
      set(requirement finite)
    endif()
    add_cfbm_unit_test(NAME fmc_validation_${quantity}_${scenario}_unit
      COMMAND fmc_validation_unit ${quantity}_${scenario} LABELS fuel moisture
      EXPECT "STOP:${source} ${field} must be ${requirement}")
  endforeach()
endforeach()
add_cfbm_unit_test(NAME fmc_validation_load_negative_unit
  COMMAND fmc_validation_unit load_negative LABELS fuel moisture
  EXPECT "STOP:.*fuel load in category.*must be nonegative")
foreach(scenario IN ITEMS nan inf neginf)
  add_cfbm_unit_test(NAME fmc_validation_load_${scenario}_unit
    COMMAND fmc_validation_unit load_${scenario} LABELS fuel moisture
    EXPECT "STOP:Moisture averaging fuel load must be finite for category,class:")
endforeach()
add_cfbm_unit_test(NAME fmc_validation_load_overflow_unit
  COMMAND fmc_validation_unit load_overflow LABELS fuel moisture
  EXPECT "STOP:Moisture averaging total fuel load exceeds representable range for category:")
if(OPENMP)
  get_property(fmc_tests DIRECTORY PROPERTY TESTS)
  list(FILTER fmc_tests INCLUDE REGEX "^fmc_validation_")
  set_tests_properties(${fmc_tests} PROPERTIES ENVIRONMENT "OMP_NUM_THREADS=2;OMP_DYNAMIC=FALSE")
endif()

# Check the production wind routine's array bounds even in Release builds.
# Keep its test module separate from firelib's normal compilation products.
target_sources(wind_order_unit PRIVATE ${PROJECT_SOURCE_DIR}/io/coupling_mod.F90)
set_target_properties(wind_order_unit PROPERTIES
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/wind_order_modules")
target_include_directories(wind_order_unit PRIVATE
  "${CMAKE_CURRENT_BINARY_DIR}/wind_order_modules" "${CMAKE_Fortran_MODULE_DIRECTORY}")
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  target_compile_options(wind_order_unit PRIVATE -fcheck=bounds)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  target_compile_options(wind_order_unit PRIVATE -check bounds)
endif()

# Atmospheric initialization validates timing without requiring an ignition.
add_cfbm_unit_test(NAME namelist_atm_only_unit
  COMMAND namelist_validation_unit atm_only LABELS namelist timing
  EXPECT "namelist validation accepted: atm_only")
add_cfbm_unit_test(NAME namelist_atm_nonmultiple_unit
  COMMAND namelist_validation_unit atm_nonmultiple LABELS namelist timing
  EXPECT "STOP:Time intervals must be integer multiples of dt")
add_cfbm_unit_test(NAME namelist_atm_missing_unit
  COMMAND namelist_validation_unit atm_missing LABELS namelist timing
  EXPECT "STOP:interval_atm must be positive")
add_cfbm_unit_test(NAME namelist_atm_zero_dt_unit
  COMMAND namelist_validation_unit atm_zero_dt LABELS namelist timing
  EXPECT "STOP:dt must be finite and positive")

add_cfbm_unit_test(NAME tiles_unit COMMAND tiles_unit LABELS tiles)
add_cfbm_unit_test(NAME tile_namelist_unit COMMAND tile_namelist_unit LABELS tiles namelist)
add_cfbm_unit_test(NAME observed_perimeter_unit COMMAND observed_perimeter_unit LABELS perimeter)
if(OPENMP)
  set_tests_properties(tiles_unit PROPERTIES ENVIRONMENT "OMP_NUM_THREADS=4;OMP_DYNAMIC=FALSE")
endif()

if(DM_PARALLEL)
  add_executable(namelist_broadcast_mpi namelist_broadcast_mpi.F90)
  target_link_libraries(namelist_broadcast_mpi PRIVATE firelib MPI::MPI_Fortran)
  add_cfbm_unit_test(NAME namelist_broadcast_mpi LABELS namelist mpi
    COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4
            ${MPIEXEC_PREFLAGS} $<TARGET_FILE:namelist_broadcast_mpi> ${MPIEXEC_POSTFLAGS})
endif()

foreach(scenario IN ITEMS aligned wrf_unset ideal_unset boundary_time boundary_above boundary_below line_interior)
  add_cfbm_unit_test(NAME namelist_${scenario}_unit
    COMMAND namelist_validation_unit ${scenario} LABELS namelist)
endforeach()
foreach(scenario IN ITEMS reversed nonmultiple)
  add_cfbm_unit_test(NAME namelist_${scenario}_unit
    COMMAND namelist_validation_unit ${scenario} LABELS namelist
    EXPECT "STOP:Time intervals must be integer multiples of dt")
endforeach()
foreach(scenario IN ITEMS zero_count negative_count)
  add_cfbm_unit_test(NAME namelist_${scenario}_unit
    COMMAND namelist_validation_unit ${scenario} LABELS namelist
    EXPECT "STOP:line-ignition mode requires at least one ignition")
endforeach()
add_cfbm_unit_test(NAME namelist_zero_dt_unit
  COMMAND namelist_validation_unit zero_dt LABELS namelist
  EXPECT "STOP:dt must be finite and positive")
add_cfbm_unit_test(NAME namelist_missing_atm_unit
  COMMAND namelist_validation_unit missing_atm LABELS namelist
  EXPECT "STOP:interval_atm must be positive")
add_cfbm_unit_test(NAME namelist_excess_count_unit
  COMMAND namelist_validation_unit excess_count LABELS namelist
  EXPECT "STOP:fire_num_ignitions exceeds")
add_cfbm_unit_test(NAME namelist_perimeter_count_unit
  COMMAND namelist_validation_unit perimeter_count LABELS namelist
  EXPECT "STOP:perimeter mode requires one ignition")
add_cfbm_unit_test(NAME namelist_negative_time_unit
  COMMAND namelist_validation_unit negative_time LABELS namelist
  EXPECT "STOP:perimeter ignition time must be finite and nonnegative")
add_cfbm_unit_test(NAME namelist_interior_time_unit
  COMMAND namelist_validation_unit interior_time LABELS namelist
  EXPECT "STOP:perimeter ignition time must align")

add_cfbm_unit_test(NAME time_interval_unit COMMAND time_interval_unit first LABELS timing
  EXPECT "advance behavior passed: first")
add_cfbm_unit_test(NAME time_interval_invalid_unit
  COMMAND time_interval_unit invalid LABELS timing
  EXPECT "STOP:Fire advance requires itimestep >= 1")
foreach(scenario IN ITEMS perimeter perimeter_above perimeter_below perimeter_fractional)
  add_cfbm_unit_test(NAME ${scenario}_advance_unit
    COMMAND time_interval_unit ${scenario} LABELS perimeter timing
    EXPECT "advance behavior passed: ${scenario}")
endforeach()

# Both legacy float coordinates and double coordinates must survive the reader.
add_cfbm_unit_test(NAME coordinate_precision_unit COMMAND coordinate_precision_unit
  LABELS coordinates io EXPECT "coordinate precision checks passed")

add_cfbm_unit_test(NAME projection_precision_unit COMMAND projection_precision_unit
  LABELS coordinates projection EXPECT "projection precision checks passed")

add_cfbm_unit_test(NAME bilinear_precision_unit COMMAND bilinear_precision_unit
  LABELS interpolation EXPECT "bilinear precision checks passed")

add_cfbm_unit_test(NAME fuel_precision_unit COMMAND fuel_precision_unit
  LABELS fuel EXPECT "fuel precision checks passed")

add_cfbm_unit_test(NAME wind_order_unit COMMAND wind_order_unit
  LABELS wind interpolation EXPECT "horizontal-first wind checks passed")

add_cfbm_unit_test(NAME wind_state_10m_unit COMMAND wind_state_unit u10m
  LABELS wind interpolation EXPECT "state wind checks passed")
add_cfbm_unit_test(NAME wind_state_invalid_unit COMMAND wind_state_unit invalid
  LABELS wind interpolation EXPECT "STOP:Error: wrong wind_vinterp_opt")

add_cfbm_unit_test(NAME fire_clock_unit COMMAND fire_clock_unit valid
  LABELS timing EXPECT "fire clock checks passed")
add_cfbm_unit_test(NAME fire_clock_unaligned_unit COMMAND fire_clock_unit unaligned
  LABELS timing EXPECT "STOP:Time intervals must be integer multiples of dt")
add_cfbm_unit_test(NAME fire_clock_final_unit COMMAND fire_clock_unit final
  LABELS timing EXPECT "STOP:Final time must align with a fire timestep")
add_cfbm_unit_test(NAME fire_clock_missed_unit COMMAND fire_clock_unit missed
  LABELS timing EXPECT "STOP:Missed scheduled fire event")

foreach(scenario IN ITEMS each delayed time late_first_call)
  add_cfbm_unit_test(NAME fmc_${scenario}_unit COMMAND fmc_initialization_unit ${scenario}
    LABELS moisture restart EXPECT "moisture initialization checks passed")
  if(OPENMP)
    set_tests_properties(fmc_${scenario}_unit PROPERTIES ENVIRONMENT "OMP_NUM_THREADS=4;OMP_DYNAMIC=FALSE")
  endif()
endforeach()
foreach(scenario IN ITEMS temperature pressure humidity rain)
  add_cfbm_unit_test(NAME fmc_${scenario}_unit COMMAND fmc_initialization_unit ${scenario}
    LABELS moisture restart EXPECT "STOP:FMC requires positive temperature and pressure and nonnegative humidity and rain")
endforeach()
foreach(scenario IN ITEMS nan old_forcing)
  add_cfbm_unit_test(NAME fmc_${scenario}_unit COMMAND fmc_initialization_unit ${scenario}
    LABELS moisture restart EXPECT "STOP:FMC requires finite atmospheric forcing")
endforeach()
add_cfbm_unit_test(NAME fmc_missing_unit COMMAND fmc_initialization_unit missing
  LABELS moisture restart EXPECT "STOP:FMC atmospheric forcing is unavailable")

add_cfbm_unit_test(NAME fmc_duplicate_unit COMMAND fmc_initialization_unit duplicate
  LABELS moisture restart EXPECT "STOP:FMC advance requires increasing moisture time")
