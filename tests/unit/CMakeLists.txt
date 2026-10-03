# Keep each test's command, labels, and expected diagnostic together.
function(add_cfbm_unit_test)
  cmake_parse_arguments(PARSE_ARGV 0 test "" "NAME;EXPECT" "COMMAND;LABELS")
  add_test(NAME ${test_NAME} COMMAND ${test_COMMAND})
  set_tests_properties(${test_NAME} PROPERTIES LABELS "unit;${test_LABELS}")
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
                      namelist_validation_unit time_interval_unit)
  add_executable(${test} ${test}.F90)
  target_link_libraries(${test} PRIVATE firelib)
endforeach()

# Atmospheric initialization validates timing without requiring an ignition.
add_cfbm_unit_test(NAME namelist_atm_only_unit
  COMMAND namelist_validation_unit atm_only LABELS namelist timing
  EXPECT "namelist validation accepted: atm_only")
add_cfbm_unit_test(NAME namelist_atm_nonmultiple_unit
  COMMAND namelist_validation_unit atm_nonmultiple LABELS namelist timing
  EXPECT "STOP:interval_atm must be an integer multiple of dt")
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
    EXPECT "STOP:interval_atm must be an integer multiple of dt")
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
