# Keep each test's command and labels together.
function(add_cfbm_unit_test)
  cmake_parse_arguments(PARSE_ARGV 0 test "" "NAME" "COMMAND;LABELS")
  add_test(NAME ${test_NAME} COMMAND ${test_COMMAND})
  set_tests_properties(${test_NAME} PROPERTIES LABELS "unit;${test_LABELS}")
endfunction()

foreach(test IN ITEMS tiles_unit tile_namelist_unit)
  add_executable(${test} ${test}.F90)
  target_link_libraries(${test} PRIVATE firelib)
endforeach()

add_cfbm_unit_test(NAME tiles_unit COMMAND tiles_unit LABELS tiles)
add_cfbm_unit_test(NAME tile_namelist_unit COMMAND tile_namelist_unit LABELS tiles namelist)
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
