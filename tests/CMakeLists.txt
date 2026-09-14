cmake_minimum_required(VERSION 3.20)
enable_testing()

add_subdirectory(unit)

# Register isolated standalone quick cases through the Python harness. Each
# invocation stages inputs, runs the selected executable, and reports comparison
# results in a new attempt directory beneath the configured artifact root.
if(TARGET fire_behavior.exe AND NOT NUOPC AND NOT ESMX)
  find_package(Python3 REQUIRED COMPONENTS Interpreter)
  execute_process(
    COMMAND ${Python3_EXECUTABLE} -c "import numpy, netCDF4, yaml"
    RESULT_VARIABLE CFBM_PYTHON_IMPORT_STATUS
    ERROR_VARIABLE CFBM_PYTHON_IMPORT_ERROR
  )
  if(NOT CFBM_PYTHON_IMPORT_STATUS EQUAL 0)
    message(FATAL_ERROR "CFBM regression Python dependencies are unavailable: ${CFBM_PYTHON_IMPORT_ERROR}")
  endif()

  set(CFBM_REGRESSION_CONFIG "${CMAKE_CURRENT_SOURCE_DIR}/regression/cases.yaml"
      CACHE FILEPATH "CFBM regression YAML configuration")
  set(CFBM_REGRESSION_BASELINE "" CACHE PATH
      "Approved CFBM immutable baseline root; empty requires CFBM_BASELINE_ROOT or approved.yaml")
  set(CFBM_REGRESSION_ATTEMPT_ROOT "${CMAKE_CURRENT_BINARY_DIR}/cfbm-regression-runs"
      CACHE PATH "Parent directory for isolated CFBM CTest attempts")
  set(CFBM_REGRESSION_EXECUTABLE "$<TARGET_FILE:fire_behavior.exe>"
      CACHE STRING "Explicit CFBM executable for direct CMake testing")

  if(DM_PARALLEL)
    set(_cfbm_execution mpi4)
    set(_cfbm_variant mpi)
    set(_cfbm_processors 4)
    set(_cfbm_launcher_args
        "--launcher-arg=${MPIEXEC_EXECUTABLE}"
        "--process-count-flag=${MPIEXEC_NUMPROC_FLAG}")
    foreach(_flag IN LISTS MPIEXEC_PREFLAGS)
      list(APPEND _cfbm_launcher_args "--launcher-arg=${_flag}")
    endforeach()
    foreach(_flag IN LISTS MPIEXEC_POSTFLAGS)
      list(APPEND _cfbm_launcher_args "--launcher-post-arg=${_flag}")
    endforeach()
  elseif(OPENMP)
    set(_cfbm_execution omp4)
    set(_cfbm_variant omp)
    set(_cfbm_processors 4)
    set(_cfbm_launcher_args "")
  else()
    set(_cfbm_execution serial)
    set(_cfbm_variant serial)
    set(_cfbm_processors 1)
    set(_cfbm_launcher_args "")
  endif()

  foreach(_case IN ITEMS circle_nowind fuel_strip_wind terrain_fuel_fmc_wind)
    if(NOT _cfbm_execution STREQUAL serial AND NOT _case STREQUAL terrain_fuel_fmc_wind)
      continue()
    endif()
    if(_case STREQUAL circle_nowind)
      set(_cfbm_feature point_bilinear_10m)
    elseif(_case STREQUAL fuel_strip_wind)
      set(_cfbm_feature line_nearest_10m)
    else()
      set(_cfbm_feature perimeter_bilinear_10m)
    endif()
    set(_test_name "cfbm_quick_${_case}_${_cfbm_execution}")
    set(_cfbm_command
        ${Python3_EXECUTABLE} -B
        ${CMAKE_CURRENT_SOURCE_DIR}/regression/regression.py case
        --config ${CFBM_REGRESSION_CONFIG}
        --case ${_case}
        --profile quick
        --method ref94
        --feature ${_cfbm_feature}
        --execution ${_cfbm_execution}
        --executable ${CFBM_REGRESSION_EXECUTABLE}
        --attempt-parent ${CFBM_REGRESSION_ATTEMPT_ROOT}/${_cfbm_execution}/${_case}/quick/ref94
        ${_cfbm_launcher_args})
    if(CFBM_REGRESSION_BASELINE)
      list(APPEND _cfbm_command --baseline-root ${CFBM_REGRESSION_BASELINE})
    endif()
    add_test(
      NAME ${_test_name}
      COMMAND ${_cfbm_command}
    )
    set_tests_properties(${_test_name} PROPERTIES
      LABELS "quick;pr;${_case};${_cfbm_variant};${CMAKE_BUILD_TYPE};${_cfbm_execution};reference"
      PROCESSORS ${_cfbm_processors}
      TIMEOUT 1200
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
  endforeach()
endif()

if(NUOPC)
  add_test(NAME test7esmf
           COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test7esmf.s
           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  add_test(NAME test8esmf
           COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test8esmf.s
           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
endif()

if(ESMX)
  add_test(NAME test7esmx
           COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test7esmx.s
           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  add_test(NAME test8esmx
           COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test8esmx.s
           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
  add_test(NAME testx
           COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/testx.s
           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
endif()
