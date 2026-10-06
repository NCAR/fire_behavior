# Keep historical cases separate from generated regression cases.
set(_legacy test7 test8)
if(NUOPC)
  list(APPEND _legacy test7esmf test8esmf)
endif()
if(ESMX)
  list(APPEND _legacy test7esmx test8esmx testx)
endif()
foreach(_test IN LISTS _legacy)
  add_test(NAME ${_test} COMMAND bash "${CMAKE_CURRENT_SOURCE_DIR}/run_legacy.sh"
    ${_test} "${CMAKE_INSTALL_PREFIX}" "${PROJECT_BINARY_DIR}/legacy-runs")
  set_tests_properties(${_test} PROPERTIES LABELS legacy TIMEOUT 1200)
endforeach()
