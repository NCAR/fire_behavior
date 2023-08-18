macro(set_fire_defaults)
  if(NOT NUOPC)
    set(NUOPC OFF)
  endif()
  if(NOT ESMX)
    set(ESMX OFF)
  endif()
endmacro()

function(set_fire_cache)
  set (NUOPC ${NUOPC} CACHE BOOL "Build NUOPC cap" FORCE)
  set (ESMX ${ESMX} CACHE BOOL "Build ESMX application" FORCE)
endfunction()

function(print_fire_settings)
  message (STATUS "FIRE BEHAVIOR BUILD SETTINGS\n"
                  "\tNUOPC: ${NUOPC}\n"
		  "\tESMX:  ${ESMX}")
endfunction()
