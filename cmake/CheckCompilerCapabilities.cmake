include (${CMAKE_SOURCE_DIR}/cmake/CheckFortranSource.cmake)

foreach (kind 8 16 32 64)
  set(CMAKE_REQUIRED_FLAGS = -fpp)
  set(CMAKE_REQUIRED_DEFINITIONS -D_KIND=INT${kind})

  CHECK_FORTRAN_SOURCE_COMPILE (
    ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/HAS_INT_KIND.F90
    _HAS_INT${kind}
    )

endforeach()

foreach (kind 32 64 128)
  set(CMAKE_REQUIRED_FLAGS = -fpp)
  set(CMAKE_REQUIRED_DEFINITIONS -D_KIND=REAL${kind})

  CHECK_FORTRAN_SOURCE_COMPILE (
    ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/HAS_REAL_KIND.F90
    _HAS_REAL${kind}
    )
  
endforeach()



