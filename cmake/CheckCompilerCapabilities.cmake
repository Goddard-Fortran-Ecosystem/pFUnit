include (${CMAKE_SOURCE_DIR}/cmake/CheckFortranSource.cmake)

CHECK_FORTRAN_SOURCE_RUN (
  ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/INT_DEFAULT_KIND.F90
  _INT_DEFAULT_KIND
  )
foreach (kind 8 16 32 64)
  set(CMAKE_REQUIRED_FLAGS = -fpp)
  set(CMAKE_REQUIRED_DEFINITIONS -D_KIND=INT${kind})

  CHECK_FORTRAN_SOURCE_RUN (
    ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/INT_KIND.F90
    _INT${kind}
    )

endforeach()

CHECK_FORTRAN_SOURCE_RUN (
  ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/REAL_DEFAULT_KIND.F90
  _REAL_DEFAULT_KIND
  )

foreach (kind 32 64 128)
  set(CMAKE_REQUIRED_FLAGS = -fpp)
  set(CMAKE_REQUIRED_DEFINITIONS -D_KIND=REAL${kind})

  CHECK_FORTRAN_SOURCE_RUN (
    ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/REAL_KIND.F90
    _REAL${kind}
    )
  
endforeach()



