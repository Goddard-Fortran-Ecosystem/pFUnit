include (${CMAKE_SOURCE_DIR}/cmake/CheckFortranSource.cmake)

CHECK_FORTRAN_SOURCE_RUN (
  ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/LOGICAL_DEFAULT_KIND.F90
  _LOGICAL_DEFAULT_KIND
  )

CHECK_FORTRAN_SOURCE_RUN (
  ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/INT_DEFAULT_KIND.F90
  _INT_DEFAULT_KIND
  )
foreach (kind 8 16 32 64)
  set(CMAKE_REQUIRED_FLAGS = -fpp)
  set(CMAKE_REQUIRED_DEFINITIONS -D_KIND=INT${kind})

  CHECK_FORTRAN_SOURCE_RUN (
    ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/INT_KIND.F90
    _ISO_INT${kind}
    )

endforeach()

CHECK_FORTRAN_SOURCE_RUN (
  ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/REAL_DEFAULT_KIND.F90
  _REAL_DEFAULT_KIND
  )

CHECK_FORTRAN_SOURCE_RUN (
  ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/DOUBLE_DEFAULT_KIND.F90
  _DOUBLE_DEFAULT_KIND
  )

foreach (kind 32 64 80 128 256)
  set(CMAKE_REQUIRED_FLAGS = -fpp)
  set(CMAKE_REQUIRED_DEFINITIONS -D_KIND=REAL${kind})

  try_compile (
    code_compiles
    ${CMAKE_BINARY_DIR}
    ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/REAL_KIND.F90
    CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}")
  
  if (code_compiles)
    CHECK_FORTRAN_SOURCE_RUN (
      ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/REAL_KIND.F90
      _ISO_REAL${kind}
      )
  endif ()
  
  try_compile (
    code_compiles
    ${CMAKE_BINARY_DIR}
    ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/REAL_KIND.F90
    CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}")
  
  if (code_compiles)
    CHECK_FORTRAN_SOURCE_RUN(
      ${CMAKE_SOURCE_DIR}/cmake/Trial_sources/REAL_KIND_IEEE_SUPPORT.F90
      _REAL${kind}_IEEE_SUPPORT
      )
  endif ()

endforeach()



