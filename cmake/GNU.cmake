# Compiler specific flags for GNU Fortran compiler

set (traceback "-fbacktrace")
set (check_all "-fbounds-check")
set (cpp "-cpp")
set (MISMATCH "-fallow-argument-mismatch")

string(REGEX MATCH "Microsoft" WSL ${CMAKE_HOST_SYSTEM_VERSION})
if (WSL)
  set(opt "-O2")
else ()
  set(opt "-O0")
endif ()

set(CMAKE_Fortran_FLAGS "-g ${cpp} ${opt} ${traceback} -ffree-line-length-512")
if (CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${MISMATCH}")
endif ()

add_definitions(-D_GNU)
