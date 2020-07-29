# Compiler specific flags for GNU Fortran compiler

set(traceback "-fbacktrace")
set(check_all "-fbounds-check")
set(cpp "-cpp")

string(REGEX MATCH "Microsoft" WSL ${CMAKE_HOST_SYSTEM_VERSION})
if (WSL)
  set(opt "-O2")
else ()
  set(opt "-O0")
endif ()

set(CMAKE_Fortran_FLAGS "-g ${cpp} ${opt} ${traceback}  -ffree-line-length-512")

add_definitions(-D_GNU)
