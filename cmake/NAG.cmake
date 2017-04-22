# Compiler specific flags for NAG Fortran compiler

set(traceback "-gline")
set(check_all "-C=all")
set(cpp "-fpp")

set(CMAKE_Fortran_FLAGS_DEBUG  "-O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} ${traceback} ${check_all}")

add_definitions(-D_NAG)
