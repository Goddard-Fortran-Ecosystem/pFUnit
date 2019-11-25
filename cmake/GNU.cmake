# Compiler specific flags for GNU Fortran compiler

set(traceback "-fbacktrace")
set(check_all "-fcheck=all -std=f2018")
set(cpp "-cpp")


set(CMAKE_Fortran_FLAGS_DEBUG  "-O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} ${traceback} ${check_all} -ffree-line-length-512")


add_definitions(-D_GNU)
#add_definitions(-D__GFORTRAN__)
