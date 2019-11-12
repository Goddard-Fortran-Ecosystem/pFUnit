# Compiler specific flags for GNU Fortran compiler

set(traceback "-fbacktrace")
set(check_all "-fbounds-check")
set(cpp "-cpp")


set(CMAKE_Fortran_FLAGS_DEBUG  "-O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
#set(CMAKE_Fortran_FLAGS "-g ${cpp} -O0 ${traceback} ${check_all} -ffree-line-length-512")
set(CMAKE_Fortran_FLAGS "-g ${cpp} -O0 ${traceback}  -ffree-line-length-512")


add_definitions(-D_GNU)
