# Compiler specific flags for PGI Fortran compiler
# (or is this now NVIDIA?)

set(traceback "-traceback")
set(check_all "-Mbounds -Mchkstk")

set(CMAKE_Fortran_FLAGS_DEBUG  "-O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${traceback} ${check_all} -Mallocatable=03")

