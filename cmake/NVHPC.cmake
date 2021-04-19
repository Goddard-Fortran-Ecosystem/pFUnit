# Compiler specific flags for NVHPC Fortran compiler

set(traceback "-traceback")
set(check_all "-Mbounds -Mchkstk")

set(CMAKE_Fortran_FLAGS_DEBUG  "-O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${traceback} ${check_all} -Mallocatable=03")
