# Compiler specific flags for PGI Fortran compiler
# (or is this now NVIDIA?)

set(traceback "-traceback")
if( CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 20.11 )
  set(check_all "-Mbounds -Mchkstk")
else()
  set(check_all "-Mbounds -Mchkfpstk -Mchkstk")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG  "-O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${traceback} ${check_all} -Mallocatable=03")

