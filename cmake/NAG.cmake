# Compiler specific flags for NAG Fortran compiler

set(traceback "-gline")
#set(check_all "-C=all")
#set(check_all "-C=pointer")

set(cpp "-fpp")
set(mismatch "-mismatch")

set(CMAKE_Fortran_FLAGS_DEBUG  "-O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} ${traceback} ${check_all} -w=x95")

add_definitions(-D_NAG)
