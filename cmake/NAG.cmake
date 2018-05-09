# Compiler specific flags for NAG Fortran compiler

set(traceback "-gline")
#set(check_all "-C=all")
set(check_all "-C=array -C=alias -C=bits -C=calls -C=do -C=intovf -C=present -C=pointer")

set(cpp "-fpp")
set(mismatch "-mismatch")

set(CMAKE_Fortran_FLAGS_DEBUG  "-O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} ${traceback} ${check_all}")

add_definitions(-D_NAG)
