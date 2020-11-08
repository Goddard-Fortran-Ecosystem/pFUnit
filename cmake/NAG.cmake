# Compiler specific flags for NAG Fortran compiler

set(traceback "-g")
#set(check_all "-C=all")
set(check_all "-C=pointer")

set(cpp "-fpp")
set(mismatch "-mismatch")

set(common_flags "${cpp} -w=x95")

set(CMAKE_Fortran_FLAGS_DEBUG "${common_flags} -O0 ${check_all} ${traceback}")
set(CMAKE_Fortran_FLAGS_RELEASE ${common_flags})

add_definitions(-D_NAG)
