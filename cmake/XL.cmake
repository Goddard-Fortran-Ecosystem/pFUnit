# Compiler specific flags for XL Fortran compiler


set(check_all "-C")
set(cpp "-WF,-qfpp")

set(CMAKE_Fortran_FLAGS_DEBUG  "-O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} ${check_all}")

add_definitions(-DIBM)

