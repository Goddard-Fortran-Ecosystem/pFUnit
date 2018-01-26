# Compiler specific flags for Intel Fortran compiler

if(WIN32)
  set(no_optimize "-Od")
  set(check_all "-check:all")
else()
  set(no_optimize "-O0")
  set(check_all "-check all")
endif()
  

set(disable_warning_for_long_names "-diag-disable 5462")
set(traceback "-traceback")
set(cpp "-cpp")


set(CMAKE_Fortran_FLAGS_DEBUG  "${no_optimize}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} ${traceback} ${check_all} ${disable_warning_for_long_names} -save-temps")

add_definitions(-D_INTEL)
