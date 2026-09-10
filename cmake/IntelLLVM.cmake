# Compiler specific flags for Intel Fortran compiler

if(WIN32)
  set(no_optimize "-Od")
  set(check_all "-check:all")
  set(debug_info "-Zi")
  set(disable_warning_for_long_names "-Qdiag-disable:5462")
  set(cpp "-fpp")
else()
  set(no_optimize "-O0")
  set(check_all "-check all,nouninit")
  set(debug_info "-g")
  set(disable_warning_for_long_names "-diag-disable 5462")
  set(cpp "-cpp")
endif()
  

set(traceback "-traceback")


set(common_flags "${cpp} ${disable_warning_for_long_names}")
set(CMAKE_Fortran_FLAGS_DEBUG  "${no_optimize} ${debug_info} ${common_flags} ${traceback} ${check_all}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${common_flags}")

add_definitions(-D_INTEL)
add_definitions(-D__ifort_18)
