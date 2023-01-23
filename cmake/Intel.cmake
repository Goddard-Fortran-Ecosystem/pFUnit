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


set(common_flags "${cpp} ${disable_warning_for_long_names}")
set(CMAKE_Fortran_FLAGS_DEBUG  "-O0 -g ${common_flags} ${traceback} ${no_optimize} ${check_all}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${common_flags}")

add_compile_definitions(_INTEL)
add_compile_definitions(__ifort_18)
