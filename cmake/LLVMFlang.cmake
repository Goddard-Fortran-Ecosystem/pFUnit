# Compiler specific flags for LLVM Flang

set(cpp "-cpp")

set(common_flags "${cpp}")
set(CMAKE_Fortran_FLAGS_DEBUG  "-O0 -g ${common_flags}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${common_flags}")
