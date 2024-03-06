if (CMAKE_Fortran_COMPILER_ID MATCHES Fujitsu)
    if (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.10.0)
      message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} version must be at least 4.10.0!")
    endif ()
endif ()

# Compiler specific flags for Fujitsu Fortran compiler

set(check_all "-Nquickdbg")
set(cpp "-Cfpp")

set(CMAKE_Fortran_FLAGS_DEBUG "-O0 ${check_all}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} -Nalloc_assign -Free")
