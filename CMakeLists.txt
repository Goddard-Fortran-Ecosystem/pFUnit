# ------------------------------------------------------------------------ #
# DOCUMENTATION:
# ------------------------------------------------------------------------ #
# Command line options:
#     MPI=YES               ! defaults to NO
#     INSTALL_PATH=<path>   ! defaults to $(PFUNIT)
# ------------------------------------------------------------------------ #
INCLUDE(CMakeForceCompiler)
cmake_minimum_required(VERSION 2.8)
project (pFUnit)
enable_language (Fortran)
set (CMAKE_VERBOSE_MAKEFILE 1)

if ( DEFINED ENV{PFUNIT} )
    OPTION(INSTALL_PATH "Use PFUNIT for development" ENV{PFUNIT})
else ()
endif ()

OPTION(MPI "Use MPI for parallel runs" NO)
OPTION(OPENMP "Use OPENMP for parallel runs" NO)

if (MPI)
    add_definitions(-DUSE_MPI)
    find_package(MPI REQUIRED) 
    include_directories(${MPI_INCLUDE_PATH}) 
    set (COMPILER_WRAPPER mpif90)
    message( STATUS "MPI enabled")
endif()

if (OPENMP)
    add_definitions(-DUSEOPENMP)
    find_package(OpenMP REQUIRED) 
    if(OPENMP_FOUND)
      set(OPENMP_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
      set(OPENMP_LINKER_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    endif()
    message( STATUS "OpenMP enabled")
endif()

get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME STREQUAL "ifort")

    # Use ifort
    add_definitions(-DIntel)
    add_definitions(-DSTRINGIFY_OPERATOR)
    if (OPENMP)
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -check uninit -check bounds -traceback -assume realloc_lhs -fopenmp")
    else()
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -check uninit -check bounds -traceback -assume realloc_lhs")
    endif()

elseif(Fortran_COMPILER_NAME STREQUAL "gfortran" OR Fortran_COMPILER_NAME STREQUAL "gfortran-mp-4.8")

    # Use Gfortran
    add_definitions(-DSTRINGIFY_SIMPLE)
    add_definitions(-DGNU)
    if (OPENMP)
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -fcheck=bounds -fopenmp")
    else()
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -fcheck=bounds")
    endif()

elseif(Fortran_COMPILER_NAME STREQUAL "nagfor")

  # use nagfor			     
  if (OPENMP)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -f2003 -w=uda -gline -fpp -mismatch_all -openmp")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -f2003 -w=uda -gline -fpp -mismatch_all")
  endif()

else()

    message( FATAL_ERROR "Unrecognized compiler. Please use ifort, gfortran, gfortran-mp-4.8 or nagfor" )

endif()

# Set the compiler name to the wrapper, if necessary
if (NOT ${COMPILER_WRAPPER} STREQUAL "")
   CMAKE_FORCE_Fortran_COMPILER(${COMPILER_WRAPPER} ${Fortran_COMPILER_NAME})
endif ()

set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

add_subdirectory (source)
add_subdirectory (tests)

