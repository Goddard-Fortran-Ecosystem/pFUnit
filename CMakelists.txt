# ------------------------------------------------------------------------ #
# DOCUMENTATION:
# ------------------------------------------------------------------------ #
# Command line options:
#     MPI=YES               ! defaults to NO
#     INSTALL_PATH=<path>   ! defaults to $(PFUNIT)
# ------------------------------------------------------------------------ #

cmake_minimum_required(VERSION 2.8)
project (pFUnit)
enable_language (Fortran)
set (CMAKE_VERBOSE_MAKEFILE 1)

if ( DEFINED ENV{PFUNIT} )
    OPTION(INSTALL_PATH "Use PFUNIT for development" ENV{PFUNIT})
else ()
endif ()

OPTION(MPI "Use MPI for parallel runs" NO)
#OPTION(OPENMP "Use OPENMP for parallel runs" YES)

if ( MPI )
    add_definitions(-DUSE_MPI)
    find_package(MPI REQUIRED) 
    include_directories(${MPI_INCLUDE_PATH}) 
endif()


get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME STREQUAL "ifort")

    # Use ifort
    add_definitions(-DIntel)
    add_definitions(-DSTRINGIFY_OPERATOR)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -check uninit -check bounds -traceback -assume realloc_lhs")

elseif(Fortran_COMPILER_NAME STREQUAL "gfortran" OR Fortran_COMPILER_NAME STREQUAL "gfortran-mp-4.8")

    # Use Gfortran

    add_definitions(-DSTRINGIFY_SIMPLE)
    add_definitions(-DGNU)
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -fcheck=bounds")

elseif(Fortran_COMPILER_NAME STREQUAL "nagfor")

  # use nagfor			     
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}")
  set(CMAKE_Fortran_FLAGS "-f2008 -dusty -I${SpF_BINARY_DIR}/framework")

else()

    message( FATAL_ERROR "Unrecognized compiler. Please use ifort, gfortran or gfortran-mp-4.8" )

endif()

set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

add_subdirectory (source)
add_subdirectory (tests)

