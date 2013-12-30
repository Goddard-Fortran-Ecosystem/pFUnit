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
#set (CMAKE_VERBOSE_MAKEFILE 1)

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


get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME STREQUAL "ifort")

    # Use ifort
    execute_process(COMMAND ifort --version 
                    OUTPUT_VARIABLE INTEL_COMPILER_VERSION)
    string (REGEX MATCH "([0-9]+)" 
            NUM_VERSION ${INTEL_COMPILER_VERSION})
    if("${NUM_VERSION}" STREQUAL "13")
        add_definitions(-DINTEL_13)
    endif()
    add_definitions(-DIntel)
    add_definitions(-DSTRINGIFY_OPERATOR)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -check uninit -check bounds -traceback -assume realloc_lhs ${CMAKE_Fortran_FLAGS}")


elseif(Fortran_COMPILER_NAME STREQUAL "gfortran" OR Fortran_COMPILER_NAME STREQUAL "gfortran-mp-4.8")

    # Use Gfortran
    add_definitions(-DSTRINGIFY_SIMPLE)
    add_definitions(-DGNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -fbounds-check")

elseif(Fortran_COMPILER_NAME STREQUAL "nagfor")

    # use nagfor			     
    add_definitions(-DNAG)
    add_definitions(-DSTRINGIFY_SIMPLE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -f2003 -w=uda -gline -fpp -mismatch_all -fpp")

else()

    message( FATAL_ERROR "Unrecognized compiler. Please use ifort, gfortran, gfortran-mp-4.8 or nagfor" )

endif()

# Fortran OpenMP support is not yet integrated into the CMake distribution.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/include/cmake/Modules/")
if (OPENMP)
    find_package(OpenMP_Fortran REQUIRED) 
    if(OPENMP_FORTRAN_FOUND)
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
      set(CMAKE_Fortran_LINKER_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
      message( STATUS "OpenMP enabled")
    endif()
endif()


set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

add_subdirectory (source)
add_subdirectory (tests)

