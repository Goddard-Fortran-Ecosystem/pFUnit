# ------------------------------------------------------------------------ #
# DOCUMENTATION:
# ------------------------------------------------------------------------ #
#
# Command line options:
#     MPI=YES               ! defaults to NO
#     INSTALL_PATH=<path>   ! defaults to $(PFUNIT)
# 
# Usage:
#   cmake -DMPI=YES -DINSTALL_PATH=<A path> <path to source>
#
# ------------------------------------------------------------------------ #
INCLUDE(CMakeForceCompiler)
cmake_minimum_required(VERSION 2.8)
project (pFUnit)
enable_language (Fortran)
#set (CMAKE_VERBOSE_MAKEFILE 1)

# MESSAGE(ENV{PFUNIT}:"$ENV{PFUNIT}") 
# set (ENV_PFUNIT "$ENV{PFUNIT}")
# MESSAGE(OK:"${ENV_PFUNIT}") 
# OPTION(INSTALL_PATH "Use PFUNIT for development" "$ENV{PFUNIT}")  
# OPTION(INSTALL_PATH "Use PFUNIT for development" "${ENV_PFUNIT}")  
OPTION(INSTALL_PATH "Use PFUNIT for development" OFF) 
IF(NOT INSTALL_PATH)
   IF(DEFINED ENV{PFUNIT})
      set (INSTALL_PATH "$ENV{PFUNIT}")
   ELSE()
      MESSAGE("** WARNING: Install path not found! Please set PFUNIT or use -DINSTALL_PATH=...")
   ENDIF()
ENDIF()
IF(NOT DEFINED ENV{PFUNIT})
   MESSAGE("++ Remember to set the environment variable PFUNIT to the install directory!")
   MESSAGE("++ Example: export PFUNIT=${INSTALL_PATH}")
ENDIF()
IF( NOT $ENV{PFUNIT} STREQUAL INSTALL_PATH )
   MESSAGE("** WARNING:  Environment variable PFUNIT not equal to INSTALL_PATH.")
   MESSAGE("**           Found: PFUNIT=$ENV{PFUNIT}")
ENDIF()
MESSAGE("-- Installing to INSTALL_PATH: ${INSTALL_PATH}")

OPTION(INSTALL_PATH "Use PFUNIT for development" ENV{PFUNIT})
OPTION(MPI "Use MPI for parallel runs" NO)
OPTION(OPENMP "Use OPENMP for parallel runs" NO)
OPTION(HAS_CPP "Compiler has useable C preprocessor" YES)

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
    set(HAS_CPP NO)
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
add_subdirectory (include)
add_subdirectory (bin)

# Override on command line with -DCMAKE_INSTALL_PREFIX=... or -DINSTALL_PATH=...
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set (CMAKE_INSTALL_PREFIX "${INSTALL_PATH}" CACHE PATH "default install path" FORCE )
endif()


