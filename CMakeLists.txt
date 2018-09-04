# ------------------------------------------------------------------------ #
# DOCUMENTATION:
# ------------------------------------------------------------------------ #
#
# Command line options:
#     MPI=YES                                 ! defaults to NO
#     MAX_RANK=<max array rank for generated code>
#                                             ! defaults to 5 or
#                                             !   ${PFUNIT_MAX_ARRAY_RANK}
#
# Usage:
#   cmake -DMPI=YES <path to source>
#
# ------------------------------------------------------------------------ #
INCLUDE(CMakeForceCompiler)
cmake_minimum_required(VERSION 3.0)
cmake_policy(SET CMP0048 NEW)
project (pFUnit
    VERSION 3.1.1
    LANGUAGES Fortran)
#enable_language (Fortran)

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

OPTION(MAX_RANK "Maximum array rank for generated code." NO)
IF(NOT MAX_RANK)
   # Remove PFUNIT_MAX_RANK in version 4.
   IF(DEFINED ENV{PFUNIT_MAX_RANK})
      SET (MAX_RANK "$ENV{PFUNIT_MAX_RANK}")
   # Promote following to primary in version 4.
   ELSEIF(DEFINED ENV{PFUNIT_MAX_ARRAY_RANK})
      SET (MAX_RANK "$ENV{PFUNIT_MAX_ARRAY_RANK}")
   ELSE()
      SET (MAX_RANK 5)
   ENDIF()
ENDIF()


OPTION(MPI "Use MPI for parallel runs" NO)
OPTION(FIND_MPI "Attempt to automatically find MPI information" YES)
OPTION(MPI_USE_MPIEXEC "Use CMake's FindMPI to find mpiexec" NO)
OPTION(OPENMP "Use OPENMP for parallel runs" NO)

# The following sets robust to "ON".
OPTION(ROBUST "Build robust test runner" YES)

# If the user specifies this on the command line (or equivalently, in the
# CMake cache), we take them at their word and do not attempt any kind of
# autodetection.
set(OPENMP_FLAGS "OPENMP_FLAGS-NOTFOUND"
  CACHE STRING "OpenMP flags for the Fortran compiler.")


if (MPI)
    add_definitions(-DUSE_MPI)
    if (FIND_MPI)
      find_package(MPI REQUIRED)
      add_definitions(${MPI_Fortran_COMPILE_FLAGS})
      include_directories(${MPI_Fortran_INCLUDE_PATH})
    endif()
    message( STATUS "MPI enabled")
endif()

# 2014-0807 MLR MAX_RANK needed for code generation, not here.
#if (MAX_RANK)
#   add_definitions(-DMAX_RANK=${MAX_RANK})
#endif()

if (ROBUST)
    add_definitions(-DBUILD_ROBUST)
endif()

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")

if (UNIX)
   execute_process (COMMAND ifort --version 
		    OUTPUT_VARIABLE INTEL_COMPILER_VERSION)
			   string (REGEX MATCH "([0-9]+)" 
                    NUM_VERSION ${INTEL_COMPILER_VERSION})
  if ("${NUM_VERSION}" STREQUAL "13")
        add_definitions(-DINTEL_13)
  endif ()
  if ("${NUM_VERSION}" STREQUAL "16")
        add_definitions(-DINTEL_16)
  endif ()
endif ()

    #Use ifort
    #There seems to be no easy way to determine the compiler version on Windows
    #since it isn't necessary defined in the PATH. As a workaround, assume version
    #different to 13 if exectuing ifort doesn't give any results
    if(NOT "${INTEL_COMPILER_VERSION}" STREQUAL "")
      string (REGEX MATCH "([0-9]+)"
              NUM_VERSION ${INTEL_COMPILER_VERSION})
      if("${NUM_VERSION}" STREQUAL "13")
          add_definitions(-DINTEL_13)
      endif()
      if("${NUM_VERSION}" STREQUAL "16")
          add_definitions(-DINTEL_16)
      endif()
    endif()

    add_definitions(-DIntel)
    if (WIN32)
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS_DEBUG} -Od -check:uninit -check:bounds -traceback -assume:realloc_lhs ${CMAKE_Fortran_FLAGS}"
            )
    else (WIN32)
        set(CMAKE_Fortran_FLAGS
            "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -check uninit -check bounds -check pointers -check stack -traceback -assume realloc_lhs ${CMAKE_Fortran_FLAGS}"
            )
    endif (WIN32)


elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)

    # Use Gfortran
    add_definitions(-DGNU)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -fbounds-check")

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL NAG)

    # use nagfor
    add_definitions(-DNAG)
# Strong checking (-C=all breaks 6.0, but later releases will include it.
#    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -f2003 -w=uda -gline -fpp -C=all -fpp")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -f2003 -w=uda -gline -fpp -mismatch_all -C=array -C=alias -C=bits -C=do -C=intovf -C=present -C=pointer")

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL XL)

    # Use XLF.
    add_definitions(-DIBM)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -WF,-qfpp -C")

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)

    # Use PGI.
    add_definitions(-DPGI)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -traceback -Mallocatable=03 -Mbounds -Mchkfpstk -Mchkstk")

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Cray)

    # The Fortran module does not set a variable to the compiler version
    # number like the C module does. Since CCE is a suite of compilers we
    # can use the C version as a proxy for the Fortran version.
    #
    if(CMAKE_C_COMPILER_VERSION VERSION_GREATER 8.6.0)
      message( FATAL_ERROR "Cray compiler version number ${CMAKE_C_COMPILER_VERSION} < 8.6.0 minimum" )
    endif()

    # Use Cray
    add_definitions(-DCray)
    add_definitions(-DSTRINGIFY_SIMPLE)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG} -G0 -Rb ${CMAKE_Fortran_FLAGS}")
    set(OPENMP_FLAGS " ")

else()

    message( FATAL_ERROR "Unrecognized compiler. Please use ifort, gfortran, gfortran-mp-4.8, pgfortran, nagfor or cce" )

endif()

# Fortran OpenMP support is not yet integrated into the CMake distribution.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/include/cmake/Modules/")
if (OPENMP)
#    find_package(OpenMP_Fortran REQUIRED)
#    if(OPENMP_FORTRAN_FOUND)
#      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
#      set(CMAKE_Fortran_LINKER_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
#      message( STATUS "OpenMP enabled")
#    endif()
   if (OPENMP_FLAGS)
     set(OpenMP_Fortran_FLAGS "${OPENMP_FLAGS}")
   else()
     find_package(OpenMP_Fortran REQUIRED)
   endif()
   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
   set(CMAKE_Fortran_LINKER_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
   message( STATUS "OpenMP enabled")
endif()

set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

add_subdirectory (source)
add_subdirectory (tests)
add_subdirectory (include)
add_subdirectory (bin)

# Packaging
include (cmake/packaging.cmake)

# Create the pFUnitConfig.cmake and pFUnitConfigVersion files
configure_file(cmake/pFUnitConfig.cmake.in
  "${PROJECT_BINARY_DIR}/pFUnitConfig.cmake" @ONLY)
configure_file(cmake/pFUnitConfigVersion.cmake.in
  "${PROJECT_BINARY_DIR}/pFUnitConfigVersion.cmake" @ONLY)

# Install the pFUnitConfig.cmake and pFUnitConfigVersion.cmake
install(FILES
  "${PROJECT_BINARY_DIR}/pFUnitConfig.cmake"
  "${PROJECT_BINARY_DIR}/pFUnitConfigVersion.cmake"
  DESTINATION . )
