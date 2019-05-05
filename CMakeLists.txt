# ------------------------------------------------------------------------ #
# DOCUMENTATION:
# ------------------------------------------------------------------------ #
#
# Package specific command line options:
#
#     -DMAX_ASSERT_RANK=<max_rank>     Limit overloaded interfaces to
#                                      rank <= max_rank
#
#     -DSKIP_MPI=YES                   Skip build of MPI features even if
#                                      MPI is found.
#
#     -SKIP_OPENMP=YES                 Skip build of OpenMP features even if
#                                      OpenMP is found
#
#     -SKIP_FHAMCREST=YES              Skip build of Hamcrest features
#
#     -SKIP_ROBUST=YES                 Skip build of RobustRunner
#
#
# Usage:
#   cmake <path-to-source> [<options>]
#
# ------------------------------------------------------------------------ #
cmake_minimum_required(VERSION 3.12)

project (PFUNIT
  VERSION 4.0.0
  LANGUAGES Fortran C)

enable_testing()

#---------------------------
# Main configuration options
#---------------------------
# Most users of this software do not (should not?) have permissions to
# install in the cmake default of /usr/local (or equiv on other os's).
# Below, the default is changed to a directory within the build tree
# unless the user explicitly sets CMAKE_INSTALL_PREFIX in the cache.
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set (CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/installed" CACHE PATH "default install path" FORCE )
    message(STATUS "*** Setting default install prefix to ${CMAKE_INSTALL_PREFIX}.")
    message(STATUS "*** Override with -DCMAKE_INSTALL_PREFIX=<path>.")
endif()

set (MAX_ASSERT_RANK 5 CACHE STRING "Maximum array rank for generated code.")
set (SKIP_MPI NO CACHE BOOL "Skip building MPI support.")
set (SKIP_OPENMP NO CACHE BOOL "Skip OpenMP compiler options.")
set (SKIP_FHAMCREST NO CACHE BOOL "Skip building fHamcrest support")
set (SKIP_ESMF YES CACHE BOOL "Skip building ESMF support")


set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PFUNIT_SOURCE_DIR}/cmake" "${PFUNIT_SOURCE_DIR}/include")
include(CheckCompilerCapabilities)

#-----------------------------------
# Set default Fortran compiler flags
#-----------------------------------
include(cmake/${CMAKE_Fortran_COMPILER_ID}.cmake RESULT_VARIABLE found)
if(NOT found)
  message( FATAL_ERROR "Unrecognized Fortran compiler. Please use ifort, gfortran, NAG, PGI, or XL.")
endif()


if (NOT SKIP_MPI)
  find_package (MPI QUIET)
  if (MPI_Fortran_FOUND)
    message (STATUS "MPI enabled")
  else ()
    message (STATUS "MPI not found; building without MPI")
  endif()    
endif()




# Fortran OpenMP support is not yet integrated into the CMake distribution.
if (NOT SKIP_OPENMP)
  find_package(OpenMP QUIET)
  if (OpenMP_Fortran_FOUND)
    message (STATUS "OpenMP enabled")
  else (OpenMP_Fortran_FOUND)
    message (STATUS "OpenMP not found - skipping openmp support.")
  endif()
endif()


# ESMF options are still under development ...
if (NOT SKIP_ESMF)
  set (ESMF_LIBRARIES "" CACHE PATH "Path to ESMF libraries")
  set (ESMF_INCLUDE "" CACHE PATH "Path to ESMF include dir")
  set (NETCDF_INCLUDE "" CACHE PATH "Path to NETCD include dir")
  set (ESMF_FOUND)
endif ()


set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

add_custom_target(tests COMMAND ${CMAKE_CTEST_COMMAND})
set (dest "PFUNIT-${PFUNIT_VERSION_MAJOR}.${PFUNIT_VERSION_MINOR}")

# Code to silence warnings from RANLIB on OSX
# From https://stackoverflow.com/questions/4929255/building-static-libraries-on-mac-using-cmake-and-gcc
SET(CMAKE_Fortran_ARCHIVE_CREATE   "<CMAKE_AR> Scr <TARGET> <LINK_FLAGS> <OBJECTS>")
SET(CMAKE_Fortran_ARCHIVE_FINISH   "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")

add_subdirectory (extern)
add_subdirectory (src)
add_subdirectory (tests)
add_subdirectory (include)
add_subdirectory (bin)





# Packaging
include (cmake/packaging.cmake)

# Create the PFUNITConfig.cmake and pFUnitConfigVersion files
configure_file(cmake/PFUNITConfig.cmake.in
  "${PROJECT_BINARY_DIR}/PFUNITConfig.cmake" @ONLY)
configure_file(cmake/PFUNITConfig-version.cmake.in
  "${PROJECT_BINARY_DIR}/PFUNITConfig-version.cmake" @ONLY)
configure_file(cmake/PFUNIT.mk.in
  "${PROJECT_BINARY_DIR}/PFUNIT.mk" @ONLY)

# Install the pFUnitConfig.cmake and pFUnitConfigVersion.cmake
set (top_dir PFUNIT-${PFUNIT_VERSION_MAJOR}.${PFUNIT_VERSION_MINOR})
install(FILES
  "${PROJECT_BINARY_DIR}/PFUNITConfig.cmake"
  "${PROJECT_BINARY_DIR}/PFUNITConfig-version.cmake"
  DESTINATION "${top_dir}/cmake")
install(FILES
  "${PROJECT_BINARY_DIR}/PFUNIT.mk"
  DESTINATION "${top_dir}/include")

