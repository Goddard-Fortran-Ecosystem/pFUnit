# ------------------------------------------------------------------------ #
# DOCUMENTATION:
# ------------------------------------------------------------------------ #
#
# Package specific command line options:
#
#     -DSKIP_MPI=YES                   Skip build of MPI features even if
#                                      MPI is found.
#
#     -DMAX_ASSERT_RANK=<max_rank>     Limit overloaded interfaces to
#                                      rank <= max_rank
#
#     -SKIP_OPENMP=YES                 Skip build of OpenMP features even if
#                                      OpenMP is found
#
#     -SKIP_HAMCREST=YES               Skip build of Hamcrest features
#
#
# Usage:
#   cmake <path-to-source> [<options>]
#
# ------------------------------------------------------------------------ #
cmake_minimum_required(VERSION 3.13)

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
endif()

OPTION(USE_OPENMP "Use OPENMP for parallel runs" NO)
set(MPI OFF CACHE BOOL "Build with MPI support.")
set(OPENMP OFF CACHE BOOL "Build with OpenMP support.")
set(MAX_ASSERT_RANK 5 CACHE STRING "Maximum array rank for generated code.")


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
  endif()    
endif()




# Fortran OpenMP support is not yet integrated into the CMake distribution.
if (USE_OPENMP)
  find_package(OpenMP_Fortran REQUIRED)
  if(OPENMP_FORTRAN_FOUND)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
    set(CMAKE_Fortran_LINKER_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
    message( STATUS "OpenMP enabled")
  endif()
endif()


# A bit ugly, but main use case install ESMF in a non-canonical manner.
set (ESMF OFF CACHE BOOL "Build with ESMF support.")
if (ESMF)
  set (ESMF_LIBRARIES "" CACHE PATH "Path to ESMF libraries")
  set (ESMF_INCLUDE "" CACHE PATH "Path to ESMF include dir")
  set (NETCDF_INCLUDE "" CACHE PATH "Path to NETCD include dir")
endif ()


if (USE_ROBUST)
    add_definitions(-DUSE_ROBUST)
endif()


set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

add_custom_target(tests COMMAND ${CMAKE_CTEST_COMMAND})
set (dest "PFUNIT-${PFUNIT_VERSION_MAJOR}.${PFUNIT_VERSION_MINOR}")

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

# Install the pFUnitConfig.cmake and pFUnitConfigVersion.cmake
install(FILES
  "${PROJECT_BINARY_DIR}/PFUNITConfig.cmake"
  "${PROJECT_BINARY_DIR}/PFUNITConfig-version.cmake"
  DESTINATION "PFUNIT-${PFUNIT_VERSION_MAJOR}.${PFUNIT_VERSION_MINOR}/cmake")
