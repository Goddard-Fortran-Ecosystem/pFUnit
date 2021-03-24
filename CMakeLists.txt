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
#     -DSKIP_OPENMP=YES                 Skip build of OpenMP features even if
#                                      OpenMP is found
#
#     -DSKIP_FHAMCREST=YES              Skip build of Hamcrest features
#
#     -DSKIP_ROBUST=YES                 Skip build of RobustRunner
#
#
# Usage:
#   cmake <path-to-source> [<options>]
#
# ------------------------------------------------------------------------ #
cmake_minimum_required(VERSION 3.12)

project (PFUNIT
  VERSION 4.2.1
  LANGUAGES Fortran C)

# Determine if pFUnit is built as a subproject (using
# add_subdirectory) or if it is the main project. Snippet taken from
# fmt: https://github.com/fmtlib/fmt/blob/eb52ac7/CMakeLists.txt
set(MAIN_PROJECT OFF)
if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  set(MAIN_PROJECT ON)
endif ()

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
option (SKIP_MPI "Skip building MPI support." NO)
option (SKIP_OPENMP "Skip OpenMP compiler options." NO)
option (SKIP_FHAMCREST "Skip building fHamcrest support" NO)
option (SKIP_ROBUST "Skip building RobustRunner" NO)
option (SKIP_ESMF "Skip building ESMF support" YES)
option (ENABLE_TESTS "Build pFUnit tests" ${MAIN_PROJECT})



set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake" "${PROJECT_SOURCE_DIR}/include")
include(BuildType)
include(CheckCompilerCapabilities)

#-----------------------------------
# Set default Fortran compiler flags
#-----------------------------------
include(cmake/${CMAKE_Fortran_COMPILER_ID}.cmake RESULT_VARIABLE found)
if(NOT found)
  message( FATAL_ERROR "Unrecognized Fortran compiler. Please use ifort, gfortran, NAG, PGI, or XL.")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Skip MPI and RobustRunner on MinGW
if (MINGW)
  set(SKIP_MPI YES)
  set(SKIP_ROBUST YES)
endif ()


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
  # Kludge to workaround cmake+NAG+openmp linkage problem
  if (CMAKE_Fortran_COMPILER_ID MATCHES NAG)
    set_property(TARGET OpenMP::OpenMP_Fortran PROPERTY
      INTERFACE_LINK_LIBRARIES "")
    set_property(TARGET OpenMP::OpenMP_Fortran PROPERTY INTERFACE_LINK_OPTIONS "-openmp")
  endif ()
endif()


# ESMF options are still under development ...
if (NOT SKIP_ESMF)
  set (ESMF_LIBRARIES "" CACHE PATH "Path to ESMF libraries")
  set (ESMF_INCLUDE "" CACHE PATH "Path to ESMF include dir")
  set (NETCDF_INCLUDE "" CACHE PATH "Path to NETCD include dir")
  set (ESMF_FOUND)
endif ()

find_package(Python)

set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

set (dest "PFUNIT-${PFUNIT_VERSION_MAJOR}.${PFUNIT_VERSION_MINOR}")

# Code to silence warnings from RANLIB on OSX
# From https://stackoverflow.com/questions/4929255/building-static-libraries-on-mac-using-cmake-and-gcc
if (APPLE)
  SET(CMAKE_Fortran_ARCHIVE_CREATE   "<CMAKE_AR> Scr <TARGET> <LINK_FLAGS> <OBJECTS>")
  SET(CMAKE_Fortran_ARCHIVE_FINISH   "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
endif ()

add_subdirectory (src)
add_subdirectory (include)
add_subdirectory (bin)

if (ENABLE_TESTS)
  enable_testing()
  add_custom_target(build-tests)

  add_custom_target(tests
    COMMAND ${CMAKE_CTEST_COMMAND}
    EXCLUDE_FROM_ALL)
  add_dependencies(tests build-tests)

  add_subdirectory (tests EXCLUDE_FROM_ALL)

  # The following forces tests to be built when using "make ctest" even if some targets
  # are EXCLUDE_FROM_ALL
  # From https://stackoverflow.com/questions/733475/cmake-ctest-make-test-doesnt-build-tests/56448477#56448477
  build_command(CTEST_CUSTOM_PRE_TEST TARGET build-tests)
  string(CONFIGURE \"@CTEST_CUSTOM_PRE_TEST@\" CTEST_CUSTOM_PRE_TEST_QUOTED ESCAPE_QUOTES)
  file(WRITE "${CMAKE_BINARY_DIR}/CTestCustom.cmake" "set(CTEST_CUSTOM_PRE_TEST ${CTEST_CUSTOM_PRE_TEST_QUOTED})" "\n")
endif()

# Packaging
include(CMakePackageConfigHelpers)
include (cmake/packaging.cmake)

# Create the PFUNITConfig.cmake and pFUnitConfigVersion files
configure_package_config_file(cmake/PFUNITConfig.cmake.in PFUNITConfig.cmake
  INSTALL_DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/PFUNITConfig.cmake"
)

write_basic_package_version_file(
  PFUNITConfig-version.cmake
  VERSION ${PACKAGE_VERSION}
  COMPATIBILITY SameMajorVersion
  )

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

install(EXPORT PFUNIT
  FILE PFUNITTargets.cmake
  NAMESPACE PFUNIT::
  DESTINATION "${top_dir}/cmake"
  )
export(EXPORT PFUNIT
  FILE "${CMAKE_CURRENT_BINARY_DIR}/PFUNITTargets.cmake"
  NAMESPACE PFUNIT::
  )

# Set some variables needed by add_pfunit_test in the parent scope so
# that the build directory can be used directly
if (NOT MAIN_PROJECT)
  set(PFUNIT_PARSER "${Python_EXECUTABLE}" "${PROJECT_SOURCE_DIR}/bin/funitproc" PARENT_SCOPE)
  set(PFUNIT_DRIVER "${PROJECT_SOURCE_DIR}/include/driver.F90" PARENT_SCOPE)
  set(PFUNIT_TESTUTILS "${PROJECT_SOURCE_DIR}/include/TestUtil.F90" PARENT_SCOPE)

  if (BUILD_SHARED_LIBS)
    set (PFUNIT_LIBRARIES PFUNIT::funit_shared PARENT_SCOPE)
    if (NOT SKIP_MPI)
      set(PFUNIT_LIBRARIES ${PFUNIT_LIBRARIES} PFUNIT::pfunit_shared PARENT_SCOPE)
    endif()
  else()
    set (PFUNIT_LIBRARIES PFUNIT::funit PARENT_SCOPE)
    if (NOT SKIP_MPI)
      set(PFUNIT_LIBRARIES ${PFUNIT_LIBRARIES} PFUNIT::pfunit PARENT_SCOPE)
    endif()
  endif()

  include(add_pfunit_test)
endif()
