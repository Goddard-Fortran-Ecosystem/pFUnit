# ------------------------------------------------------------------------ #
# DOCUMENTATION:
# ------------------------------------------------------------------------ #
#
# Command line options:
#     USE_MPI=YES                             ! defaults to NO
#     USE_OPENMP=YES                          ! defaults to NO
#     USE_ROBUST=YES                          ! defaults to NO
#     MAX_ASSERT_RANK=<max array rank for generated code>
#                                             ! defaults to 5 or
#
# Usage:
#   cmake -DMPI=YES <path-to-source>
#
# ------------------------------------------------------------------------ #
cmake_minimum_required(VERSION 3.8.0)

project (pFUnit
  VERSION 3.1.1
  LANGUAGES Fortran)

enable_testing()

#---------------------------
# Main configuration options
#---------------------------
OPTION(USE_OPENMP "Use OPENMP for parallel runs" NO)
set(MPI OFF CACHE BOOL "Build with MPI support.")
set(OPENMP OFF CACHE BOOL "Build with OpenMP support.")
set(MAX_ASSERT_RANK 5 CACHE STRING "Maximum array rank for generated code.")


set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
include(CheckCompilerCapabilities)

#-----------------------------------
# Set default Fortran compiler flags
#-----------------------------------
include(cmake/${CMAKE_Fortran_COMPILER_ID}.cmake RESULT_VARIABLE found)
if(NOT found)
  message( FATAL_ERROR "Unrecognized Fortran compiler. Please use ifort, gfortran, NAG, PGI, or XL.")
endif()




if (MPI)
  find_package(MPI REQUIRED)
  if(MPI_Fortran_FOUND)
    if(MPI_FORTRAN_COMPILE_FLAGS)
      set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS})
    endif()
    if(MPI_Fortran_INCLUDE_PATH)
      include_directories(${MPI_Fortran_INCLUDE_PATH})
    endif()
    if(MPI_Fortran_LIBRARIES)
      set(CMAKE_Fortran_LIBRARIES "${CMAKE_Fortran_LIBRARIES} ${MPI_Fortran_LIBRARIES}")
    endif()
    if(MPI_Fortran_LINK_FLAGS)
      set(CMAKE_Fortran_LINK_FLAGS "${CMAKE_Fortran_LINK_FLAGS} ${MPI_Fortran_LINK_FLAGS}")
    endif()
  endif()
  add_definitions(-DUSE_MPI)
  if (MPIEXEC MATCHES ".*openmpi*")
    list(APPEND MPIEXEC_PREFLAGS "--oversubscribe")
  endif()
  message( STATUS "MPI enabled")
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





if (USE_ROBUST)
    add_definitions(-DUSE_ROBUST)
endif()

# By default pFUnit uses FTL as a git submodule.   But this can be overridden with:
set(FTL "" CACHE PATH "Optional path to externally installed FTL package.")

if (FTL) # use default
  set(ftl_install_dir ${FTL})
else()
  set(ftl_source_dir ${CMAKE_CURRENT_SOURCE_DIR}/FTL)
  set(ftl_install_dir ${CMAKE_CURRENT_BINARY_DIR}/FTL/install)

  include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)
  file(GLOB all_files ${ftl_source_dir}/*)
  list(LENGTH all_files n_files)

  if(n_files LESS_EQUAL 3)
    # git clone command did not use --recurse-submodules
    set(repository https://github.com/Goddard-Fortran-Ecosystem/gFTL.git)
    set(download_command git submodule init)
    set(update_command git submodule update)
  else()
    set(repository "")
    set(download_command "")
    set(update_command "")
  endif()

  ExternalProject_Add(gFTL
    GIT_REPOSITORY ${repository}
    DOWNLOAD_COMMAND ${download_command}
    UPDATE_COMMAND ${update_command}
    PREFIX ${CMAKE_CURRENT_BINARY_DIR}/FTL
    SOURCE_DIR ${ftl_source_dir}
    INSTALL_DIR ${ftl_install_dir}
    BUILD_COMMAND make
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${ftl_install_dir}  -DCMAKE_INSTALL_MESSAGE=LAZY
    INSTALL_COMMAND make install)
endif()

set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SKIP_RPATH ON)

add_custom_target(tests COMMAND ${CMAKE_CTEST_COMMAND})


add_subdirectory (src)
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
