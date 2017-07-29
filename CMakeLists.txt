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
OPTION(USE_MPI "Use MPI for parallel runs" NO)
OPTION(USE_OPENMP "Use OPENMP for parallel runs" NO)
OPTION(USE_ROBUST "Build robust test runner" NO)
set(MAX_ASSERT_RANK 5 CACHE STRING "Maximum array rank for generated code.")




set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/include/cmake/Modules/")

#-----------------------------------
# Set default Fortran compiler flags
#-----------------------------------
include(cmake/${CMAKE_Fortran_COMPILER_ID}.cmake RESULT_VARIABLE found)
if(NOT found)
  message( FATAL_ERROR "Unrecognized Fortran compiler. Please use ifort, gfortran, NAG, PGI, or XL.")
endif()




if (USE_MPI)
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
      link_directories(${MPI_Fortran_LIBRARIES})
    endif()
    if(MPI_Fortran_LINK_FLAGS)
      set(CMAKE_Fortran_LINK_FLAGS "${CMAKE_Fortran_LINK_FLAGS} ${MPI_Fortran_LINK_FLAGS}")
      link_directories(${MPI_Fortran_LIBRARIES})
    endif()
  endif()
  add_definitions(-DUSE_MPI)
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
