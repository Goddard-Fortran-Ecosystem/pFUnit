#include_directories(${pFUnit_BINARY_DIRECTORY}/src/core)
#include_directories(${CMAKE_BINARY_DIR}/mod)

set(CMAKE_Fortran_MODULE_DIRECTORY ${pFUnit_BINARY_DIR}/mod)

if (BUILD_SHARED)
  add_library(pfunit_shared SHARED ${srcs} ${mpi_srcs})
  set_target_properties(pfunit_shared PROPERTIES OUTPUT_NAME pfunit)
endif ()


set (mpi_srcs
  MpiContext.F90
  MpiStubs.F90
  MpiTestParameter.F90
  MpiTestCase.F90
  MpiTestMethod.F90
  pFUnit.F90
  pfunit_main.F90

  # mpi hamcrest
  OnProcess.F90
  OnEveryProcess.F90
  )
add_library(pfunit STATIC ${srcs} ${mpi_srcs})
target_link_libraries (pfunit PUBLIC funit)
target_include_directories (pfunit PRIVATE ${ftl_install_dir}/include)
set_target_properties (pfunit PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

if (NOT FTL)
  add_dependencies(pfunit gFTL)
endif()
if (mismatch)
  set_source_files_properties(MpiContext.F90 PROPERTIES COMPILE_FLAGS ${mismatch})
endif()

install (TARGETS pfunit DESTINATION lib)
if (BUILD_SHARED)
  install (TARGETS pfunit_shared DESTINATION lib)
endif ()

INSTALL (DIRECTORY  ${CMAKE_BINARY_DIR}/mod DESTINATION .)

