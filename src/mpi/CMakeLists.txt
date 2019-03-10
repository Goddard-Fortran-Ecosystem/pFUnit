set(CMAKE_Fortran_MODULE_DIRECTORY ${PFUNIT_BINARY_DIR}/mod)

if (BUILD_SHARED)
  add_library(pfunit_shared SHARED ${srcs} ${mpi_srcs})
  set_target_properties(pfunit_shared PROPERTIES OUTPUT_NAME pfunit)
endif ()


set (mpi_srcs
  MpiContext.F90
  MpiTestParameter.F90
  MpiTestCase.F90
  MpiTestMethod.F90
  pFUnit.F90
  pfunit_main.F90

  )
add_library(pfunit STATIC ${srcs} ${mpi_srcs})
target_link_libraries (pfunit PUBLIC funit ${CMAKE_Fortran_LIBRARIES})
target_include_directories(pfunit PUBLIC ${MPI_Fortran_INCLUDE_PATH})
set_target_properties (pfunit PROPERTIES Fortran_MODULE_DIRECTORY ${PFUNIT_BINARY_DIR}/mod)

if (mismatch)
  set_source_files_properties(MpiContext.F90 PROPERTIES COMPILE_FLAGS ${mismatch})
endif()

install (DIRECTORY  ${PFUNIT_BINARY_DIR}/mod/ DESTINATION ${dest}/include)
install (TARGETS pfunit EXPORT PFUNIT DESTINATION ${dest}/lib)
if (BUILD_SHARED)
    install (TARGETS pfunit_shared EXPORT PFUNIT DESTINATION ${dest}/lib)
endif()

