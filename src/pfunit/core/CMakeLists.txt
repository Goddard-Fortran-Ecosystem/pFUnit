set (srcs
  MpiContext.F90
  MpiTestParameter.F90
  MpiTestCase.F90
  MpiTestMethod.F90
  )

add_library (pfunit-core OBJECT ${srcs})
target_link_libraries (pfunit-core PUBLIC funit ${MPI_Fortran_LIBRARIES})

target_include_directories (pfunit-core PUBLIC ${MPI_Fortran_INCLUDE_PATH})

target_include_directories (pfunit-core PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../mod>
  $<BUILD_INTERFACE:${PFUNIT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${dest}/include>
)

set_target_properties (pfunit-core PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../mod)

if (mismatch)
  set_source_files_properties(MpiContext.F90 PROPERTIES COMPILE_FLAGS ${mismatch})
endif()

