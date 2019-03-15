# ToDo:  Create/find a FindESMF.cmake 

set (srcs
  ESMF_TestCase.F90
  ESMF_TestMethod.F90
  ESMF_TestParameter.F90
)  

add_library (efunit  EXCLUDE_FROM_ALL ${srcs})
set_target_properties (efunit PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories (efunit PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../pfunit/mod>
  $<INSTALL_INTERFACE:${dest}/include>)
target_include_directories (efunit PUBLIC ${ESMF_INCLUDE} ${NETCDF_INCLUDE})

target_link_libraries (efunit PUBLIC pfunit ${ESMF_LIBRARIES})


if (BUILD_SHARED)
  add_library (efunit_shared SHARED EXCLUDE_FROM_ALL ${srcs})
  target_link_libraries (efunit_shared PUBLIC pfunit ${ESMF_LIBRARIES})
  set_target_properties (efunit_shared PROPERTIES OUTPUT_NAME efunit)
  target_include_directories (efunit_shared PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../pfunit/mod>
    $<INSTALL_INTERFACE:${dest}/include>)
  target_include_directories (efunit_shared PUBLIC ${ESMF_INCLUDE} ${NETCDF_INCLUDE})
endif ()
