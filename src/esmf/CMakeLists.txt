
set (srcs
  ESMF_TestCase.F90
  ESMF_TestMethod.F90
  ESMF_TestParameter.F90
)  

add_library (esmfunit  EXCLUDE_FROM_ALL ${srcs})
target_link_libraries (esmfunit PUBLIC pfunit ${ESMF_LIBRARIES})
target_link_libraries (esmfunit PUBLIC pfunit ${ESMF_LIBRARIES})
target_include_directories (esmfunit PUBLIC ${ESMF_INCLUDE} ${NETCDF_INCLUDE})

