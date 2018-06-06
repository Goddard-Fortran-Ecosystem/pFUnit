
set (srcs
  ESMF_TestCase.F90
  ESMF_TestMethod.F90
  ESMF_TestParameter.F90
)  

add_library (esmfunit}  EXCLUDE_FROM_ALL ${srcs})

include_directories (${INC_ESMF})
include_directories (${INC_NETCDF})
include_directories (${PFUNIT_INCLUDE_DIRS})

target_link_libraries (esmfunit ${ESMF_LIBRARIES} ${PFUNIT_LIBRARIES})

