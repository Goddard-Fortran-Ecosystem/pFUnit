cmake_minimum_required(VERSION 3.0)

include_directories (${PFUNIT_SOURCE_DIR}/include)
include_directories (${PFUNIT_BINARY_DIR}/src)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

set(pf_tests
  Test_Core.pf
  Test_AnyOf.pf
  Test_AllOf.pf
  Test_Every.pf
  Test_IsEqual_extended.pf
  Test_StringContains.pf
  Test_StringStartsWith.pf
  Test_StringEndsWith.pf
  Test_IsArrayWithSize.pf
  Test_IsNear.pf
  Test_IsRelativelyNear.pf
)


set(test_srcs)
add_pfunit_sources (test_srcs ${pf_tests})

add_library(fhamcrest_tests EXCLUDE_FROM_ALL ${test_srcs})
target_link_libraries(fhamcrest_tests funit)

configure_file(${CMAKE_SOURCE_DIR}/include/driver.F90 driver.F90)
add_executable (fhamcrest_tests.x EXCLUDE_FROM_ALL driver.F90)
set_source_files_properties(driver.F90 PROPERTIES COMPILE_DEFINITIONS _TEST_SUITES="testSuites.inc")
set_source_files_properties(driver.F90 PROPERTIES OBJECT_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/testSuites.inc)
target_link_libraries(fhamcrest_tests.x fhamcrest_tests)



add_test(NAME fhamcrest_tests
  COMMAND fhamcrest_tests.x
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
add_dependencies(tests fhamcrest_tests.x)
set_property (TEST fhamcrest_tests
    PROPERTY FAIL_REGULAR_EXPRESSION "Encountered 1 or more failures/errors during testing"
    )

