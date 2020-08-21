cmake_minimum_required(VERSION 3.0)

set(pf_tests
  Test_Core.pf
  Test_AnyOf.pf
  Test_AllOf.pf
  Test_Every.pf
  Test_IsEqual.pf
  Test_IsEqual_extended.pf
  Test_IsGreater.F90
  Test_StringContains.pf
  Test_StringStartsWith.pf
  Test_StringEndsWith.pf
  Test_IsArrayWithSize.pf
  Test_IsNear.pf
  Test_IsRelativelyNear.pf
)

set(test_srcs)
add_pfunit_sources (test_srcs ${pf_tests})

add_library(fhamcrest_tests  ${test_srcs})
target_link_libraries(fhamcrest_tests funit)

add_pfunit_ctest(fhamcrest_tests.x
        TEST_SOURCES ${pf_tests}
        LINK_LIBRARIES funit
        )
add_dependencies(build-tests fhamcrest_tests.x)
