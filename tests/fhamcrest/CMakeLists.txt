cmake_minimum_required(VERSION 3.24)

set(pf_tests
  Test_Core.pf
  Test_AnyOf.pf
  Test_AllOf.pf
  Test_Every.pf
  Test_IsEqual.pf
  Test_IsEqual_extended.pf
  Test_StringContains.pf
  Test_StringStartsWith.pf
  Test_StringEndsWith.pf
  Test_IsArrayWithSize.pf
  Test_IsNear.pf
  Test_IsRelativelyNear.pf
)

add_pfunit_ctest(fhamcrest_tests.x
        TEST_SOURCES ${pf_tests}
        LINK_LIBRARIES funit
        )
add_dependencies(build-tests fhamcrest_tests.x)
