cmake_minimum_required(VERSION 3.0)

#include_directories (${PFUNIT_SOURCE_DIR}/include)
#include_directories (${PFUNIT_BINARY_DIR}/src)
#include_directories(${CMAKE_CURRENT_BINARY_DIR})
#include_directories(${CMAKE_CURRENT_SOURCE_DIR})

configure_file( junit-4.xsd junit-4.xsd COPYONLY )

set (other_sources
  MakeNaN.F90
  MakeInf.F90
  MockListener.F90
  BrokenTestCase.F90
  BrokenSetUpCase.F90
  FixtureTestCase.F90
  )

add_library(other_shared ${other_sources})
target_link_libraries(other_shared PUBLIC funit)

set(pf_tests
  Test_ExceptionList.pf
  Test_DisableTest.pf
  Test_AssertString.pf
  Test_Assert_Integer.pf
  Test_AssertGreaterThan_Integer.pf
  Test_AssertEqual_Real.pf
  Test_AssertNotEqual_Real.pf
  Test_AssertRelativelyEqual_Real.pf
  Test_AssertLessThan_Real.pf
  Test_AssertLessThanOrEqual_Real.pf
  Test_AssertGreaterThan_Real.pf
  Test_AssertGreaterThanOrEqual_Real.pf
  Test_AssertEqual_Complex.pf
#  Test_AssertNotEqual_Real.pf
#  Test_AssertRelativelyEqual_Real.pf
  Test_Norms.pf
  Test_GlobPattern.pf
  Test_LiteralPattern.pf
  Test_DotPattern.pf
  Test_RepeatPattern.pf
  Test_TapListener.pf
  Test_NameFilter.pf
  Test_RegularExpression.pf
  Test_TestMethod_before.pf
  Test_Long_Subroutine_Name.pf)

set (new_test_srcs)
set (test_srcs
  Test_Assert.F90
  Test_AssertBasic.F90
  SimpleTestCase.F90
  Test_Assert.F90
  Test_Exception.F90
  Test_FixtureTestCase.F90
  Test_MockCall.F90
  Test_MockRepository.F90
  Test_SimpleTestCase.F90
  Test_StringUtilities.F90
  Test_TestMethod.F90
  Test_TestResult.F90
  Test_XmlPrinter.F90
  Test_TestSuite.F90
  )

if (OPENMP_FORTRAN_FOUND)
  list(APPEND test_srcs Test_BasicOpenMP.F90)
endif ()


add_library (funit_tests STATIC ${test_srcs})
target_link_libraries(funit_tests other_shared)

#add_pfunit_sources (new_test_srcs ${pf_tests})

#add_library(new_stests EXCLUDE_FROM_ALL ${new_test_srcs})
#target_link_libraries(new_stests other_shared)
#add_executable (new_tests.x EXCLUDE_FROM_ALL ${PFUNIT_SOURCE_DIR}/include/driver.F90)
#set_source_files_properties(${PFUNIT_SOURCE_DIR}/include/driver.F90 PROPERTIES COMPILE_DEFINITIONS _TEST_SUITES="testSuites.inc")
#target_link_libraries(new_tests.x new_stests funit)


add_executable (funit_tests.x EXCLUDE_FROM_ALL serial_tests.F90)
target_link_libraries(funit_tests.x funit_tests funit)

add_test(NAME old_tests
  COMMAND funit_tests.x
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
add_dependencies(build-tests funit_tests.x)

if (NOT SKIP_ROBUST)
endif ()

if (NOT SKIP_ROBUST)
  add_library(robust robustTestSuite.F90)
  target_link_libraries(robust funit)
  add_pfunit_ctest (robust_tests.x
    TEST_SOURCES Test_UnixProcess.pf Test_RobustRunner.pf
    LINK_LIBRARIES robust
    )
  add_dependencies(build-tests robust_tests.x)

  set (REMOTE_EXE remote.x)
  add_executable (${REMOTE_EXE} EXCLUDE_FROM_ALL serialRemoteProgram.F90)
  target_link_libraries(${REMOTE_EXE} funit robust)
  add_dependencies(robust_tests.x ${REMOTE_EXE})
  add_dependencies(build-tests ${REMOTE_EXE})

endif ()


add_pfunit_ctest (new_tests.x
  TEST_SOURCES ${pf_tests}
  LINK_LIBRARIES other_shared
  REGISTRY testSuites.inc
)

add_dependencies(build-tests new_tests.x)
set_property (TEST old_tests
    PROPERTY FAIL_REGULAR_EXPRESSION "Encountered 1 or more failures/errors during testing"
    )



