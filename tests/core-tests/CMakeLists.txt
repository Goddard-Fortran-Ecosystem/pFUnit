cmake_minimum_required(VERSION 2.8)

include_directories (${pFUnit_SOURCE_DIR}/include)
include_directories (${pFUnit_BINARY_DIR}/src)
include_directories(${CMAKE_BINARY_DIR}/mod)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

configure_file( junit-4.xsd junit-4.xsd COPYONLY )

set(OTHER_SOURCES
  MakeNaN.F90 MakeInf.F90 MockListener.F90
  BrokenTestCase.F90 BrokenSetUpCase.F90

  FixtureTestCase.F90)

set(pf_tests
  Test_ExceptionList.pf
  Test_DisableTest.pf
  Test_AssertString.pf
  Test_Assert_Integer.pf
  Test_AssertEqual_Real.pf
  Test_AssertNotEqual_Real.pf
  Test_AssertRelativelyEqual_Real.pf
  Test_AssertLessThan_Real.pf
  Test_AssertLessThanOrEqual_Real.pf
  Test_AssertGreaterThan_Real.pf
  Test_AssertGreaterThanOrEqual_Real.pf
  Test_Norms.pf
  Test_GlobPattern.pf
  Test_LiteralPattern.pf
  Test_DotPattern.pf
  Test_RepeatPattern.pf
  Test_TapListener.pf
  Test_RegularExpression.pf)

set(new_tests)
foreach (file ${pf_tests})
  get_filename_component(basename ${file} NAME_WE)
  set(f90_src ${basename}.F90)
  add_custom_command (
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${f90_src}
    COMMAND python
    ARGS ${CMAKE_SOURCE_DIR}/bin/pFUnitParser.py ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${f90_src}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${file}
    )
  list(APPEND new_tests ${f90_src})
endforeach()
  

set(SERIAL_TEST_SRCS
  Test_Assert.F90
  Test_AssertBasic.F90
  SimpleTestCase.F90
  Test_Assert.F90
#  Test_AssertComplex.F90
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

  list (APPEND SERIAL_TEST_SRCS
    Test_UnixProcess.F90
    Test_RobustRunner.F90
    robustTestSuite.F90
    )

list(APPEND SERIAL_TEST_SRCS Test_BasicOpenMP.F90)

add_library(funit_tests EXCLUDE_FROM_ALL STATIC ${SERIAL_TEST_SRCS} ${OTHER_SOURCES})
target_link_libraries(funit_tests funit)

add_library(new_stests EXCLUDE_FROM_ALL ${new_tests} ${OTHER_SOURCES})
target_link_libraries(new_stests funit)
add_executable (new_tests.x EXCLUDE_FROM_ALL ${CMAKE_SOURCE_DIR}/include/driver.F90)
set_source_files_properties(${CMAKE_SOURCE_DIR}/include/driver.F90 PROPERTIES COMPILE_DEFINITIONS _TEST_SUITES="testSuites.inc")
target_link_libraries(new_tests.x new_stests funit)



add_executable (funit_tests.x EXCLUDE_FROM_ALL serial_tests.F90)
target_link_libraries(funit_tests.x funit_tests funit)

set(REMOTE_EXE remote.x)
add_executable (${REMOTE_EXE} EXCLUDE_FROM_ALL serialRemoteProgram.F90)
target_link_libraries(${REMOTE_EXE} funit funit_tests)
#  if (MPI)
#    target_link_libraries(${REMOTE_EXE} ${MPI_Fortran_LIBRARIES} )
#  endif()
# Note for pfunit_2.1.0:  following line has no counterpart in
#  "master" branch, i.e. > pfunit_2.1.0...
add_dependencies(tests ${REMOTE_EXE})


add_test(NAME new_tests
  COMMAND new_tests.x
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
add_dependencies(tests new_tests.x)
set_property (TEST new_tests
    PROPERTY FAIL_REGULAR_EXPRESSION "Encountered 1 or more failures/errors during testing"
    )

add_test(NAME old_tests
  COMMAND funit_tests.x
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
add_dependencies(tests funit_tests.x)

set_property (TEST old_tests
    PROPERTY FAIL_REGULAR_EXPRESSION "Encountered 1 or more failures/errors during testing"
    )



