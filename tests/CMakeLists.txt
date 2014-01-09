cmake_minimum_required(VERSION 2.8)

include_directories (${pFUnit_SOURCE_DIR}/include)
#include_directories (${pFUnit_BINARY_DIR}/source)
include_directories(${CMAKE_BINARY_DIR}/mod)

set(OTHER_SOURCES MakeNaN.F90 MakeInfinity.F90 MockListener.F90)  
list(APPEND OTHER_SOURCES BrokenTestCase.F90 BrokenSetUpCase.F90)
list(APPEND OTHER_SOURCES FixtureTestCase.F90)

set(TEST_SRCS Test_Assert.F90)
list(APPEND TEST_SRCS Test_AssertReal.F90)
list(APPEND TEST_SRCS Test_AssertBasic.F90)
list(APPEND TEST_SRCS SimpleTestCase.F90)
list(APPEND TEST_SRCS Test_Assert.F90)
list(APPEND TEST_SRCS Test_AssertComplex.F90)
list(APPEND TEST_SRCS Test_AssertInteger.F90)
list(APPEND TEST_SRCS Test_Exception.F90)
list(APPEND TEST_SRCS Test_FixtureTestCase.F90)
list(APPEND TEST_SRCS Test_MockCall.F90)
list(APPEND TEST_SRCS Test_MockRepository.F90)
list(APPEND TEST_SRCS Test_SimpleTestCase.F90)
list(APPEND TEST_SRCS Test_StringConversionUtilities.F90)
list(APPEND TEST_SRCS Test_TestMethod.F90)
list(APPEND TEST_SRCS Test_TestResult.F90)
list(APPEND TEST_SRCS Test_UnixProcess.F90)

if (ROBUST)
  list(APPEND TEST_SRCS Test_RobustRunner.F90)
  list(APPEND TEST_SRCS Test_TestSuite.F90)
  list(APPEND TEST_SRCS robustTestSuite.F90)
endif()

list(APPEND TEST_SRCS Test_BasicOpenMP.F90)

if (MPI)
  list(APPEND TEST_SRCS Test_MpiContext.F90)
  list(APPEND TEST_SRCS Test_MpiException.F90)
  list(APPEND TEST_SRCS Test_MpiTestCase.F90)
endif()

if (HAS_CPP)

set(SRCS ${TEST_SRCS})

else() # use external cpp

if ("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang")
  set(CPP_STRINGIFY "STRINGIFY_OPERATOR")
else()
  set(CPP_STRINGIFY "STRINGIFY_SIMPLE")
endif()


# Create intermediate targets for all TEST_SRCS
function( ADD_TEST_SOURCES OUTVAR )
     set( outfiles )
     foreach( f ${ARGN} )
         # first we might need to make the input file absolute
         get_filename_component( f "${f}" ABSOLUTE )
         # get the relative path of the file to the current source dir
         file( RELATIVE_PATH baseFile "${CMAKE_CURRENT_SOURCE_DIR}" "${f}" )
         # alter the name ...
         string( REGEX REPLACE ".F90" "_cpp.F90" outFile "${CMAKE_CURRENT_BINARY_DIR}/${baseFile}" )
         # append the output file to the list of outputs
         list( APPEND outfiles "${outFile}" )
         # create the output directory if it doesn't exist
         get_filename_component( dir "${outFile}" PATH )
         if( NOT IS_DIRECTORY "${dir}" )
             file( MAKE_DIRECTORY "${dir}" )
         endif( NOT IS_DIRECTORY "${dir}" )
         # now add the custom command to generate the output file
         add_custom_command( OUTPUT "${outFile}"
             COMMAND ${CMAKE_C_COMPILER} -E -C -D${CPP_STRINGIFY} -DNAG -I${CMAKE_SOURCE_DIR}/include ${f} >${outFile}
             DEPENDS "${f}"
             )
     endforeach( f )
     # set the output list in the calling scope
     set( ${OUTVAR} ${outfiles} PARENT_SCOPE )
endfunction( ADD_TEST_SOURCES )

set(SRCS " ")
ADD_TEST_SOURCES( SRCS ${TEST_SRCS} )

endif() # HAS_CPP else clause

add_library(testpfunit STATIC ${SRCS} ${OTHER_SOURCES})
target_link_libraries(testpfunit pfunit)

set(TEST_EXE tests.x)

add_executable (${TEST_EXE} selfTests.F90)

add_dependencies(tests.x pfunit)

target_link_libraries(${TEST_EXE} pfunit testpfunit)

if (MPI)
     target_link_libraries(${TEST_EXE} ${MPI_Fortran_LIBRARIES} )
     add_custom_target(tests COMMAND mpirun -np 4 tests/${TEST_EXE} DEPENDS ${TEST_EXE} WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
else()
     add_custom_target(tests COMMAND ${TEST_EXE} DEPENDS ${TEST_EXE} WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
endif ()

if (ROBUST)
  set(REMOTE_EXE remote.x)
  add_executable (${REMOTE_EXE} RemoteProgram.F90)
  target_link_libraries(${REMOTE_EXE} pfunit testpfunit)
  if (MPI)
    target_link_libraries(${REMOTE_EXE} ${MPI_Fortran_LIBRARIES} )
  endif()
# Note for pfunit_2.1.0:  following line has no counterpart in
#  "master" branch, i.e. > pfunit_2.1.0...
  add_dependencies(tests ${REMOTE_EXE})
endif()
