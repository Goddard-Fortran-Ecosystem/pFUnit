cmake_minimum_required(VERSION 2.8)

SET (CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

set(srcs Assert.F90 AssertBasic.F90 AssertInteger.F90)
list(APPEND srcs BaseTestRunner.F90)
list(APPEND srcs DebugListener.F90)
list(APPEND srcs DynamicTestCase.F90)
list(APPEND srcs Exception.F90)
list(APPEND srcs MockCall.F90)
list(APPEND srcs MockRepository.F90)

list(APPEND srcs ParallelContext.F90)
list(APPEND srcs ParallelException.F90)
list(APPEND srcs ParameterizedTestCase.F90)
list(APPEND srcs Params.F90)
list(APPEND srcs ResultPrinter.F90)

if ( ROBUST )
  list(APPEND srcs UnixPipeInterfaces.F90)
  list(APPEND srcs UnixProcess.F90)
  list(APPEND srcs RobustRunner.F90)
  list(APPEND srcs RemoteProxyTestCase.F90)
endif()

list(APPEND srcs SerialContext.F90)
list(APPEND srcs SourceLocation.F90)
list(APPEND srcs StringConversionUtilities.F90)
list(APPEND srcs SubsetRunner.F90)
list(APPEND srcs SurrogateTestCase.F90)
list(APPEND srcs Test.F90)
list(APPEND srcs TestCase.F90)
list(APPEND srcs TestFailure.F90)
list(APPEND srcs TestListener.F90)
list(APPEND srcs TestMethod.F90)
list(APPEND srcs TestResult.F90)
list(APPEND srcs TestRunner.F90)
list(APPEND srcs TestSuite.F90)
list(APPEND srcs ThrowFundamentalTypes.F90)
list(APPEND srcs pFUnit.F90)
list(APPEND srcs pFUnitPackage.F90)
list(APPEND srcs AssertComplex.F90)
list(APPEND srcs AssertInteger1.F90)
list(APPEND srcs AssertReal.F90)

if (MPI) 
  list(APPEND srcs MpiContext.F90)
  list(APPEND srcs MpiStubs.F90)
  list(APPEND srcs MpiTestCase.F90)
  list(APPEND srcs MpiTestMethod.F90)
endif()


list(APPEND pysrcs CodeUtilities.py)
list(APPEND pysrcs GenerateRealArrayNewSignature.py)
list(APPEND pysrcs Utilities.py)

ADD_CUSTOM_COMMAND(
   OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/AssertReal.F90 ${CMAKE_CURRENT_BINARY_DIR}/AssertComplex.F90 ${CMAKE_CURRENT_BINARY_DIR}/AssertInteger1.F90
   COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/GenerateRealArrayNewSignature.py
   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/GenerateRealArrayNewSignature.py
   )

list(APPEND srcs AssertReal.F90 AssertComplex.F90 AssertInteger1.F90)

set(SOURCES ${srcs})

add_library(pfunit STATIC ${SOURCES})

include_directories(${CMAKE_BINARY_DIR}/mod)

INSTALL(TARGETS pfunit DESTINATION lib)
INSTALL(DIRECTORY  ${CMAKE_BINARY_DIR}/mod DESTINATION .)
