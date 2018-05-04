ExternalProject_Get_Property(FTL install_dir)
include_directories(${ftl_dir}/include)
include_directories(${install_dir}/FTL/src/FTL-build/include)


set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
find_package(PythonInterp REQUIRED) #Sets ${PYTHON_EXECUTABLE}

set(generated_files
  AssertArraysInternalassertEqual.F90
  AssertArraysInternalassertGreaterThan.F90
  AssertArraysInternalassertGreaterThanOrEqual.F90
  AssertArraysInternalassertLessThan.F90
  AssertArraysInternalassertLessThanOrEqual.F90
  AssertArraysInternalassertNotEqual.F90
  AssertArraysInternalassertRelativelyEqual.F90
  AssertArraysSupport.F90
  )

foreach(type Integer Real Complex)
  foreach(rank RANGE ${MAX_ASSERT_RANK})
    list(APPEND generated_files Assert${type}${rank}.F90)
  endforeach()
endforeach()

set(new_generated_sources)
get_directory_property(dir_defs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMPILE_DEFINITIONS)
set(python_defs)
foreach (def ${dir_defs})
  list(APPEND python_defs "-${def}")
endforeach()

foreach(type Logical Integer Real Complex)
  foreach(rank RANGE ${MAX_ASSERT_RANK})
    set(ofile Assert${type}_${rank}d.F90)
    set(ifile ${CMAKE_CURRENT_SOURCE_DIR}/asserts/Assert_${type}.tmpl)
    list(APPEND new_generated_sources ${ofile})
    message("Rank ${rank} ${MAX_ASSERT_RANK}")
    add_custom_command(
      OUTPUT ${ofile}
      COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/bin/overload_template.py --rank=${rank} -i ${ifile} -o ${ofile} ${python_defs}
      DEPENDS ${ifile} ${CMAKE_SOURCE_DIR}/bin/overload_template.py
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT Generating ${ofile}
      )
  endforeach()
endforeach()

foreach(rank RANGE ${MAX_ASSERT_RANK})
  set(ofile Norms_${rank}d.F90)
  set(ifile ${CMAKE_CURRENT_SOURCE_DIR}/asserts/Norms.tmpl)
  list(APPEND new_generated_sources ${ofile})
  message("Rank ${rank} ${MAX_ASSERT_RANK}")
  add_custom_command(
    OUTPUT ${ofile}
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/bin/overload_template.py --rank=${rank} -i ${ifile} -o ${ofile} ${python_defs}
    DEPENDS ${ifile} ${CMAKE_SOURCE_DIR}/bin/overload_template.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT Generating ${ofile}
    )
endforeach()

# Generate AssertArray files:  AssertArrays.fh, generated.inc, and AssertXYZ?.F90
add_custom_command(
  OUTPUT ${generated_files} AssertArrays.fh

  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/GenerateAssertsOnArrays.py --maxRank ${MAX_ASSERT_RANK}
  DEPENDS GenerateAssertsOnArrays.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT Generating Assert<type><rank>.F90
  )

set(srcs
#  ${generated_files}
  ${new_generated_sources}
  KeywordEnforcer.F90
  MatchObject.F90
  AbstractPattern.F90
  StringVector.F90
  StringUnlimitedMap.F90
  funit_main.F90
#  pfunit_main.F90
  CommandLineArguments.F90
  Arg.F90
  ArgVector.F90
  ArgParser.F90
  None.F90
  GlobPattern.F90
  LiteralPattern.F90
  DotPattern.F90
  RepeatPattern.F90
  RegularExpression.F90
  AbstractTestParameter.F90
  asserts/AssertUtilities.F90
  asserts/Assert.F90
  asserts/AssertBasic.F90
  asserts/AssertString.F90
  AbstractTestResult.F90
  BaseTestRunner.F90
  DebugListener.F90
  TapListener.F90
  DynamicTestCase.F90
  Exception.F90
  ExceptionVector.F90
  ExceptionList.F90
  Expectation.F90
  MockCall.F90
  MockRepository.F90
  ParallelContext.F90
  ParameterizedTestCase.F90
  Params.F90
  ResultPrinter.F90
  SerialContext.F90
  SourceLocation.F90
  StringUtilities.F90
  SubsetRunner.F90
  SurrogateTestCase.F90
  Test.F90
  TestAnnotation.F90
  TestVector.F90
  TestCase.F90
  TestFailure.F90
  TestFailureVector.F90
  TestListener.F90
  TestListenerVector.F90
  TestMethod.F90
  TestResult.F90
  TestRunner.F90
  TestSuite.F90
  ThrowFundamentalTypes.F90
  XmlPrinter.F90

  FUnit.F90
  )

  list (APPEND srcs
    UnixPipeInterfaces.F90
    UnixProcess.F90
    RobustRunner.F90
    RemoteProxyTestCase.F90
    )

#include( ${CMAKE_CURRENT_BINARY_DIR}/${generated_sources_output} )

message("generated files: ${generated_sources_output}")
add_library(funit STATIC ${srcs} "${generated_sources_output}")

if (BUILD_SHARED)
    add_library(funit_shared SHARED ${srcs})
    set_target_properties(funit_shared PROPERTIES OUTPUT_NAME funit)

   if (USE_MPI)
     add_library(pfunit_shared SHARED ${srcs} ${mpi_srcs})
     set_target_properties(pfunit_shared PROPERTIES OUTPUT_NAME pfunit)
   endif ()
endif ()

add_dependencies(funit FTL)

include_directories(${CMAKE_BINARY_DIR}/mod)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

install (TARGETS funit DESTINATION lib)
if (BUILD_SHARED)
    install (TARGETS funit_shared pfunit_shared DESTINATION lib)
endif()

if (USE_MPI)
    set (mpi_srcs
      MpiContext.F90
      MpiStubs.F90
      MpiTestParameter.F90
      MpiTestCase.F90
      MpiTestMethod.F90
      pFUnit.F90
      )
    add_library(pfunit STATIC ${srcs} ${mpi_srcs})
    if (mismatch)
      set_source_files_properties(MpiContext.F90 PROPERTIES COMPILE_FLAGS ${mismatch})
    endif()

  install (TARGETS pfunit DESTINATION lib)
  if (BUILD_SHARED)
    install (TARGETS pfunit_shared DESTINATION lib)
  endif ()
endif ()

INSTALL (DIRECTORY  ${CMAKE_BINARY_DIR}/mod DESTINATION .)
