
set(CMAKE_Fortran_MODULE_DIRECTORY ${PFUNIT_BINARY_DIR}/mod)
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
    add_custom_command(
      OUTPUT ${ofile}
      COMMAND ${PYTHON_EXECUTABLE} ${PFUNIT_SOURCE_DIR}/bin/overload_template.py --rank=${rank} -i ${ifile} -o ${ofile} ${python_defs}
      DEPENDS ${ifile} ${PFUNIT_SOURCE_DIR}/bin/overload_template.py
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT Generating ${ofile}
      )
  endforeach()
endforeach()

set(ofile FormatIntrinsic.F90)
set(ifile ${CMAKE_CURRENT_SOURCE_DIR}/asserts/FormatIntrinsic.tmpl)
list(APPEND new_generated_sources ${ofile})
add_custom_command(
  OUTPUT ${ofile}
  COMMAND ${PYTHON_EXECUTABLE} ${PFUNIT_SOURCE_DIR}/bin/overload_template.py -i ${ifile} -o ${ofile} ${python_defs}
  DEPENDS ${ifile} ${PFUNIT_SOURCE_DIR}/bin/overload_template.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT Generating ${ofile}
  )

foreach(rank RANGE ${MAX_ASSERT_RANK})
  set(ofile Norms_${rank}d.F90)
  set(ifile ${CMAKE_CURRENT_SOURCE_DIR}/asserts/Norms.tmpl)
  list(APPEND new_generated_sources ${ofile})
  add_custom_command(
    OUTPUT ${ofile}
    COMMAND ${PYTHON_EXECUTABLE} ${PFUNIT_SOURCE_DIR}/bin/overload_template.py --rank=${rank} -i ${ifile} -o ${ofile} ${python_defs}
    DEPENDS ${ifile} ${PFUNIT_SOURCE_DIR}/bin/overload_template.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT Generating ${ofile}
    )
endforeach()


  
add_executable(posix_predefined.x posix_predefined.c)

set_source_files_properties(posix_predefined.inc PROPERTIES GENERATED TRUE)
add_custom_command(
    OUTPUT posix_predefined.inc
    COMMAND ./posix_predefined.x > posix_predefined.inc
    DEPENDS posix_predefined.x
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating posix_predefined.inc"
    )
add_custom_target(generate_posix_parameters
  DEPENDS posix_predefined.inc
  )

set(srcs
  ${new_generated_sources}
  KeywordEnforcer.F90
  MatchObject.F90
  AbstractPattern.F90
  funit_main.F90
#  pfunit_main.F90
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
  SurrogateTestCase.F90
  Test.F90
  TestAnnotation.F90
  DisableAnnotation.F90
  TimeoutAnnotation.F90
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

  TestFilter.F90
  NameFilter.F90
  FUnit.F90
  get_errno.c
  )

# Support for RobustRunner
list (APPEND srcs
  Posix.F90
  File.F90
  UnixPipeInterfaces.F90
  UnixProcess.F90
  TestTimer.F90
  RobustRunner.F90
  RemoteRunner.F90
  RemoteProxyTestCase.F90
  )

add_library(funit STATIC ${srcs} "${generated_sources_output}")

target_link_libraries(funit PUBLIC ext)
add_dependencies(funit generate_posix_parameters)

if (BUILD_SHARED)
    add_library(funit_shared SHARED ${srcs})
    set_target_properties(funit_shared PROPERTIES OUTPUT_NAME funit)
endif ()

set_target_properties (funit PROPERTIES Fortran_MODULE_DIRECTORY ${PFUNIT_BINARY_DIR}/mod)
target_include_directories (funit PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_include_directories (funit PUBLIC ${PFUNIT_BINARY_DIR}/mod)
target_include_directories (funit PRIVATE ${PFUNIT_SOURCE_DIR}/include)

install (TARGETS funit DESTINATION lib)
if (BUILD_SHARED)
    install (TARGETS funit_shared DESTINATION lib)
endif()

INSTALL (DIRECTORY  ${PFUNIT_BINARY_DIR}/mod DESTINATION .)


