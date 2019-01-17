
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
    add_custom_command(
      OUTPUT ${ofile}
      COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/bin/overload_template.py --rank=${rank} -i ${ifile} -o ${ofile} ${python_defs}
      DEPENDS ${ifile} ${CMAKE_SOURCE_DIR}/bin/overload_template.py
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
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/bin/overload_template.py -i ${ifile} -o ${ofile} ${python_defs}
  DEPENDS ${ifile} ${CMAKE_SOURCE_DIR}/bin/overload_template.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT Generating ${ofile}
  )

foreach(rank RANGE ${MAX_ASSERT_RANK})
  set(ofile Norms_${rank}d.F90)
  set(ifile ${CMAKE_CURRENT_SOURCE_DIR}/asserts/Norms.tmpl)
  list(APPEND new_generated_sources ${ofile})
  add_custom_command(
    OUTPUT ${ofile}
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/bin/overload_template.py --rank=${rank} -i ${ifile} -o ${ofile} ${python_defs}
    DEPENDS ${ifile} ${CMAKE_SOURCE_DIR}/bin/overload_template.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT Generating ${ofile}
    )
endforeach()

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

  TestFilter.F90
  NameFilter.F90
  FUnit.F90
  )

  list (APPEND srcs
    UnixPipeInterfaces.F90
    UnixProcess.F90
    RobustRunner.F90
    RemoteProxyTestCase.F90
    )

message("generated files: ${generated_sources_output}")
add_library(funit STATIC ${srcs} "${generated_sources_output}")

if (NOT GFTL)
  add_dependencies(funit gFTL)
endif()

if (NOT GFTL_SHARED)
  add_dependencies(funit gFTL-shared)
endif()

if (NOT FARGPARSE)
  add_dependencies(funit fArgParse)
endif()

target_link_libraries(funit PUBLIC lib_fargparse lib_gFTL-shared)

if (BUILD_SHARED)
    add_library(funit_shared SHARED ${srcs})
    set_target_properties(funit_shared PROPERTIES OUTPUT_NAME funit)
endif ()

if (NOT GFTL)
  add_dependencies(funit gFTL)
endif()

set_target_properties (funit PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
target_include_directories (funit PRIVATE ${gFTL_install_dir}/include)
target_include_directories (funit PRIVATE ${gFTL-shared_install_dir}/mod)
target_include_directories (funit PRIVATE ${fArgParse_install_dir}/include)
target_include_directories (funit PUBLIC ${CMAKE_BINARY_DIR}/mod)
target_include_directories (funit PRIVATE ${CMAKE_SOURCE_DIR}/include)

install (TARGETS funit DESTINATION lib)
if (BUILD_SHARED)
    install (TARGETS funit_shared DESTINATION lib)
endif()

INSTALL (DIRECTORY  ${CMAKE_BINARY_DIR}/mod DESTINATION .)


