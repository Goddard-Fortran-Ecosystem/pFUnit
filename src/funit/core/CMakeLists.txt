set(srcs
  KeywordEnforcer.F90
  MatchObject.F90
  AbstractPattern.F90
#  pfunit_main.F90
  None.F90
  GlobPattern.F90
  LiteralPattern.F90
  DotPattern.F90
  RepeatPattern.F90
  RegularExpression.F90
  AbstractTestParameter.F90
  AbstractTestResult.F90
  AbstractPrinter.F90
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
  TestTimer.F90
  ThrowFundamentalTypes.F90
  XmlPrinter.F90

  TestFilter.F90
  NameFilter.F90
  GlobFilter.F90
  FUnit_Core.F90
  get_errno.c
  )

# Add regex support for Unix/Linux/macOS only
if(NOT WIN32)
  list(APPEND srcs
    regex_F.c
    regex_module.F90
    RegexFilter.F90
  )
endif()

# Support for RobustRunner
if (NOT SKIP_ROBUST)

  add_executable(posix_predefined.x posix_predefined.c)
  set_target_properties(posix_predefined.x PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    )
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

  list (APPEND srcs
    Posix.F90
    File.F90
    UnixPipeInterfaces.F90
    UnixProcess.F90
    RobustRunner.F90
    RemoteRunner.F90
    RemoteProxyTestCase.F90
    )
endif ()


add_library (funit-core OBJECT ${srcs})
# Set Fortran module directories
set_funit_target_properties (funit-core)
# Link libraries and set compile definitions
funit_target_link_pfunit (funit-core)

# This is for dependency tracking
target_include_directories(funit-core INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

if (NOT SKIP_ROBUST)
  add_dependencies (funit-core generate_posix_parameters)
  target_include_directories (funit-core PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  )
endif ()

