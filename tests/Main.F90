#include "reflection.h"
Program Main
  use pFUnit
  Implicit None

  Character(Len=100) :: testsummary
  Type (TestResult_type) :: result
  Type (TestSuite_type)  :: suite_all

  integer :: ier
  logical :: flag

  type (Report_type) :: rprt
#ifdef USE_MPI
  integer, parameter :: PFUNIT_MINIMUM_PROCESSES = 5
#else
  integer, parameter :: PFUNIT_MINIMUM_PROCESSES = 1 ! skip MPI tests
#endif

  integer :: npes

  call pFUnit_init()

  suite_all = TestSuite('top', &
       & (/ basic_tests(), testcase_tests(), testsuite_tests(), &
       & paramtest_tests(), FortranName_tests(), &
       & defect_tests()/))
#ifdef USE_DSO
  call add(suite_all, dso_tests())
#endif

  npes = numProcesses()

#ifdef USE_MPI
  if (npes < PFUNIT_MINIMUM_PROCESSES) then
     print*,'Must launch with at least',PFUNIT_MINIMUM_PROCESSES,'processes.'
     call mpi_abort(commWorld(), ier)
     stop
  end if
  call add(suite_all, newMpi_tests())
#endif

  result = newTestResult(mode=MODE_USE_STDOUT+MODE_USE_LOGFILE)

  Call Run(suite_all, result)

  if (amRoot()) then
     testsummary = Trim(Summary(result))
     print*
     print*, trim(testsummary)
  end if

  call Clean(result)
  call Clean(suite_all)

  ! Catch remaining exceptions

  if (amRoot()) then
     If (catch(preserve=.true.)) then
        Print*,'Remaining exception: '
        rprt = GenerateExceptionReport()
        call Print(rprt)
        call clean(rprt)
     end If
  endif
  call pFUnit_finalize()

Contains

  Function paramtest_tests() Result (asuite)
    Use TestParameterizedTestCase_mod, only: testWasRun, testMethod_uni, testMethod_multi_summary
    Use TestParameterizedTestCase_mod, only: setup => setup_ptest, teardown => teardown_ptest
    Use TestParameterizedTestCase_mod, only: testCountTestsA, testCountTestsB
    Use ParameterizedTestCase_mod
    Type (TestSuite_type) :: aSuite

    aSuite = TestSuite('parameterized tests')
    Call Add(aSuite, TestCase1StepFixture(setup, teardown, REFLECT(testWasRun)))
    Call Add(aSuite, TestCase1StepFixture(setup, teardown, REFLECT(testmethod_uni)))
    Call Add(aSuite, TestCase1StepFixture(setup, teardown, REFLECT(testmethod_multi_summary)))
    Call Add(aSuite, TestCase1StepFixture(setup, teardown, REFLECT(testCountTestsA)))
    Call Add(aSuite, TestCase1StepFixture(setup, teardown, REFLECT(testCountTestsB)))

  End Function paramtest_tests

  Function basic_tests() Result (aSuite)
    use test_AssertString_mod, only: testAssertEqual_unequalLength
    use test_AssertString_mod, only: testAssertEqual_different
    use test_AssertString_mod, only: testAssertEqual_ignoreWhiteSpace
    use test_AssertString_mod, only: test_trimWhiteSpace

    use test_AssertArray_mod,   only: test_areConformableA
    use test_AssertArray_mod,   only: test_areConformableB
    use test_AssertArray_mod,   only: test_areConformableC
    use test_AssertArray_mod,   only: test_areConformableD

    use test_AssertInteger_mod, only: test_AssertEqual_scalar
    use test_AssertInteger_mod, only: test_assertequal_scalar_message
    use test_AssertInteger_mod, only: test_arrayString
    use test_AssertInteger_mod, only: test_assertequal_vector
    use test_AssertInteger_mod, only: test_assertequal_scalar_vector

    use test_AssertReal_mod,    only: test_vectorNorm
    use test_AssertReal_mod,    only: test_isWithinTolerance
    use test_AssertReal_mod,    only: test_assertequal_float
    use test_AssertReal_mod,    only: test_assertequal_tolerance
    use test_AssertReal_mod,    only: test_assertequal_floatFail
    use test_AssertReal_mod,    only: test_assertequal_floatMessage

    use test_AssertComplex_mod, only: test_assertequal_complex
    use test_AssertComplex_mod, only: test_assertequal_complex_failed
    use test_AssertComplex_mod, only: test_assertequal_complex_int
    use test_AssertComplex_mod, only: test_assertequal_complex_real

    use TestTestResult_mod, only: test_testStarted
    use TestTestResult_mod, only: test_testFailed
    use TestTestResult_mod, only: test_testFailedReport
    use TestTestResult_mod, only: test_summary

    Type (TestSuite_type) :: aSuite

    aSuite = TestSuite('basic')
    call add(aSuite, exceptionTests())
    call add(aSuite, procedureTests())
    call add(aSuite, ioTests())

    call add(aSuite, TestCase1Step(REFLECT(testAssertEqual_unequalLength)))
    call add(aSuite, TestCase1Step(REFLECT(testAssertEqual_different)))
    call add(aSuite, TestCase1Step(REFLECT(testAssertEqual_ignoreWhiteSpace)))
    call add(aSuite, TestCase1Step(REFLECT(test_trimWhiteSpace)))

    call add(aSuite, TestCase1Step(REFLECT(test_areConformableA)))
    call add(aSuite, TestCase1Step(REFLECT(test_areConformableB)))
    call add(aSuite, TestCase1Step(REFLECT(test_areConformableC)))
    call add(aSuite, TestCase1Step(REFLECT(test_areConformableD)))

    call add(aSuite, TestCase1Step(REFLECT(test_AssertEqual_scalar)))
    call add(aSuite, TestCase1Step(REFLECT(test_assertequal_scalar_message)))
    call add(aSuite, TestCase1Step(REFLECT(test_arrayString)))
    call add(aSuite, TestCase1Step(REFLECT(test_assertequal_vector)))
    call add(aSuite, TestCase1Step(REFLECT(test_assertequal_scalar_vector)))

    call add(aSuite, TestCase1Step(REFLECT(test_vectorNorm)))
    call add(aSuite, TestCase1Step(REFLECT(test_isWithinTolerance)))
    call add(aSuite, TestCase1Step(REFLECT(test_AssertEqual_float)))
    call add(aSuite, TestCase1Step(REFLECT(test_AssertEqual_tolerance)))
    call add(aSuite, TestCase1Step(REFLECT(test_AssertEqual_floatFail)))
    call add(aSuite, TestCase1Step(REFLECT(test_AssertEqual_floatMessage)))

    call add(aSuite, TestCase1Step(REFLECT(test_AssertEqual_complex)))
    call add(aSuite, TestCase1Step(REFLECT(test_AssertEqual_complex_failed)))
    call add(aSuite, TestCase1Step(REFLECT(test_AssertEqual_complex_int)))
    call add(aSuite, TestCase1Step(REFLECT(test_AssertEqual_complex_real)))

    call add(aSuite, TestCase1Step(REFLECT(test_testStarted)))
    call add(aSuite, TestCase1Step(REFLECT(test_testFailed)))
    call add(aSuite, TestCase1Step(REFLECT(test_testFailedReport)))
    call add(aSuite, TestCase1Step(REFLECT(test_summary)))

  End Function basic_tests

  function exceptionTests() result(aSuite)
     use test_Exception_mod
     type (TestSuite_type) :: aSuite

     aSuite = TestSuite('exception')

     call add(aSuite, TestCase1Step(REFLECT(test_catchEmpty)))
     call add(aSuite, TestCase1Step(REFLECT(test_throw1)))
     call add(aSuite, TestCase1Step(REFLECT(test_catchFail)))
     call add(aSuite, TestCase1Step(REFLECT(test_catchSucceed)))
     call add(aSuite, TestCase1Step(REFLECT(test_catchOnly)))
     call add(aSuite, TestCase1Step(REFLECT(test_catchRethrow)))
     call add(aSuite, TestCase1Step(REFLECT(test_clearAll)))
     call add(aSuite, TestCase1Step(REFLECT(test_catchOnce)))
     call add(aSuite, TestCase1Step(REFLECT(test_catchGetException)))
     call add(aSuite, TestCase1Step(REFLECT(test_ignoreOtherBlock)))
     call add(aSuite, TestCase1Step(REFLECT(test_nestedTry)))
     call add(aSuite, TestCase1Step(REFLECT(test_macros)))
     call add(aSuite, TestCase1Step(REFLECT(test_getMessage)))
     call add(aSuite, TestCase1Step(REFLECT(test_ignoreSameMessage)))
#ifdef USE_MPI
     call add(aSuite, TestCase1Step(REFLECT(test_gatherExceptionsA), passMPI=.true.))
     call add(aSuite, TestCase1Step(REFLECT(test_gatherExceptionsB), passMPI=.true.))
#endif
  end function exceptionTests

  function procedureTests() result(aSuite)
     use test_BaseAddress_mod
     use test_ProcedurePointer_mod
     type (TestSuite_type) :: aSuite

     aSuite = TestSuite('procedurePointer')

     call add(aSuite, TestCase1Step(REFLECT(test_sameAddress)))
     call add(aSuite, TestCase1Step(REFLECT(test_differentAddress)))
     call add(aSuite, TestCase1Step(REFLECT(test_isNull)))

     call add(aSuite, TestCase1Step(REFLECT(test_callUninitialized)))
     call add(aSuite, TestCase1Step(REFLECT(test_callNoArguments)))
     call add(aSuite, TestCase1Step(REFLECT(test_callOneIntegerArgument)))
     call add(aSuite, TestCase1Step(REFLECT(test_callOneRealArgument)))

  end function procedureTests

  function ioTests() result(aSuite)
     use test_IO_Utilities_mod

     type (TestSuite_type) :: aSuite

     aSuite = TestSuite('IO')

     call add(aSuite, TestCase1Step(REFLECT(test_getUnit)))
     call add(aSuite, TestCase1Step(REFLECT(test_getUnit2)))
     call add(aSuite, TestCase1Step(REFLECT(test_openNew)))
     call add(aSuite, TestCase1Step(REFLECT(test_open2Files)))
     call add(aSuite, TestCase1Step(REFLECT(test_openNewFailExists)))
     call add(aSuite, TestCase1Step(REFLECT(test_openFailIsOpen)))
     call add(aSuite, TestCase1Step(REFLECT(test_openOld)))
     call add(aSuite, TestCase1Step(REFLECT(test_openOldFail)))
     call add(aSuite, TestCase1Step(REFLECT(test_deleteFile)))
     call add(aSuite, TestCase1Step(REFLECT(test_deleteFileIsOpen)))
     call add(aSuite, TestCase1Step(REFLECT(test_deleteFileDoesNotExist)))
     call add(aSuite, TestCase1Step(REFLECT(test_deleteFileDoesNotExistIgnore)))
     call add(aSuite, TestCase1Step(REFLECT(test_openUnformatted)))
     call add(aSuite, TestCase1Step(REFLECT(test_openFormatted)))

     call add(aSuite, TestCase1Step(REFLECT(test_isConnected)))
     call add(aSuite, TestCase1Step(REFLECT(test_isOpenUnit)))
     call add(aSuite, TestCase1Step(REFLECT(test_isOpenFileName)))
     call add(aSuite, TestCase1Step(REFLECT(test_getUnitFromName)))
     call add(aSuite, TestCase1Step(REFLECT(test_getUnitFromNameFail)))

    call add(aSuite, TestCase1Step(REFLECT(test_createNamedPipe)))

  end function ioTests

#ifdef USE_MPI
  Function newMpi_tests() Result (aSuite)
    use test_newMpiTestCase_mod
    use test_MpiTestCase_mod
    use test_TestInfo_mod
    Type (TestSuite_type) :: aSuite

    aSuite = TestSuite('new MPI suite')
!!$    call add(aSuite, TestCase1Step(REFLECT(test_readWrite)))
!!$    call add(aSuite, TestCase1Step(REFLECT(test_runOnServer)))

    call add(aSuite, TestCase1Step(REFLECT(test_constructor), passMPI=.true.))
    call add(aSuite, TestCase1Step(REFLECT(test_rank), passMPI=.true.))
    call add(aSuite, TestCase1Step(REFLECT(test_subset), passMPI=.true.))
    call add(aSuite, TestCase1Step(REFLECT(test_MPI_HelloWorld1), passMPI=.true.))
    call add(aSuite, TestCase1Step(REFLECT(test_MPI_HelloWorld4), passMPI=.true.))
    call add(aSuite, TestCase1Step(REFLECT(test_Broken_MPI_Method), passMPI=.true.))
    call add(aSuite, TestCase1Step(REFLECT(test_MPI_setup_teardown), passMPI=.true.))
    call add(aSuite, TestCase1Step(REFLECT(test_MPI_Suite), passMPI=.true.))
    call add(aSuite, TestCase1Step(REFLECT(test_MPI_CountTests)))

 end Function newMpi_tests

#endif

  Function testcase_tests() Result (aSuite)
    Use TestCaseTest_mod
    Type (TestSuite_type) :: aSuite

    aSuite = TestSuite('testcase')

    call add(aSuite, TestCase1Step(REFLECT(test_runTest)))
    call add(aSuite, TestCase1Step(REFLECT(test_run2Tests)))
    call add(aSuite, TestCase1Step(REFLECT(test_runSetUp)))
    call add(aSuite, TestCase1Step(REFLECT(test_runSetUpMissing)))
    call add(aSuite, TestCase1Step(REFLECT(test_runTearDown)))
    call add(aSuite, TestCase1Step(REFLECT(test_runTearDownMissing)))
    call add(aSuite, TestCase1Step(REFLECT(test_runSimpleTestCase_A)))
    call add(aSuite, TestCase1Step(REFLECT(test_runSimpleTestCase_B)))
    call add(aSuite, TestCase1Step(REFLECT(test_runSimpleTestCase_C)))
    call add(aSuite, TestCase1Step(REFLECT(test_runSimpleTestCaseFail)))
    call add(aSuite, TestCase1Step(REFLECT(test_runTestCaseFailSetUp)))
    call add(aSuite, TestCase1Step(REFLECT(test_runTestCaseFailTearDown)))
    call add(aSuite, TestCase1Step(REFLECT(test_runTestCaseSpoofFixture)))
    call add(aSuite, TestCase1Step(REFLECT(test_countTests)))

  End Function testcase_tests

  function FortranName_tests() result (aSuite)
     use test_FortranNameMangle_mod
     Use TestSuiteTest_mod
     Type (TestSuite_type) :: aSuite

     aSuite = TestSuite('testsuite')
     Call Add(aSuite, TestCase1Step(REFLECT(test_lowerCase)))
     Call Add(aSuite, TestCase1Step(REFLECT(test_fortranNameMangle_xlf)))
     Call Add(aSuite, TestCase1Step(REFLECT(test_fortranNameMangle_g95)))
     Call Add(aSuite, TestCase1Step(REFLECT(test_fortranNameMangle_nag)))
     Call Add(aSuite, TestCase1Step(REFLECT(test_isModuleProcedure)))
     Call Add(aSuite, TestCase1Step(REFLECT(test_moduleName)))
     Call Add(aSuite, TestCase1Step(REFLECT(test_procedureName)))
  end Function FortranName_tests

  Function testsuite_tests() Result (aSuite)
    Use TestSuiteTest_mod
    Type (TestSuite_type) :: aSuite

    aSuite = TestSuite('testsuite')
    call add(aSuite, TestCase1Step(REFLECT(testRunSuite)))
    call add(aSuite, TestCase1Step(REFLECT(testAddSuite)))
    call add(aSuite, TestCase1Step(REFLECT(testReportLineage)))
    call add(aSuite, TestCase1Step(REFLECT(testCountTestsA)))
    call add(aSuite, TestCase1Step(REFLECT(testCountTestsB)))
    call add(aSuite, TestCase1Step(REFLECT(testCountTestsC)))
#ifdef USE_MPI
    call add(aSuite, TestCase1Step(REFLECT(testCountTestsD)))
#endif

  End Function testsuite_tests

#ifdef USE_DSO
  function dso_tests() result (aSuite)
     use test_cString_mod
     use test_SharedObjLibUtilities_mod
     use test_pFUnitUseCase_mod
     use test_pFUnitDriver_mod
    type (TestSuite_type) :: aSuite

    aSuite = TestSuite('dsoUtils')
    Call Add(aSuite, TestCase1Step(REFLECT(test_cString)))
    Call Add(aSuite, TestCase1Step(REFLECT(test_cStringTrim)))

    Call Add(aSuite, TestCase1Step(REFLECT(test_openLibrary)))
    Call Add(aSuite, TestCase1Step(REFLECT(test_openLibraryFail)))
    Call Add(aSuite, TestCase1Step(REFLECT(test_getProcedureHandle)))
    Call Add(aSuite, TestCase1Step(REFLECT(test_getProcedureHandleFail)))
    Call Add(aSuite, TestCase1Step(REFLECT(test_ExternalSymbolsA)))
    Call Add(aSuite, TestCase1Step(REFLECT(test_ExternalSymbolsB)))
    Call Add(aSuite, TestCase1Step(REFLECT(test_F90ModuleSymbols)))

    call add(aSuite, TestCase1Step(REFLECT(test_testCase)))

    call add(aSuite, TestCase1Step(REFLECT(test_loadDsoTests)))
    call add(aSuite, TestCase1Step(REFLECT(test_runDsoTests)))
    call add(aSuite, TestCase1Step(REFLECT(test_loadTestsFromFile1)))
    call add(aSuite, TestCase1Step(REFLECT(test_loadTestsFromFile2)))

  end function dso_tests
#endif

  Function internalfile_tests() Result(aSuite)
    use TestInternalFile_mod
    type (TestSuite_type) :: aSuite

    aSuite = TestSuite('testinternalfile')
    call add(aSuite, TestCase1StepFixture(setup, teardown, REFLECT(test_NumLines)))
    call add(aSuite, TestCase1StepFixture(setup, teardown, REFLECT(test_AppendLine)))
    call add(aSuite, TestCase1StepFixture(setup, teardown, REFLECT(test_LoadFile)))
    call add(aSuite, TestCase1StepFixture(setup, teardown, REFLECT(test_WriteFile)))

  End Function internalfile_tests

  function defect_tests() result(aSuite)
    use TestDefects_mod
    type (TestSuite_type) :: aSuite

    aSuite = TestSuite('test_defectReports')
    call add(aSuite, TestCase1Step(REFLECT(testMixedTypeAssert)))

  end function defect_tests

End Program Main
