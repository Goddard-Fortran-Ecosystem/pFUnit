program main
   use pfunit, only: initialize
   use pfunit, only: finalize
   use pfunit, only: TestResult
   use pfunit, only: ListenerPointer
   use pfunit, only: newResultPrinter
   use pfunit, only: ResultPrinter
!$$   use pfunit, only: DebugListener
   implicit none

   logical :: success

   call initialize()
   success = runTests()
   call finalize(success)

contains

   logical function runTests() result(success)
      use pfunit, only: newTestSuite
      use pfunit, only: TestSuite
      use pfunit, only: TestRunner, newTestRunner
      use pfunit, only: MpiContext, newMpiContext
      use pfunit, only: ParallelContext

      use Test_StringUtilities_mod, only: StringUtilitiesSuite => suite    ! (1)
      use Test_UnixProcess_mod, only: unixProcessSuite => suite                ! (1)

      use Test_Exception_mod, only: exceptionSuite => suite                ! (2)
      use Test_AssertBasic_mod, only: assertBasicSuite => suite            !
      use Test_Assert_mod, only: assertSuite => suite                      ! (3)
      use Test_AssertInteger_mod, only: assertIntegerSuite => suite        !

      use Test_AssertReal_mod, only: assertRealSuite => suite              ! (5)
      use Test_AssertComplex_mod, only: assertComplexSuite => suite              ! (5)

      use Test_TestResult_mod, only: testResultSuite => suite              ! (6)
      use Test_TestSuite_mod, only: testTestSuiteSuite => suite                ! (7)

      use Test_TestMethod_mod, only: testTestMethodSuite => suite  ! (8)
      use Test_SimpleTestCase_mod, only: testSimpleTestCaseSuite => suite          ! (9)
      use Test_FixtureTestCase_mod, only: testFixtureTestCaseSuite => suite        ! (10)

      use Test_BasicOpenMP_mod, only: testBasicOpenMpSuite => suite  ! (8)

      use Test_MockCall_mod, only: testMockCallSuite => suite      ! (11)
      use Test_MockRepository_mod, only: testMockRepositorySuite => suite      ! (11)
      use Test_XmlPrinter_mod, only: testXmlPrinterSuite => suite

      use Test_RobustRunner_mod, only: testRobustRunnerSuite => suite

      use Test_MpiContext_mod, only: MpiContextSuite => suite
      use Test_MpiException_mod, only: MpiExceptionSuite => suite
      use Test_MpiTestCase_mod, only: MpiTestCaseSuite => suite
      use Test_MpiParameterizedTestCase_mod, only: MpiParameterizedTestCaseSuite => suite
      use iso_fortran_env, only: OUTPUT_UNIT

      type (TestSuite) :: allTests
      type (TestRunner) :: runner
      type (TestResult) :: tstResult

#ifdef INTEL_13
      type (ResultPrinter), target :: printer
#endif
      type (ListenerPointer), target, allocatable :: ll(:)

#ifndef INTEL_13
      allocate(ll(1))
      allocate(ll(1)%pListener, source=newResultPrinter(OUTPUT_UNIT))
      ! TODO: We'll make this a feature in 4.0
!!$      allocate(ll(2))
!!$      allocate(ll(1)%pListener, source=newResultPrinter(OUTPUT_UNIT))
!!$      allocate(ll(2)%pListener, source=DebugListener())
#else
      allocate(ll(1))
      printer = newResultPrinter(OUTPUT_UNIT)
      ll(1)%pListener => printer
#endif

      allTests = newTestSuite('allTests')
      runner = newTestRunner(ll)

#define ADD(suite) call allTests%addTest(suite())

      ADD(StringUtilitiesSuite)
      ADD(UnixProcessSuite)
      ADD(exceptionSuite)

      ADD(assertBasicSuite)
      ADD(assertSuite)
      ADD(assertIntegerSuite)

      ADD(assertRealSuite)
      ADD(assertComplexSuite)

      ADD(testResultSuite)
      ADD(testTestSuiteSuite)

      ADD(testTestMethodSuite)
      ADD(testSimpleTestCaseSuite)
      ADD(testFixtureTestCaseSuite)

      ADD(testBasicOpenMpSuite)

      ADD(testMockCallSuite)
      ADD(testMockRepositorySuite)

      ADD(testXmlPrinterSuite)

      ADD(testRobustRunnerSuite)

      ADD(MpiContextSuite)
      ADD(MpiExceptionSuite)
      ADD(MpiTestCaseSuite)
      ADD(MpiParameterizedTestCaseSuite)

      tstResult = runner%run(allTests, newMpiContext())

      success = tstResult%wasSuccessful()

  end function runTests

end program main


