program main
   use FUnit, only: initialize
   use FUnit, only: finalize
   use FUnit, only: TestResult
   use FUnit, only: stub
!$$   use FUnit, only: DebugListener
   implicit none

   logical :: success

   call initialize(stub)
   success = runTests()
   call finalize(stub, success)

contains

   logical function runTests() result(success)
      use FUnit, only: TestSuite
      use FUnit, only: TestRunner, TestRunner
      use FUnit, only: SerialContext

      use Test_StringUtilities, only: StringUtilitiesSuite => suite    ! (1)
      use Test_UnixProcess, only: unixProcessSuite => suite                ! (1)
      use Test_Exception, only: exceptionSuite => suite                ! (2)
      use Test_AssertBasic, only: assertBasicSuite => suite            !
      use Test_Assert, only: assertSuite => suite                      ! (3)

      use Test_TestResult, only: testResultSuite => suite              ! (6)
      use Test_TestSuite, only: testTestSuiteSuite => suite                ! (7)

      use Test_TestMethod, only: testTestMethodSuite => suite  ! (8)
      use Test_SimpleTestCase, only: testSimpleTestCaseSuite => suite          ! (9)
      use Test_FixtureTestCase, only: testFixtureTestCaseSuite => suite        ! (10)


!$    use Test_BasicOpenMP, only: testBasicOpenMpSuite => suite  ! (8)

      use Test_MockCall, only: testMockCallSuite => suite      ! (11)
      use Test_MockRepository, only: testMockRepositorySuite => suite      ! (11)
      use Test_XmlPrinter, only: testXmlPrinterSuite => suite

      use Test_RobustRunner, only: testRobustRunnerSuite => suite


      type (TestSuite) :: allTests
      type (TestRunner) :: runner
      type (TestResult) :: tstResult

      allTests = TestSuite('allTests')
      runner = TestRunner()

#define ADD(suite) call allTests%addTest(suite())

      ADD(StringUtilitiesSuite)
      ADD(UnixProcessSuite)
      ADD(exceptionSuite)
      ADD(assertBasicSuite)
      ADD(assertSuite)

!!$      ADD(assertComplexSuite)

      ADD(testResultSuite)
      ADD(testTestSuiteSuite)

      ADD(testTestMethodSuite)
      ADD(testSimpleTestCaseSuite)
      ADD(testFixtureTestCaseSuite)

!$    ADD(testBasicOpenMpSuite)

      ADD(testMockCallSuite)
      ADD(testMockRepositorySuite)

      ADD(testXmlPrinterSuite)
      ADD(testRobustRunnerSuite)

      tstResult = runner%run(allTests, SerialContext())

      success = tstResult%wasSuccessful()

  end function runTests

end program main


