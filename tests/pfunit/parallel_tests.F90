program main
   use pfunit, only: initialize
   use pfunit, only: finalize
   use pfunit, only: TestResult
   use pfunit, only: TestListenerVector
   use pfunit, only: stub
!$$   use pfunit, only: DebugListener
   implicit none

   logical :: success

   call initialize(stub)
   success = runTests()
   call finalize(stub, success)

contains

   logical function runTests() result(success)
      use pfunit, only: TestSuite
      use pfunit, only: TestRunner
      use pfunit, only: MpiContext
      use pfunit, only: ParallelContext

      use Test_MpiContext, only: MpiContextSuite => suite
      use Test_MpiException, only: MpiExceptionSuite => suite
      use Test_MpiTestCase, only: MpiTestCaseSuite => suite
      use Test_MpiParameterizedTestCase, only: MpiParameterizedTestCaseSuite => suite
      use iso_fortran_env, only: OUTPUT_UNIT

      type (TestSuite) :: allTests
      type (TestRunner) :: runner
      type (TestResult) :: tstResult


      allTests = TestSuite('allTests')
      runner = TestRunner()

#define ADD(suite) call allTests%addTest(suite())

      ADD(MpiContextSuite)
      ADD(MpiExceptionSuite)
      ADD(MpiTestCaseSuite)
      ADD(MpiParameterizedTestCaseSuite)

      tstResult = runner%run(allTests, MpiContext())

      success = tstResult%wasSuccessful()

  end function runTests

end program main


