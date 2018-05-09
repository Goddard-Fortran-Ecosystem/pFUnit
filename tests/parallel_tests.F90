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

      ADD(MpiContextSuite)
      ADD(MpiExceptionSuite)
      ADD(MpiTestCaseSuite)
      ADD(MpiParameterizedTestCaseSuite)

      tstResult = runner%run(allTests, newMpiContext())

      success = tstResult%wasSuccessful()

  end function runTests

end program main


