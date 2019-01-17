program main
   use pfunit, only: initialize
   use pfunit, only: finalize
   use pfunit, only: TestResult
   use pfunit, only: TestListenerVector
   use pfunit, only: ResultPrinter
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
      type (TestListenerVector) :: ll

      ! TODO: We'll make this a feature in 4.0
!!$      allocate(ll(2))
!!$      allocate(ll(1)%pListener, source=ResultPrinter(OUTPUT_UNIT))
!!$      allocate(ll(2)%pListener, source=DebugListener())
      call ll%push_back(ResultPrinter(OUTPUT_UNIT))

      allTests = TestSuite('allTests')
      runner = TestRunner(ll)

#define ADD(suite) call allTests%addTest(suite())

      ADD(MpiContextSuite)
      ADD(MpiExceptionSuite)
      ADD(MpiTestCaseSuite)
      ADD(MpiParameterizedTestCaseSuite)

      tstResult = runner%run(allTests, MpiContext())

      success = tstResult%wasSuccessful()

  end function runTests

end program main


