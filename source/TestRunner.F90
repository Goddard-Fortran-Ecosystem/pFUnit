module TestRunner_mod
   use TestListener_mod
   use ResultPrinter_mod
   implicit none
   private

   public :: TestRunner
   public :: newTestRunner

   type, extends(TestListener) :: TestRunner
      type (ResultPrinter) :: printer
   contains
      procedure :: run
      procedure :: createTestResult
      procedure :: addFailure
      procedure :: startTest
      procedure :: endTest
   end type TestRunner

   interface newTestRunner
      module procedure newTestRunner_default
      module procedure newTestRunner_unit
   end interface

contains

   function newTestRunner_default() result(runner)
      use ISO_FORTRAN_ENV, only: OUTPUT_UNIT
      type (TestRunner) :: runner
      runner = newTestRunner(OUTPUT_UNIT)
   end function newTestRunner_default

   function newTestRunner_unit(unit) result(runner)
      integer, intent(in) :: unit
      type (TestRunner) :: runner
      runner%printer = newResultPrinter(unit)
   end function newTestRunner_unit

   function createTestResult(this) result(tstResult)
      use TestResult_mod
      class (TestRunner), intent(inout) :: this
      type (TestResult) :: tstResult
      tstResult = newTestResult()
    end function createTestResult

    subroutine run(this, aTest, context)
      use Test_mod
      use TestResult_mod
      use ParallelContext_mod
      use DebugListener_mod
      class (TestRunner), intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context

      type (TestResult) :: result
      integer :: clockStart
      integer :: clockStop
      integer :: clockRate
      real :: runTime

      result = this%createTestResult()
      call result%addListener(this%printer)
      call system_clock(clockStart)
      call aTest%run(result, context)
      call system_clock(clockStop, clockRate)
      runTime = real(clockStop - clockStart) / clockRate
      if (context%isRootProcess())  then
         call this%printer%print(result, runTime)
      end if

   end subroutine run

    subroutine startTest(this, testName)
       class (TestRunner), intent(inout) :: this
       character(len=*), intent(in) :: testName
    end subroutine startTest

    subroutine endTest(this, testName)
       class (TestRunner), intent(inout) :: this
       character(len=*), intent(in) :: testName
    end subroutine endTest

    subroutine addFailure(this, testName, exceptions)
       use Exception_mod
       class (TestRunner), intent(inout) :: this
       character(len=*), intent(in) :: testName
       type (Exception), intent(in) :: exceptions(:)
    end subroutine addFailure

end module TestRunner_mod
