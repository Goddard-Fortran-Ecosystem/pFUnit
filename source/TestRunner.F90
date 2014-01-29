!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: TestRunner
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------

module TestRunner_mod
   use Test_mod
   use BaseTestRunner_mod
   use TestListener_mod
   use BaseTestRunner_mod
   use ResultPrinter_mod
   use DebugListener_mod
   implicit none
   private

   public :: TestRunner
   public :: newTestRunner

   type, extends(BaseTestRunner) :: TestRunner
!!$      private
      type (ResultPrinter) :: printer
      type (DebugListener) :: debugger
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
      use iso_fortran_env, only: OUTPUT_UNIT
      type (TestRunner) :: runner
      runner = newTestRunner(OUTPUT_UNIT)
   end function newTestRunner_default

   function newTestRunner_unit(unit) result(runner)
      integer, intent(in) :: unit
      type (TestRunner) :: runner
      runner%printer = newResultPrinter(unit)
      runner%debugger = DebugListener(unit)
   end function newTestRunner_unit

   function createTestResult(this) result(tstResult)
      use TestResult_mod
      class (TestRunner), intent(inout) :: this
      type (TestResult) :: tstResult

      tstResult = newTestResult()
      call tstResult%addListener(this%printer)
      if (this%debug()) call tstResult%addListener(this%debugger)

    end function createTestResult

    function run(this, aTest, context) result(result)
      use Test_mod
      use TestResult_mod
      use ParallelContext_mod
      use DebugListener_mod

      type (TestResult) :: result
      class (TestRunner), intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context

      integer :: clockStart
      integer :: clockStop
      integer :: clockRate
      real :: runTime


      call system_clock(clockStart)

      result = this%createTestResult()

      call aTest%run(result, context)
      call system_clock(clockStop, clockRate)
      runTime = real(clockStop - clockStart) / clockRate
      if (context%isRootProcess())  then
         call this%printer%print(result, runTime)
      end if

   end function run

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
