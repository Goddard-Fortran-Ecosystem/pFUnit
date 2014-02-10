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
   use BaseTestRunner_mod
   use TestListener_mod
   use AbstractPrinter_mod
   use ResultPrinter_mod
   implicit none
   private

   public :: TestRunner
   public :: newTestRunner

   type, extends(BaseTestRunner) :: TestRunner
      type (PrinterPointer), allocatable :: printers(:)
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
      type (TestRunner) :: runner
      allocate(runner%printers(0))
   end function newTestRunner_default

   function newTestRunner_unit(printers) result(runner)
      type(PrinterPointer), intent(in) :: printers(:)
      type (TestRunner) :: runner
      allocate(runner%printers(size(printers)), source=printers)
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
      integer :: i

      type (DebugListener) :: debug

      call system_clock(clockStart)
      result = this%createTestResult()
      do i=1,size(this%printers)
         call result%addListener(this%printers(i)%pPrinter)
      end do
#ifdef DEBUG_ON
      call result%addListener(debug)
#endif
      call aTest%run(result, context)
      call system_clock(clockStop, clockRate)
      runTime = real(clockStop - clockStart) / clockRate
      if (context%isRootProcess())  then
         do i=1,size(this%printers)
            call this%printers(i)%pPrinter%print(result, runTime)
         end do
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
