#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: TestRunner
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
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

module PF_TestRunner
   use PF_Test
   use PF_BaseTestRunner
   use PF_ResultPrinter
   use Pf_TestListenerVector
   implicit none
   private

   public :: TestRunner

   type, extends(BaseTestRunner) :: TestRunner
      private
      type (ResultPrinter) :: printer
   contains
      procedure :: run
      procedure :: runWithResult
      procedure :: createTestResult
      procedure :: addFailure
      procedure :: startTest
      procedure :: endTest
      procedure :: endRun
      procedure :: addSuccess
   end type TestRunner

   interface TestRunner
      module procedure newTestRunner_default
      module procedure newTestRunner_unit
      module procedure newTestRunner_printer
   end interface

contains

  function newTestRunner_default() result(runner)
    use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
    type (TestRunner) :: runner
    runner = TestRunner(OUTPUT_UNIT)
  end function newTestRunner_default

  function newTestRunner_unit(unit) result(runner)
    type (TestRunner) :: runner
    integer, intent(in) :: unit
    runner = TestRunner(ResultPrinter(unit))
  end function newTestRunner_unit
  
  function newTestRunner_printer(printer) result(runner)
    type (TestRunner) :: runner
    type(ResultPrinter), intent(in) :: printer
    runner%printer = printer
  end function newTestRunner_printer

   function createTestResult(this) result(tstResult)
      use PF_TestResult
      class (TestRunner), target, intent(inout) :: this
      type (TestResult) :: tstResult

      integer :: i
      type(TestListenerVector), pointer :: listeners
      _UNUSED_DUMMY(this)

      tstResult = TestResult()
      listeners => this%get_listeners()
      do i = 1, listeners%size()
         call tstResult%addListener(listeners%at(i))
      end do

    end function createTestResult

    recursive function run(this, aTest, context) result(result)
      use PF_Test
      use PF_TestSuite
      use PF_TestCase
      use PF_TestResult
      use PF_ParallelContext

      type (TestResult) :: result
      class (TestRunner), target, intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context
      
      integer :: clock_start
      integer :: clock_stop
      integer :: clock_rate
      real :: elapsed_time

      call system_clock(clock_start)

      result = this%createTestResult()
      call result%addListener(this%printer)
      call result%setName(aTest%getName())

      call this%runWithResult(aTest, context, result)

      call system_clock(clock_stop, clock_rate)
      elapsed_time = real(clock_stop - clock_start) / clock_rate
      call result%addRunTime(elapsed_time)

      if (context%isRootProcess())  then
         call this%printer%print(result, elapsed_time)
      end if

   end function run

   recursive subroutine runWithResult(this, aTest, context, result)
     use PF_ParallelContext
     use PF_TestResult
     class (TestRunner), target, intent(inout) :: this
     class (Test), intent(inout) :: aTest
     class (ParallelContext), intent(in) :: context
     type (TestResult), intent(inout) :: result

     _UNUSED_DUMMY(this)
     
     call aTest%run(result, context)
     
   end subroutine runWithResult


! Recall, runner is also a listener and these will be called from
! TestResult, adding the ability to put in functionality here. In
! addition to other listeners added above.
!
    subroutine startTest(this, testName)
       class (TestRunner), intent(inout) :: this
       character(len=*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)

    end subroutine startTest

    subroutine endTest(this, testName)
       class (TestRunner), intent(inout) :: this
       character(len=*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)

    end subroutine endTest

    subroutine endRun(this, result)
      use PF_AbstractTestResult, only : AbstractTestResult
      class (TestRunner), intent(inout) :: this
      class (AbstractTestResult), intent(in) :: result

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(result)

    end subroutine endRun

    subroutine addFailure(this, testName, exceptions)
       use PF_ExceptionList
       class (TestRunner), intent(inout) :: this
       character(len=*), intent(in) :: testName
       type (ExceptionList), intent(in) :: exceptions

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
      _UNUSED_DUMMY(exceptions)


    end subroutine addFailure


   subroutine addSuccess(this, testName)
      class (TestRunner), intent(inout) :: this
      character(*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)

   end subroutine addSuccess

end module PF_TestRunner
