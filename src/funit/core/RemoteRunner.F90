#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: RemoteRunner
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
!------------------------------------------------------------------------
! The purpose of this class is to support detection of SUT errors that
! crash the framework.  The RobustRunner (better name?) class launches
! and monitors a separate process which runs a RemoteRunner.  If the
! RemoteRunner crashes, then RobustRunner detects this and relaunches
! - skipping the earlier tests and the test that crashed.  The
! algorithm is guaranteed to eventually provide a result for every
! test.
!
! Both RobustRunner and RemoteRunner work with a flat list of test
! cases obtained through TestSuite::getTestCases().  This greatly
! simplifies the task of managing the interactions between
! RobustRunner and RemoteRunner.
! -----------------------------------------------------------------------

module PF_RemoteRunner
   use PF_Test
   use PF_BaseTestRunner
   use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
   implicit none
   private

   public :: RemoteRunner


   integer, parameter :: MAX_LEN_NAME=80
   type, extends(BaseTestRunner) :: RemoteRunner
      private
      integer :: numSkip
      integer :: unit
   contains
      procedure :: run
      procedure :: addFailure
      procedure :: startTest
      procedure :: endTest
      procedure :: endRun
      procedure :: addSuccess
   end type RemoteRunner

   interface RemoteRunner
      module procedure newRemoteRunner_stdout
      module procedure newRemoteRunner
   end interface RemoteRunner

contains

   function newRemoteRunner_stdout(numSkip) result(runner)
      use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
      type (RemoteRunner) :: runner
      integer, intent(in) :: numSkip

      runner%numSkip = numSkip
      runner%unit = OUTPUT_UNIT

   end function newRemoteRunner_stdout

   function newRemoteRunner(numSkip, unit) result(runner)
      type (RemoteRunner) :: runner
      integer, intent(in) :: numSkip
      integer, intent(in) :: unit

      runner%numSkip = numSkip
      runner%unit = unit

   end function newRemoteRunner

   function run(this, aTest, context) result(result)
      use PF_Test
      use PF_TestVector
      use PF_ParallelContext
      use PF_TestCase
      use PF_TestResult
      use PF_TestSuite

      type (TestResult) :: result
      class (RemoteRunner), target, intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context

      type (TestVector)  :: testCaseList
      class (Test), pointer :: t
      integer :: i

      select type (aTest)
      class is (TestSuite)
         call aTest%getTestCases(testCaseList)
      class is (TestCase)
         call testCaseList%push_back(aTest)
      class default
         stop
      end select

      result = TestResult()
      call result%setName(aTest%getName())
      call result%addListener( this )

      ! This should be a named pipe
      ! Note - uses F2008 extension:  "newunit=..."

      write(this%unit,'(a)',advance='no') '*LAUNCHED*' // C_NULL_CHAR
      flush(this%unit)

      do i = this%numSkip + 1, testCaseList%size()
         t => testCaseList%at(i)
         call t%run(result, context)
      end do

   end function run

   subroutine addFailure(this, testName, exceptions)
      use PF_Exception
      use PF_ExceptionList
      use, intrinsic :: iso_c_binding
      class (RemoteRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

      integer :: i
      class (Exception), pointer :: e

      _UNUSED_DUMMY(testName)

      write(this%unit,'(a,i0,a)',advance='no') &
           & 'failed: numExceptions=<<<',exceptions%size(),'>>>'//C_NULL_CHAR
      flush(this%unit)
      do i = 1, exceptions%size()
         e => exceptions%at(i)
         associate(fileName => e%location%fileName, lineNumber => e%location%lineNumber)
           write(this%unit,'(i0,a,a,a)',advance='no')i,' fileName=<<< ',trim(fileName),' >>>'//C_NULL_CHAR
      flush(this%unit)
           write(this%unit,'(i0,a,i0,a)',advance='no')i,' lineNumber=<<< ',lineNumber,' >>>'//C_NULL_CHAR
      flush(this%unit)
           write(this%unit,'(a,a,a)',advance='no')'message=<<< ', &
                trim(e%message),' >>>'//C_NULL_CHAR
      flush(this%unit)
         end associate
      end do
      flush(this%unit)

   end subroutine addFailure

   subroutine startTest(this, testName)
      class (RemoteRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName

      write(this%unit,'(a,a)',advance='no')'started: ', trim(testName) // C_NULL_CHAR
      flush(this%unit)

   end subroutine startTest

   subroutine endTest(this, testName)
      class (RemoteRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName

      write(this%unit,'(a,a)',advance='no')'ended: ', trim(testName)//C_NULL_CHAR
      flush(this%unit)

   end subroutine endTest

   subroutine endRun(this, result)
     use PF_AbstractTestResult, only : AbstractTestResult
     class (RemoteRunner), intent(inout) :: this
     class (AbstractTestResult), intent(in) :: result

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(result)
   end subroutine endRun

   subroutine addSuccess(this, testName)
      class (RemoteRunner), intent(inout) :: this
      character(*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)

   end subroutine addSuccess
   
end module PF_RemoteRunner
