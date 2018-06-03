!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: SubsetRunner
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
! and monitors a separate process which runs a SubsetRunner.  If the
! SubsetRunner crashes, then RobustRunner detects this and relaunches
! - skipping the earlier tests and the test that crashed.  The
! algorithm is guaranteed to eventually provide a result for every
! test.
!
! Both RobustRunner and SubsetRunner work with a flat list of test
! cases obtained through TestSuite::getTestCases().  This greatly
! simplifies the task of managing the interactions between
! RobustRunner and SubsetRunner.
! -----------------------------------------------------------------------

module PF_SubsetRunner_mod
   use PF_Test_mod
   use PF_BaseTestRunner_mod
   implicit none
   private

   public :: SubsetRunner


   integer, parameter :: MAX_LEN_NAME=80
   type, extends(BaseTestRunner) :: SubsetRunner
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
   end type SubsetRunner

   interface SubsetRunner
      module procedure newSubsetRunner_stdout
      module procedure newSubsetRunner
   end interface SubsetRunner

contains

   function newSubsetRunner_stdout(numSkip) result(runner)
      use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
      type (SubsetRunner) :: runner
      integer, intent(in) :: numSkip

      runner%numSkip = numSkip
      runner%unit = OUTPUT_UNIT

   end function newSubsetRunner_stdout

   function newSubsetRunner(numSkip, unit) result(runner)
      type (SubsetRunner) :: runner
      integer, intent(in) :: numSkip
      integer, intent(in) :: unit

      runner%numSkip = numSkip
      runner%unit = unit

   end function newSubsetRunner

   function run(this, aTest, context) result(result)
      use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
      use PF_Test_mod
      use PF_TestVector_mod
      use PF_ParallelContext_mod
      use PF_TestCase_mod
      use PF_TestResult_mod
      use PF_TestSuite_mod

      type (TestResult) :: result
      class (SubsetRunner), target, intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context

      type (TestVector)  :: testCaseList
      class (Test), pointer :: t
      integer :: i

      !print *,'a00000'
      
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
      
      write(this%unit,'(a)') '*LAUNCHED*'

      do i = this%numSkip + 1, testCaseList%size()
         t => testCaseList%at(i)
         call t%run(result, context)
      end do

      if (this%unit /= OUTPUT_UNIT) close(this%unit)

   end function run

   subroutine addFailure(this, testName, exceptions)
      use PF_Exception_mod
      use PF_ExceptionList_mod
      use, intrinsic :: iso_c_binding
      class (SubsetRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

      integer :: i
      class (Exception), pointer :: e

      write(this%unit,'(a,i0)')'failed: numExceptions=',exceptions%size()
      do i = 1, exceptions%size()
         e => exceptions%at(i)
         associate(fileName => e%location%fileName, lineNumber => e%location%lineNumber)
           write(this%unit,'(i0,a,i0,a)')i,' len(fileName)=< ',len_trim(fileName),' >'
           write(this%unit,'(i0,a,a,a)')i,' fileName=< ',trim(fileName),' >'
           write(this%unit,'(i0,a,i0,a)')i,' lineNumber=< ',lineNumber,' >'
           write(this%unit,'(i0,a,i0,a)')i,' len(message)=< ', &
                & len_trim(e%message),' >'
           write(this%unit,'(i0,a,a,a)')i,' message=< ', &
                trim(e%message),' >'//C_NULL_CHAR
         end associate
      end do

   end subroutine addFailure

   subroutine startTest(this, testName)
      class (SubsetRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      write(this%unit,'(a,a)')'started: ', trim(testName)

   end subroutine startTest

   subroutine endTest(this, testName)
      class (SubsetRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      write(this%unit,'(a,a)')'ended: ', trim(testName)
   end subroutine endTest

   subroutine endRun(this, result)
     use PF_AbstractTestResult_mod, only : AbstractTestResult
     class (SubsetRunner), intent(inout) :: this
     class (AbstractTestResult), intent(in) :: result
   end subroutine endRun

   subroutine addSuccess(this, testName)
      class (SubsetRunner), intent(inout) :: this
      character(*), intent(in) :: testName
   end subroutine addSuccess
   
end module PF_SubsetRunner_mod
