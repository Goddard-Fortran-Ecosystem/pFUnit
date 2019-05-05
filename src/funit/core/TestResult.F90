!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: TestResult
!
!> @brief
!! <BriefDescription>
!! Note: A possible extension point for user-specialized TestResults.
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
module PF_TestResult
   use PF_AbstractTestResult
   use PF_SurrogateTestCase
   use PF_TestListener
   use PF_TestListenerVector
   use PF_TestFailure
   use PF_TestFailureVector

   implicit none
   private

   public :: TestResult

#ifndef DEFERRED_LENGTH_CHARACTER
   integer, parameter :: MAX_LENGTH_NAME = 64
#endif

   type, extends(AbstractTestResult) :: TestResult
      private
      integer :: numRun = 0
      integer :: numDisabled = 0
      real    :: runTime
      type (TestListenerVector) :: listeners
      type (TestFailureVector) :: failures
      type (TestFailureVector) :: errors
      type (TestFailureVector) :: successes
#ifdef DEFERRED_LENGTH_CHARACTER
      character(:), allocatable :: name
#else
      character(len=MAX_LENGTH_NAME) :: name
#endif
   contains
      procedure :: addFailure
      procedure :: addError
      procedure :: addSuccess
      procedure :: zeroRunTime
      procedure :: addRunTime
      procedure :: getRunTime
      procedure :: failureCount
      procedure :: errorCount
      procedure :: disableCount
      procedure :: startTest
      procedure :: disableTest
      procedure :: endTest
      procedure :: runCount
      procedure :: run
      procedure :: addListener
      procedure :: wasSuccessful
      procedure :: getSuccesses
      procedure :: getErrors
      procedure :: getFailures
      procedure :: getIthFailure
      procedure :: getName
      procedure :: setName
   end type TestResult

   interface TestResult
      module procedure new_TestResult
   end interface TestResult

contains

   function new_TestResult(name)
      type (TestResult) :: new_TestResult
      character(len=*), intent(in), optional :: name
      new_TestResult%numRun = 0
      new_TestResult%runTime = 0
      if(present(name)) then
         new_TestResult%name = name
      else
         new_TestResult%name = 'default_suite_name'
      end if
   end function new_TestResult

   subroutine addFailure(this, aTest, exceptions)
      use PF_ExceptionList
      use PF_TestFailure
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest
      type (ExceptionList), intent(in) :: exceptions

      integer :: i
      class (TestListener), pointer :: pListener

      call this%failures%push_back(TestFailure(aTest%getName(), exceptions))

      do i = 1, this%listeners%size()
         pListener => this%listeners%at(i)
         call pListener%addFailure(aTest%getName(), exceptions)
      end do

   end subroutine addFailure

   subroutine addError(this, aTest, exceptions)
      use PF_TestFailure
      use PF_ExceptionList
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest
      type (ExceptionList), intent(in) :: exceptions

      integer :: i
      class (TestListener), pointer :: pListener

      call this%errors%push_back(TestFailure(aTest%getName(), exceptions))

      do i = 1, this%listeners%size()
         pListener => this%listeners%at(i)
         call pListener%addError(aTest%getName(), exceptions)
      end do

   end subroutine addError

   subroutine addSuccess(this, aTest)
      use PF_TestFailure
      use PF_ExceptionList
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest

      type (ExceptionList) :: noExceptions ! empty

      call this%successes%push_back(TestFailure(aTest%getName(), noExceptions))

   end subroutine addSuccess

   integer function failureCount(this)
      class (TestResult), intent(in) :: this
      failureCount = this%failures%size()
   end function failureCount

   integer function errorCount(this)
      class (TestResult), intent(in) :: this
      errorCount = this%errors%size()
   end function errorCount

   integer function disableCount(this)
      class (TestResult), intent(in) :: this
      disableCount = this%numDisabled
   end function disableCount

   subroutine startTest(this, aTest)
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest

      integer :: i
      class (TestListener), pointer :: listener

      this%numRun = this%numRun + 1

      do i = 1, this%listeners%size()
         listener => this%listeners%at(i)
         call listener%startTest(aTest%getName())
      end do

   end subroutine startTest

   subroutine disableTest(this, aTest)
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest

      integer :: i
      class (TestListener), pointer :: listener

      this%numDisabled = this%numDisabled + 1

      do i = 1, this%listeners%size()
         listener => this%listeners%at(i)
         call listener%disableTest(aTest%getName())
      end do

   end subroutine disableTest

   subroutine endTest(this, aTest)
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest

      integer :: i
      class (TestListener), pointer :: listener

      do i = 1, this%listeners%size()
         listener => this%listeners%at(i)
         call listener%endTest(aTest%getName())
      end do

   end subroutine endTest

   integer function runCount(this)
      class (TestResult), intent(in) :: this
      runCount = this%numRun
   end function runCount

! only invoked for a "real" test, not suites etc.
   recursive subroutine run(this, test, context)
      use PF_Exception
      use PF_ExceptionList
      use PF_ParallelContext
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(inout) :: test 
      class (ParallelContext), intent(in) :: context


      if (test%is_disabled()) then
         call this%disableTest(test)
         return
      end if

      if (context%isRootProcess()) call this%startTest(test)

      call test%runBare()

      if (context%isRootProcess()) then
         if (anyErrors()) then
            call this%addError(test, getExceptions())
         elseif (anyExceptions()) then
            call this%addFailure(test, getExceptions())
         else
            call this%addSuccess(test)
         end if
      end if

      if (context%isRootProcess()) call this%endTest(test)

   end subroutine run

   subroutine addListener(this, listener)
      use PF_TestListener, only: TestListener
      class (TestResult), intent(inOut) :: this
      class (TestListener), target, intent(in) :: listener

      call this%listeners%push_back(listener)

   end subroutine addListener

   ! Accessor - only needed for testing purposes
   function getIthFailure(this, i) result(failure)
      class (TestResult), intent(in) :: this
      integer, intent(in) :: i
      type (TestFailure), pointer :: failure

      failure => this%failures%at(i)

   end function getIthFailure

   logical function wasSuccessful(this)
      class (TestResult), intent(in) :: this
      wasSuccessful = (this%failureCount() ==  0) .and. (this%errorCount() == 0)
   end function wasSuccessful

   function getSuccesses(this) result(successes)
     class (TestResult), target, intent(in) :: this
     type (TestFailureVector), pointer :: successes
     successes => this%successes
   end function getSuccesses

   function getErrors(this) result(errors)
      class (TestResult), target, intent(in) :: this
      type (TestFailureVector), pointer :: errors
      errors => this%errors
   end function getErrors

   function getFailures(this) result(failures)
      class (TestResult), target, intent(in) :: this
      type (TestFailureVector), pointer :: failures
      failures => this%failures
   end function getFailures

   subroutine zeroRunTime(this)
     class (TestResult), intent(inout) :: this
     this%runTime = 0
   end subroutine zeroRunTime

   subroutine addRunTime(this, time)
     class (TestResult), intent(inout) :: this
     real, intent(in) :: time
     this%runTime = this%runTime + time
   end subroutine addRunTime

   function getRunTime(this) result(duration)
     class (TestResult), intent(in) :: this
     real :: duration
     duration = this%runTime
   end function getRunTime

   function getName(this) result(name)
      class (TestResult), intent(in) :: this
      character(:), allocatable :: name
      name = this%name
   end function getName

   subroutine setName(this, name)
      class (TestResult), intent(inout) :: this
      character(len=*),intent(in) :: name

      this%name = trim(name)
    end subroutine setName


end module PF_TestResult
