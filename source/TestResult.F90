!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: TestResult
!
!> @brief
!! <BriefDescription>
!! Note: A possible extension point for user-specialized TestResults.
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
module TestResult_mod
   use SurrogateTestCase_mod
   use TestListener_mod
   use TestFailure_mod
   implicit none
   private

   public :: TestResult
   public :: newTestResult

   type :: TestResult
      integer :: numFailed
      integer :: numErrors
      integer :: numRun
      type (ListenerPointer), allocatable :: listeners(:)
      type (TestFailure), allocatable :: failures(:)
      type (TestFailure), allocatable :: errors(:)
   contains
      procedure :: addFailure
      procedure :: addError
      procedure :: failureCount
      procedure :: errorCount
      procedure :: getIthFailure
      procedure :: startTest
      procedure :: endTest
      procedure :: runCount
      procedure :: run
      procedure :: addListener
      procedure :: wasSuccessful
   end type TestResult

contains

   function newTestResult()
      type (TestResult), pointer :: newTestResult
      allocate(newTestResult)
      allocate(newTestResult%listeners(0))
      allocate(newTestResult%failures(0))
      allocate(newTestResult%errors(0))
      newTestResult%numFailed = 0
      newTestResult%numErrors = 0
      newTestResult%numRun = 0
   end function newTestResult

   subroutine addFailure(this, aTest, exceptions)
      use Exception_mod, only: Exception
      use TestFailure_mod
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest
      type (Exception), intent(in) :: exceptions(:)

      integer :: i, n
      type (TestFailure), allocatable :: tmp(:)

      n = this%numFailed
      allocate(tmp(n))
      tmp(1:n) = this%failures(1:n)
      deallocate(this%failures)
      allocate(this%failures(n+1))
      this%failures(1:n) = tmp
      deallocate(tmp)
      this%failures(n+1) = TestFailure(aTest%getName(), exceptions)

      this%numFailed = n + 1
      do i = 1, size(this%listeners)
         call this%listeners(i)%pListener%addFailure(aTest%getName(), exceptions)
      end do

   end subroutine addFailure

   subroutine addError(this, aTest, exceptions)
      use Exception_mod, only: Exception
      use TestFailure_mod
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest
      type (Exception), intent(in) :: exceptions(:)

      integer :: i, n
      type (TestFailure), allocatable :: tmp(:)

      n = this%numErrors
      allocate(tmp(n))
      tmp(1:n) = this%errors(1:n)
      deallocate(this%errors)
      allocate(this%errors(n+1))
      this%errors(1:n) = tmp
      deallocate(tmp)
      this%errors(n+1) = TestFailure(aTest%getName(), exceptions)

      this%numErrors = n + 1
      do i = 1, size(this%listeners)
         call this%listeners(i)%pListener%addError(aTest%getName(), exceptions)
      end do

   end subroutine addError

   integer function failureCount(this)
      class (TestResult), intent(in) :: this
      failureCount = this%numFailed
   end function failureCount

   integer function errorCount(this)
      class (TestResult), intent(in) :: this
      errorCount = this%numErrors
   end function errorCount

   subroutine startTest(this, aTest)
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest

      integer :: i

      this%numRun = this%numRun + 1
      do i = 1, size(this%listeners)
         call this%listeners(i)%pListener%startTest(aTest%getName())
      end do

   end subroutine startTest

   subroutine endTest(this, aTest)
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase), intent(in) :: aTest

      integer :: i

      do i = 1, size(this%listeners)
         call this%listeners(i)%pListener%endTest(aTest%getName())
      end do
   end subroutine endTest

   integer function runCount(this)
      class (TestResult), intent(in) :: this
      runCount = this%numRun
   end function runCount

   recursive subroutine run(this, test, context)
      use Exception_mod
      use ParallelContext_mod
      class (TestResult), intent(inout) :: this
      class (SurrogateTestCase) :: test
      class (ParallelContext), intent(in) :: context

      type (Exception) :: anException

      if (context%isRootProcess()) call this%startTest(test)

      call test%runBare()

      if (context%isRootProcess()) then
         if (anyErrors()) then
            call this%addError(test, getExceptions())
         elseif (anyExceptions()) then
            call this%addFailure(test, getExceptions())
         end if
      end if

      if (context%isRootProcess()) call this%endTest(test)

   end subroutine run

   subroutine addListener(this, listener)
      use TestListener_mod, only: TestListener
      class (TestResult), intent(inOut) :: this
      class (TestListener), target, intent(in) :: listener

      integer :: n

      call extend(this%listeners)
      n = size(this%listeners)
      this%listeners(n)%pListener => listener

   contains

      subroutine extend(listeners)
         type (ListenerPointer), allocatable, intent(inout) :: listeners(:)
         type (ListenerPointer) :: temp(size(listeners))
         integer :: n

         temp = listeners

         deallocate(listeners)
         n = size(listeners)
         allocate(listeners(n+1))

         listeners(:n) = temp

      end subroutine extend

   end subroutine addListener

   function getIthFailure(this, i) result(failure)
      class (TestResult), intent(in) :: this
      integer, intent(in) :: i
      type (TestFailure) :: failure

      failure = this%failures(i)

   end function getIthFailure

   logical function wasSuccessful(this)
      class (TestResult), intent(in) :: this
      wasSuccessful = (this%failureCount() ==  0) .and. (this%errorCount() == 0)
   end function wasSuccessful

end module TestResult_mod
