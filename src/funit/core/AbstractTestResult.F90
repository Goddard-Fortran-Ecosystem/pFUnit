module PF_AbstractTestResult

  implicit none
  private

  public :: AbstractTestResult

  type, abstract :: AbstractTestResult
!     private
  contains
     procedure(getRuntime), deferred   :: getRuntime
     procedure(getFailures), deferred  :: getFailures
     procedure(getErrors), deferred    :: getErrors
     procedure(getSuccesses), deferred :: getSuccesses
     procedure(wasSuccessful), deferred :: wasSuccessful
     procedure(runCount), deferred      :: runCount
     procedure(failureCount), deferred  :: failureCount
     procedure(errorCount), deferred    :: errorCount
     procedure(errorCount), deferred    :: disableCount
     procedure(getName), deferred       :: getName
     procedure(setName), deferred       :: setName

  end type AbstractTestResult

  abstract interface

     function getRunTime(this) result(time)
!        TestFailure, only : TestFailure
       import AbstractTestResult
       class (AbstractTestResult), intent(in) :: this
       real :: time
     end function getRunTime

     function getFailures(this) result(failures)
       use PF_TestFailureVector, only : TestFailureVector
       import AbstractTestResult
       class (AbstractTestResult), target, intent(in) :: this
       type (TestFailureVector), pointer :: failures
     end function getFailures

     function getErrors(this) result(errors)
       use PF_TestFailureVector, only : TestFailureVector
       import AbstractTestResult
       class (AbstractTestResult), target, intent(in) :: this
       type (TestFailureVector), pointer :: errors
     end function getErrors

     function getSuccesses(this) result(successes)
       use PF_TestFailureVector, only : TestFailureVector
       import AbstractTestResult
       class (AbstractTestResult), target, intent(in) :: this
       type (TestFailureVector), pointer :: successes
     end function getSuccesses

   logical function wasSuccessful(this)
       import AbstractTestResult
      class (AbstractTestResult), intent(in) :: this
   end function wasSuccessful

   integer function runCount(this)
       import AbstractTestResult
      class (AbstractTestResult), intent(in) :: this
   end function runCount

   integer function errorCount(this)
       import AbstractTestResult
      class (AbstractTestResult), intent(in) :: this
   end function errorCount

   integer function failureCount(this)
       import AbstractTestResult
      class (AbstractTestResult), intent(in) :: this
    end function failureCount

   function getName(this) result(name)
      import AbstractTestResult
      class (AbstractTestResult), intent(in) :: this
      character(:), allocatable :: name
   end function getName

   subroutine setName(this, name)
      import AbstractTestResult
      class (AbstractTestResult), intent(inout) :: this
      character(len=*),intent(in) :: name
   end subroutine setName


  end interface

end module PF_AbstractTestResult
