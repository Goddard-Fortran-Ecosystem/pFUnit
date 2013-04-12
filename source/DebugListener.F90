module DebugListener_mod
   use TestListener_mod
   implicit none
   private

   public :: DebugListener

   type, extends(TestListener) :: DebugListener
   contains
      procedure :: addFailure
      procedure :: startTest
      procedure :: endTest
   end type DebugListener

contains

  subroutine addFailure(this, testName, exceptions)
     use Exception_mod
     class (DebugListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (Exception), intent(in) :: exceptions(:)

     write(*,*)'Failure in ',trim(testName)

  end subroutine addFailure

  subroutine startTest(this, testName)
     class (DebugListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(*,*)'Starting test: ',trim(testName)
   end subroutine startTest

  subroutine endTest(this, testName)
     class (DebugListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(*,*)'   ... ',trim(testName),' completed.'

   end subroutine endTest

end module DebugListener_mod
