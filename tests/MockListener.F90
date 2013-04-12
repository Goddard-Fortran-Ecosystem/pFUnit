module MockListener_mod
   use TestListener_mod
   implicit none
   private

   public :: MockListener
   type, extends(TestListener) :: MockListener
     character(len=40) :: log
   contains
     procedure :: addFailure
     procedure :: startTest
     procedure :: endTest
   end type MockListener

contains

  subroutine addFailure(this, testName, exceptions)
     use Exception_mod
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (Exception), intent(in) :: exceptions(:)

     write(this%log,'(a)') 'addFailure() was called'

  end subroutine addFailure

  subroutine startTest(this, testName)
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(this%log,'(a)') 'startTest() was called'

   end subroutine startTest

  subroutine endTest(this, testName)
     class (MockListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(this%log,'(a)') 'endTest() was called'

   end subroutine endTest

end module MockListener_mod
