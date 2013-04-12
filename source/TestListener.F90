module TestListener_mod
   implicit none
   private

   public :: TestListener
   public :: ListenerPointer

   type, abstract :: TestListener
      integer :: placeholder
   contains
     procedure(addFailure), deferred :: addFailure
     procedure(startTest), deferred :: startTest
     procedure(endTest), deferred :: endTest
   end type TestListener

   type ListenerPointer
     class (TestListener), pointer :: pListener
   end type ListenerPointer

   abstract interface
      subroutine addFailure(this, testName, exceptions)
         use Exception_mod
         import TestListener
         class (TestListener), intent(inout) :: this
         character(len=*), intent(in) :: testName
         type (Exception), intent(in) :: exceptions(:)
      end subroutine addFailure

      subroutine startTest(this, testName)
         import TestListener
         class (TestListener), intent(inout) :: this
         character(len=*), intent(in) :: testName
      end subroutine startTest
    
      subroutine endTest(this, testName)
         import TestListener
         class (TestListener), intent(inout) :: this
         character(len=*), intent(in) :: testName
      end subroutine endTest
   end interface

 end module TestListener_mod
