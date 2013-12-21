!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: TestListener
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
     procedure :: addError
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

contains

   ! Most scenarios in Fortran cannot diagnose true errors, so
   ! an empty stub is provided here for convenience.
   subroutine addError(this, testName, exceptions)
      use Exception_mod, only: Exception
      class (TestListener), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (Exception), intent(in) :: exceptions(:)
   end subroutine addError

 end module TestListener_mod
