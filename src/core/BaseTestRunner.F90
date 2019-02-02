!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: BaseTestRunner
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
module PF_BaseTestRunner
  use PF_TestListener
  use pf_TestListenerVector
   implicit none
   private

   public :: BaseTestRunner

   type, abstract, extends(TestListener) :: BaseTestRunner
      private
      type(TestListenerVector) :: listeners
   contains
     procedure(run), deferred :: run
     procedure :: add_listener
     procedure :: get_listeners
   end type BaseTestRunner

   abstract interface

      function run(this, aTest, context) result(result)
         use PF_Test
         use PF_ParallelContext
         use PF_TestResult
         import BaseTestRunner

         type (TestResult) :: result
         class (BaseTestRunner), target, intent(inout) :: this
         class (Test), intent(inout) :: aTest
         class (ParallelContext), intent(in) :: context
      end function run

   end interface

 contains

   subroutine add_listener(this, listener)
     class(BaseTestRunner), intent(inout) :: this
     class(TestListener), intent(in) :: listener
     call this%listeners%push_back(listener)
   end subroutine add_listener

   function get_listeners(this) result(listeners)
     type(TestListenerVector), pointer :: listeners
     class(BaseTestRunner), target, intent(in) :: this

     listeners => this%listeners
   end function get_listeners

end module PF_BaseTestRunner
