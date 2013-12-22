!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: BaseTestRunner
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
module BaseTestRunner_mod
   use TestListener_mod
   implicit none
   private

   public :: BaseTestRunner

   type, abstract, extends(TestListener) :: BaseTestRunner
   contains
      procedure(run), deferred :: run
   end type BaseTestRunner

   abstract interface
      subroutine run(this, aTest, context)
         use Test_mod
         use ParallelContext_mod
         import BaseTestRunner
         
         class (BaseTestRunner), intent(inout) :: this
         class (Test), intent(inout) :: aTest
         class (ParallelContext), intent(in) :: context
      end subroutine run
      
!!$      subroutine startTest(this, testName)
!!$         import BaseTestRunner
!!$         class (BaseTestRunner), intent(inout) :: this
!!$         character(len=*), intent(in) :: testName
!!$      end subroutine startTest
!!$
!!$      subroutine endTest(this, testName)
!!$         import BaseTestRunner
!!$         class (BaseTestRunner), intent(inout) :: this
!!$         character(len=*), intent(in) :: testName
!!$      end subroutine endTest
!!$
!!$      subroutine addFailure(this, testName, exceptions)
!!$         import BaseTestRunner
!!$         use Exception_mod
!!$         class (BaseTestRunner), intent(inout) :: this
!!$         character(len=*), intent(in) :: testName
!!$         type (Exception), intent(in) :: exceptions(:)
!!$      end subroutine addFailure
   end interface

end module BaseTestRunner_mod
