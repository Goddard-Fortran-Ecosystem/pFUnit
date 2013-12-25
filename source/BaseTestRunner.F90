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
      private
      logical :: useDebug = .false.
   contains
      procedure(run2), deferred :: run
      procedure :: setDebug
      procedure :: debug
   end type BaseTestRunner

   abstract interface

      ! TODO - report bug to NAG.  If this is named "run" then
      ! RubustRunner fails to compile with message about conflicting types

      logical function run2(this, aTest, context) result(success)
         use Test_mod
         use ParallelContext_mod
         import BaseTestRunner

         class (BaseTestRunner), intent(inout) :: this
         class (Test), intent(inout) :: aTest
         class (ParallelContext), intent(in) :: context
      end function run2
      
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

contains

    subroutine setDebug(this)
       class (BaseTestRunner), intent(inout) :: this
       this%useDebug = .true.
    end subroutine setDebug


    logical function debug(this)
       class (BaseTestRunner), intent(inout) :: this
       debug = this%useDebug
    end function debug

end module BaseTestRunner_mod
