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
   implicit none
   private

   public :: BaseTestRunner

   type, abstract, extends(TestListener) :: BaseTestRunner
      private

   contains
      procedure(run), deferred :: run
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

end module PF_BaseTestRunner
