!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: Test
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
module Test_mod
   implicit none
   private
   
   public :: Test

   ! Abstract class from which other Test classes inherit
   type, abstract :: Test
      integer :: placeholder
   contains
      procedure(countTestCases), deferred :: countTestCases
      procedure(run), deferred :: run
   end type Test

   abstract interface

      integer function countTestCases(this)
         import Test
         class (Test), intent(in) :: this
      end function countTestCases

      recursive subroutine run(this, tstResult, context)
         use TestResult_mod
         use ParallelContext_mod
         import Test
         class (Test), intent(inout) :: this
         class (TestResult), intent(inout) :: tstResult
         class (ParallelContext), intent(in) :: context
      end subroutine run

   end interface

end module Test_mod
