!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test
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
module PF_Test_mod
   implicit none
   private

   ! Abstract class from which other Test classes inherit
   type, public, abstract :: Test
      integer :: placeholder
   contains
      procedure(countTestCases), deferred :: countTestCases
      procedure(run), deferred :: run
      procedure(getName), deferred :: getName
      procedure :: setName
   end type Test

   abstract interface

      integer function countTestCases(this)
         import Test
         class (Test), target, intent(in) :: this
      end function countTestCases

      recursive subroutine run(this, tstResult, context)
         use PF_TestResult_mod
         use PF_ParallelContext_mod
         import Test
         class (Test), target, intent(inout) :: this
         class (TestResult), intent(inout) :: tstResult
         class (ParallelContext), intent(in) :: context
      end subroutine run

      function getName(this) result(name)
         import Test
         class (Test), intent(in) :: this
         character(:), allocatable :: name
      end function getName

   end interface
contains
   subroutine setName(this, name)
      class (Test), intent(inout) :: this
      character(len=*), intent(in) :: name
      ! Default: Cannot change name
   end subroutine setName

end module PF_Test_mod
