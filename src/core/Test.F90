#include "unused_dummy.fh"
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
   use pf_TestAnnotation_mod
   use pf_StringTestAnnotationMap_mod
   implicit none
   private

   public :: Test
   
   ! Abstract class from which other Test classes inherit
   type, abstract, extends(StringTestAnnotationMap) :: Test
      integer :: placeholder
   contains
      procedure(countTestCases), deferred :: countTestCases
      procedure(run), deferred :: run
      procedure(getName), deferred :: getName
      procedure :: setName

      procedure :: is_disabled
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
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(name)
   end subroutine setName


   logical function is_disabled(this)
      class (Test), intent(in) :: this

      is_disabled = (this%count(Disable%type_name()) == 1)

   end function is_disabled

end module PF_Test_mod
