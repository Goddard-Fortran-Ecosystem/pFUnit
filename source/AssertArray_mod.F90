!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: AssertArray
!
!> @brief
!! Asserts that two arrays are conformable.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 04 Feb 2008
!!
! REVISION HISTORY:
! 04 Feb 2008 - Initial Version
! 09 Jul 2010 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module AssertArray_mod
   implicit none
   private

   public :: areConformable

   
contains

   !---------------------------------------------------------------------------
   !> Return if the shapes are conformable
   !!
   !! @param shapeLHS - integer array with shape on the left side
   !! @param shapeRHS - integer array with shape on the right side
   !!
   !! @return .true. if two shapes are conformable 
   !---------------------------------------------------------------------------
   logical function areConformable(shapeLHS, shapeRHS)
      integer, intent(in) :: shapeLHS(:)
      integer, intent(in) :: shapeRHS(:)
      logical :: sameRank

      if (size(shapeRHS) == 0) then
         areConformable = .true.
      else
         sameRank = (size(shapeLHS) == size(shapeRHS))
         if (sameRank) then
            areConformable = all(shapeLHS == shapeRHS)
         else
            areConformable = .false.
         end if
      end if

   end function areConformable

end module AssertArray_mod
