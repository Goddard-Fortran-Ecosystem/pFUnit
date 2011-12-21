!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: TestMethodInfo
!
!> @brief
!! Implements the object for test method information.
!!
!! @author
!! Tom Clune, NASA/GSFC SIVO
!!
!! @date
!! 15 Dec 2007
!!
!! @todo question: does this need more implementation since it has 
!! only function?
!!
! REVISION HISTORY:
! 15 Dec 2007 - Initial Version
! 15 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module TestMethodInfo_mod
   implicit none
   private

   public :: TestMethodInfo_type
   public :: getComm

   type TestMethodInfo_type
      integer :: comm = -1
   end type TestMethodInfo_type

contains

   !---------------------------------------------------------------------------
   !> Get the type of communication. 
   !!
   !! @param this - this test method information object
   !!
   !! @return type of communication
   !---------------------------------------------------------------------------
   integer function getComm(this)
      type (TestMethodInfo_type) :: this
      
      getComm = this % comm
    end function getComm

end module TestMethodInfo_mod
