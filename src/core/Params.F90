!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Params
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

module PF_Params
  use, intrinsic :: iso_fortran_env
  implicit none

  integer, parameter, public :: MAX_LENGTH_NAME = 128

  integer, parameter :: R32 = REAL32
  integer, parameter :: R64 = REAL64
  integer, parameter :: C32 = REAL32
  integer, parameter :: C64 = REAL64

  integer, parameter :: I32 = INT32
  integer, parameter :: I64 = INT64

  integer, parameter :: NEQP=0, EQP=1, GTP=2, GEP=3, LTP=4, LEP=5, &
  &  RELEQP=6

end module PF_Params

