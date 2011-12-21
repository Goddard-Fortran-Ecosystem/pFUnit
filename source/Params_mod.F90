!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: Params_mod
!
!> @brief
!!Various constants that are shared among multiple modules.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
! REVISION HISTORY:
! 16 Oct 2006 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module Params_mod
!!$  use iso_c_binding, only : C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX
   implicit none

   integer, parameter :: R32 = selected_real_kind(p=6)
   integer, parameter :: R64 = selected_real_kind(p=14)
   integer, parameter :: C32 = R32
   integer, parameter :: C64 = R64
 
   integer, parameter :: I32 = selected_int_kind(8)
   integer, parameter :: I64 = selected_int_kind(14)

   integer, parameter :: MAX_LEN_NAME = 40
   integer, parameter :: MAX_LEN_MSG  = 80 * 10
 
#ifdef LONG_PTR
   integer, parameter :: KIND_PTR=i64
#else
   integer, parameter :: KIND_PTR=i32
#endif

   integer, parameter :: FUNIT_SUCCESS = 0
   integer, parameter :: FUNIT_FAILURE = 1

   character(len=*), parameter :: REQ_LAUNCH_SEMAPHORE = 'SEMAPHORE_REQ_LAUNCH'

end module Params_mod
