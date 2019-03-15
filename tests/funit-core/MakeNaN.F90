!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: MakeNaN
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

module MakeNaN
#ifdef _REAL32_IEEE_SUPPORT
   use, intrinsic :: iso_fortran_env, only: REAL32
#endif
#ifdef _REAL64_IEEE_SUPPORT
   use, intrinsic :: iso_fortran_env, only: REAL64
#endif
#ifdef _REAL128_IEEE_SUPPORT
   use, intrinsic :: iso_fortran_env, only: REAL128
#endif
   use, intrinsic :: ieee_arithmetic
   implicit none
   private

#ifdef _REAL32_IEEE_SUPPORT
   public :: makeNaN_32
#endif
#ifdef _REAL64_IEEE_SUPPORT
   public :: makeNaN_64
#endif
#ifdef _REAL128_IEEE_SUPPORT
   public :: makeNaN_128
#endif

contains
   
#ifdef _REAL32_IEEE_SUPPORT
   function makeNaN_32() result(nan_32)
      real(REAL32) :: nan_32

      nan_32 = ieee_value(nan_32,  ieee_quiet_nan)
      
   end function makeNaN_32
#endif

#ifdef _REAL64_IEEE_SUPPORT
   function makeNaN_64() result(nan_64)
      real(REAL64) :: nan_64

      nan_64 = ieee_value(nan_64,  ieee_quiet_nan)
      
   end function makeNaN_64
#endif

#ifdef _REAL128_IEEE_SUPPORT
   function makeNaN_128() result(nan_128)
      real(REAL128) :: nan_128

      nan_128 = ieee_value(nan_128,  ieee_quiet_nan)
      
   end function makeNaN_128
#endif



end module MakeNaN
