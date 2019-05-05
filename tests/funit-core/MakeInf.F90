!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: MakeInf
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC SIVO
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

module MakeInf
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
   public :: makeInf_32
   public :: strInf
#endif
#ifdef _REAL64_IEEE_SUPPORT
   public :: makeInf_64
#endif
#ifdef _REAL128_IEEE_SUPPORT
   public :: makeInf_128
#endif

contains

#ifdef _REAL32_IEEE_SUPPORT
   function makeInf_32() result(inf_32)
      real(REAL32) :: inf_32

      inf_32 = ieee_value(inf_32,  ieee_positive_inf)
      
   end function makeInf_32

   ! Formatted string for Infinity varies by compiler.  This function
   ! is used by self tests to generate the appropriate string for
   ! assertions.
   function strInf() result(string)
      character(:), allocatable :: string
      character(40) :: buffer
      write(buffer,'(g0)') makeInf_32()
      string = trim(buffer)
   end function strInf
#endif

#ifdef _REAL64_IEEE_SUPPORT
   function makeInf_64() result(inf_64)
      real(REAL64) :: inf_64

      inf_64 = ieee_value(inf_64,  ieee_positive_inf)
      
   end function makeInf_64
#endif

#ifdef _REAL128_IEEE_SUPPORT
   function makeInf_128() result(inf_128)
      real(REAL128) :: inf_128

      inf_128 = ieee_value(inf_128,  ieee_positive_inf)
      
   end function makeInf_128
#endif


end module MakeInf
