!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: BaseAddress
! 
!> @brief
!!This module (and external procedure) provides the means to determine
!! the base address of an entity.  Fortran provides no such means, so a
!! semi-portable interface with C is used instead.  F2003 will simplify
!! this implementation in the near future.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO 
!!
!! @date
!! 30 Aug 2006
!!
!!
! REVISION HISTORY:
! None
!-------------------------------------------------------------------------------
module BaseAddress_mod
   use ISO_C_BINDING
   implicit none
   private

   public :: BaseAddress_type !< derived type
   public :: operator(==)
   public :: isNull
   public :: nullify
   public :: get
   public :: setAddress

   public :: KIND_POINTER ! kind of integer to hold addresses
#ifdef LONG_PTR
   integer, parameter :: KIND_POINTER = selected_int_kind(16)
#else
   integer, parameter :: KIND_POINTER = selected_int_kind(8)
#endif

   type BaseAddress_type
      type (C_PTR) :: address = C_NULL_PTR
   end type BaseAddress_type

   interface operator(==)
      module procedure same
   end interface

   interface get
      module procedure get_
   end interface

contains

   !---------------------------------------------------------------------------
   !> Set the stored address of a BaseAddress object using a known
   !! address computed in some other fashion.  (Probably from a shared
   !! object library.)
   !!
   !! @param this - this object of BaseAddress 
   !! @param address - given stored adderss
   !!
   !!---------------------------------------------------------------------------
   subroutine setAddress(this, address)
      type (BaseAddress_type) :: this
      type (C_PTR), intent(in) :: address
      this % address = address
   end subroutine setAddress

   !---------------------------------------------------------------------------
   !> Determines if two BaseAddresses are the same
   !!
   !! @param a -  object of BaseAddress type
   !! @param b -  object of BaseAddress type
   !!
   !! @return .true. only if the two BaseAddresses are the same.
   !---------------------------------------------------------------------------
   logical function same(a, b)
      implicit none
      type (BaseAddress_type), intent(in) :: a
      type (BaseAddress_type), intent(in) :: b
      same = C_ASSOCIATED(a % address, b % address)
   end function same

   !---------------------------------------------------------------------------
   !> Determines if the base address is null.
   !!
   !! @param this - this object of BaseAddress 
   !!
   !! @return .true. if the stored address is null.
   !---------------------------------------------------------------------------
   logical function isNull(this)
      type (BaseAddress_type), intent(in) :: this
      isNull = .not. C_ASSOCIATED(this % address)
   end function isNull

   !---------------------------------------------------------------------------
   !> Set the stored address to null.
   !!
   !! @param this - this object of BaseAddress 
   !!
   !! @todo
   !! *** This should be renamed - it conflicts with F90 intrinsic.***
   !!
   !---------------------------------------------------------------------------
   subroutine nullify(this)
      type (BaseAddress_type), intent(out) :: this
      this % address = C_NULL_PTR
   end subroutine nullify

   !---------------------------------------------------------------------------
   !> Return the stored address in a suitably sized integer.
   !!
   !! @param this - this object of BaseAddress 
   !! 
   !! @return
   !! return the object of stored base address
   !---------------------------------------------------------------------------
   subroutine get_(this, address)
      type (BaseAddress_type), intent(in) :: this
      type (C_PTR) :: address
      address = this % address
   end subroutine get_
end module BaseAddress_mod

   !---------------------------------------------------------------------------
   !> Constructor - returns a BaseAddress object containing the address of the
   !! passed argument.
   !!
   !! Unfortunately, this requires the use of a fortran "implicit"
   !! interface, which in turn prevents this routine being within a
   !! module (which would automatically confer an "explicit"
   !! interface).  The user must manually declare the return type
   !! in each scoping unit that will use the procedure.
   !!
   !! @param anyType  - given input of type
   !!
   !! @return return the object of BaseAddress
   !---------------------------------------------------------------------------
function BaseAddress(anyType) result(address)
   use BaseAddress_mod
   use ISO_C_BINDING
   implicit none
   integer, intent(in), target :: anyType
   type (BaseAddress_type) :: address

   address%address = C_LOC(anyType)
end function BaseAddress
