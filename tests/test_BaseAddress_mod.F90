!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!BOP
! !MODULE: test_BaseAddress
!
! !AUTHORS:      Tom Clune
! !AFFILIATION:  NASA SIVO
! !DATE:         30 Aug 2006
!
! !DESCRIPTION: 
!
! Tests the BaseAddress module.
!
! !REVISION HISTORY:
! None
!-------------------------------------------------------------------------------

! !INTERFACE:  
module test_BaseAddress_mod
   use pfunit
   use BaseAddress_mod
   implicit none
   private

   public :: test_sameAddress
   public :: test_differentAddress
   public :: test_isNull

   include 'BaseAddress.inc'   

contains

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_sameAddress
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   !
   ! Tests that the two addresses of the same object are equal.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_sameAddress()
     use ISO_C_BINDING
      implicit none

      integer :: intA
      real    :: realB

      type (BaseAddress_type) :: addressA
      type (BaseAddress_type) :: addressB

      addressA = BaseAddress(intA)
      addressB = BaseAddress(intA)
      call assertTrue(addressA == addressB,'intA ?= intA')

      addressA = BaseAddress(realB)
      addressB = BaseAddress(realB)
      call assertTrue(addressA == addressB,'realB ?= realB')
      
   end subroutine test_sameAddress

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_differentAddress
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   !
   ! Tests that the two addresses of the different objects are not equal.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_differentAddress()
      implicit none

      integer :: intA, intB
      real    :: realA, realB

      type (BaseAddress_type) :: addressA
      type (BaseAddress_type) :: addressB

      addressA = BaseAddress(intA)
      addressB = BaseAddress(intB)
      call assertFalse(addressA == addressB,'intA /= intB')

      addressA = BaseAddress(realA)
      addressB = BaseAddress(realB)
      call assertFalse(addressA == addressB,'realA /= realB')
      
   end subroutine test_differentAddress

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_isNull
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   !
   ! Tests that address of a variable is not null, and that nullify creates
   ! a null address.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_isNull()
      use iso_c_binding
      type (BaseAddress_type), SAVE :: address
      integer :: dummyTarget

      call assertTrue(isNull(address),'a')
      address = BaseAddress(dummyTarget)
      call assertFalse(isNull(address),'b')
      call nullify(address)
      call assertTrue(isNull(address),'c')
      
   end subroutine test_isNull

end module test_BaseAddress_mod
