!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: ProcedurePointer
!
!> @brief
!! This module (and external procedure) provides the means to reference
!! procedure pointers from within a Fortran program.  Fortran provides
!! no such means, so a semi-portable interface with C is used instead.
!! F2003 will simplify this implementation in the near future.
!
!! @author
!! Tom Clune, NASA/GSFC SIVO
!!
!! @date 
!! 30 Aug 2006
!
! !REVISION HISTORY:
! None
!-------------------------------------------------------------------------------
module ProcedurePointer_mod
   use BaseAddress_mod
   use pFUnitException_mod
   use ISO_C_BINDING
   implicit none
   private

   public :: ProcedurePointer_type
   public :: ProcedurePointer
   public :: invoke
   public :: address

   type ProcedurePointer_type
      private
      type (C_FUNPTR) :: handle = C_NULL_FUNPTR
   end type ProcedurePointer_type

   interface ProcedurePointer
      module procedure constructFromDummy
      module procedure constructFromAddress
   end interface

   interface invoke
      module Procedure invoke0
      module Procedure invoke1
      module Procedure invoke2
   end interface

   type foo
     procedure(), pointer, nopass :: p
   end type foo


contains

   !---------------------------------------------------------------------------
   !> Standard constructor.  Accepts a procedure argument along with an
   !! integer for the number of arguments that procedure expects.
   !!
   !! @param procedure - given 'procedure' routine for binding 
   !!
   !! @return a ProcedurePointer object.
   !---------------------------------------------------------------------------
   function constructFromDummy(procedure) result(p)
      implicit none
      interface
         subroutine procedure() bind(c)
         end subroutine procedure
      end interface
      type (ProcedurePointer_type) :: p

      ! kludge through call to external C routine
      p % handle = C_FUNLOC(procedure)

   end function constructFromDummy
   
   !---------------------------------------------------------------------------
   !> Alternates constructor that accepts a BaseAddress which holds a procedure 
   !! address obtained by some other mechanism  (probably from a shared object 
   !! library).
   !! 
   !! @param address - address that is pointed by function
   !! @param name - name of procedure pointer
   !! 
   !! @return object of procedure pointer
   !!
   !! @note
   !! The "name" argument is unused and should be eliminated.  Eventually I expect
   !! that ProcedurePointers will be able to return their defined "name".
   !---------------------------------------------------------------------------
   function constructFromAddress(address, name) result(p)
      implicit none
      type (C_FUNPTR) :: address
      character(len=*), intent(in) :: name
      type (ProcedurePointer_type) :: p

      p % handle = address

   end function constructFromAddress
   
   !---------------------------------------------------------------------------
   !> Executes the referenced procedure with 0 arguments.
   !!
   !! @param procedure - given object of procedure pointer
   !!
   !! @throw Exception if the procedure pointer is not valid, or if the
   !! incorrect number of arguments are provided.
   !---------------------------------------------------------------------------
   subroutine invoke0(procedure) 
      ! throws NULL_PROCEDURE_POINTER
      ! throws INCORRECT_NUMBER_OF_ARGUMENTS
      implicit none
      type (ProcedurePointer_type) :: procedure

      type (foo) :: fptr

      if (isNullProcedure(procedure)) then
         call throw(NULL_PROCEDURE_POINTER)
         return
      end if

      call C_F_PROCPOINTER(procedure % handle, fptr%p)
      call fptr%p()

   end subroutine invoke0

   !---------------------------------------------------------------------------
   !> Execute the referenced procedure with 1 argument.
   !!
   !! @param procedure - given object of procedure pointer
   !! @param argument1 - given base address object for its argument
   !!
   !! @throw Eexception if the procedure pointer is not valid, or if the
   !! incorrect number of arguments are provided.
   !---------------------------------------------------------------------------
   subroutine invoke1(procedure, argument1)
      use BaseAddress_mod
      implicit none
      type (ProcedurePointer_type) :: procedure
      type (BaseAddress_type), intent(in) :: argument1

      type (foo) :: fptr

      integer, pointer :: aptr
      type (C_PTR) :: pArg1

      if (isNullProcedure(procedure)) then
         call throw(NULL_PROCEDURE_POINTER)
         return
      end if

      call C_F_PROCPOINTER(procedure % handle, fptr%p)
      call get(argument1, pArg1)
      call C_F_POINTER(pArg1, aptr)
      call fptr%p(aptr)

   end subroutine invoke1

   !---------------------------------------------------------------------------
   !> Execute the referenced procedure with 2 arguments.
   !!
   !! @param procedure - given object of procedure pointer
   !! @param argument1 - given base address object for its argument
   !! @param argument2 - given base address object for its argument
   !!
   !! @throw Exception if the procedure pointer is not valid, or if the
   !! incorrect number of arguments are provided.
   !---------------------------------------------------------------------------
   subroutine invoke2(procedure, argument1, argument2) 
      use BaseAddress_mod
      implicit none
      type (ProcedurePointer_type) :: procedure
      type (BaseAddress_type), intent(in) :: argument1
      type (BaseAddress_type), intent(in) :: argument2

      type (foo) :: fptr
      integer, pointer :: ptr1, ptr2
      type (C_PTR) :: pArg1
      type (C_PTR) :: pArg2

      if (isNullProcedure(procedure)) then
         call throw(NULL_PROCEDURE_POINTER)
         return
      end if

      call C_F_PROCPOINTER(procedure % handle, fptr%p)
      call get(argument1, pArg1)
      call get(argument2, pArg2)
      call C_F_POINTER(pArg1, ptr1)
      call C_F_POINTER(pArg2, ptr2)
      call fptr%p(ptr1,ptr2)

   end subroutine invoke2

   !---------------------------------------------------------------------------
   !> Determines whether the procedure pointer is null or not. 
   !!
   !! @param this - this procedure pointer object
   !!
   !! @return .true. if the procedure pointer is "null".
   !---------------------------------------------------------------------------
   logical function isNullProcedure(this)
      type (ProcedurePointer_type), intent(in) :: this

      isNullProcedure = .not. C_ASSOCIATED(this % handle)

   end function isNullProcedure

   !---------------------------------------------------------------------------
   !> Accessor method which returns the integer representation of the
   !! procedure address.
   !!
   !! @param this - this procedure pointer object
   !!
   !! @return the procedure address
   !---------------------------------------------------------------------------
   function address(this)
      type (ProcedurePointer_type), intent(in) :: this
      type (C_FUNPTR) :: address

      address = this % handle

   end function address

end module ProcedurePointer_mod
