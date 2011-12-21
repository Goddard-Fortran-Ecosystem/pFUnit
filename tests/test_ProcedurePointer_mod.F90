!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!BOP
! !MODULE: test_ProcedurePointer
!
! !AUTHORS:      Tom Clune
! !AFFILIATION:  NASA SIVO
! !DATE:         30 Aug 2006
!
! !DESCRIPTION: 
!
! Tests the ProcedurePointer module.
!
! !REVISION HISTORY:
! None
!-------------------------------------------------------------------------------

! !INTERFACE:  
module test_ProcedurePointer_mod
   use pfunit, only: assertTrue
   use pfunit, only: assertEqual
   use pfunit, only: catch, throw, Exception
   use pfunit, only: BaseAddress_type
   use ProcedurePointer_mod
   implicit none
   private

   public :: test_callUninitialized
   public :: test_callNoArguments
   public :: test_callOneIntegerArgument
   public :: test_callOneRealArgument

   public :: test_callUnsafe

contains

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_callUninitialized
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   !
   ! Test that the NULL_PROCEDURE_POINTER exception is thrown when using null
   ! pointer.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_callUninitialized()
      use pFUnitException_mod
      type (ProcedurePointer_type), SAVE :: procPointer

      call invoke(procPointer)
      call assertTrue(catch(NULL_PROCEDURE_POINTER))

   end subroutine test_callUninitialized

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_callNoArguments
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   !
   ! Test that invoke() call the correct procedure.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_callNoArguments()

      external noArguments
      type (ProcedurePointer_type) :: procPointer

      procPointer = ProcedurePointer(noArguments)
      call invoke(procPointer)
      if (.not. catch('noArguments() called')) then
         call throw(Exception('noArgument() apparently has not been called'))
      end if

   end subroutine test_callNoArguments

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_callOneIntegerArgument
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   !
   ! Test that invoke() passes correct argument to procedure.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_callOneIntegerArgument()
      use BaseAddress_mod
      implicit none
      external oneIntegerArgument
      external BaseAddress
      type (BaseAddress_type) :: BaseAddress

      integer :: iDum1

      type (ProcedurePointer_type) :: procPointer

      procPointer = ProcedurePointer(oneIntegerArgument)

      idum1 = 2
      call invoke(procPointer, BaseAddress(iDum1))
      if (.not. catch('oneIntegerArgument() was called')) then
         call throw(Exception('Failed to call oneIntegerArgument()'))
      else
         call assertEqual(3, idum1)
      end if

   end subroutine test_callOneIntegerArgument

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_callOneRealArgument
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   !
   ! Test that invoke() passes correct argument to procedure.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_callOneRealArgument()
      use BaseAddress_mod
      implicit none
      external oneRealArgument
      external BaseAddress
      type (BaseAddress_type) :: BaseAddress

      real :: xDum1

      type (ProcedurePointer_type) :: procPointer
      procPointer = ProcedurePointer(oneRealArgument)

      xdum1 = 1
      call invoke(procPointer, BaseAddress(xDum1))

      if (.not. catch('oneRealArgument() was called')) then
         call throw(Exception('Failed to call oneRealArgument()'))
      else
         call assertEqual(exp(1.), xDum1)
      end if

   end subroutine test_callOneRealArgument

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_callUnsafe
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   !
   ! Test that invoke0Arguments(), invoke1Argument(), and
   ! invoke2Arguments() call the correct procedures respectively.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_callUnsafe()
      implicit none
      external noArguments
      external oneIntegerArgument
      external oneRealArgument
      external BaseAddress
      type (BaseAddress_type) :: BaseAddress

      integer :: iDum
      real :: xDum

      type (ProcedurePointer_type) :: procPointer

      procPointer = ProcedurePointer(noArguments)

      call invoke(procPointer)

      if (.not. catch('noArguments() called')) then
         call throw(Exception('noArgument() apparently has not been called'))
      end if

      procPointer = ProcedurePointer(oneIntegerArgument)
      
      iDum=2
      call invoke(procPointer, BaseAddress(iDum))

      if (.not. catch('oneIntegerArgument() was called')) then
         call throw(Exception('Failed to call oneIntegerArgument()'))
      else
         call assertEqual(3, idum)
      end if

      procPointer = ProcedurePointer(oneRealArgument)
      
      xDum = 1
      call invoke(procPointer, BaseAddress(xDum))
      if (.not. catch('oneRealArgument() was called')) then
         call throw(Exception('Failed to call oneRealArgument()'))
      else
         call assertEqual(exp(1.), xDum)
      end if

   end subroutine test_callUnsafe

end module test_ProcedurePointer_mod


! The following are procedures used as test targets for 
! testing procedure pointers.  I.e. they are not proper tests themselves.

subroutine noArguments()
   use pfunit
   implicit none

   call throw(Exception('noArguments() called'))

end subroutine noArguments

subroutine oneIntegerArgument(argument1)
   use pfunit
   implicit none
   integer, intent(inOut) :: argument1

   call throw(Exception('oneIntegerArgument() was called'))

   argument1 = argument1 + 1

end subroutine oneIntegerArgument

subroutine oneRealArgument(argument1)
   use pfunit
   implicit none
   real, intent(inOut) :: argument1

   call throw(Exception('oneRealArgument() was called'))

   argument1 = exp(argument1)

end subroutine oneRealArgument
