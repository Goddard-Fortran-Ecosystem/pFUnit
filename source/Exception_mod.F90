!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: Exception_mod 
!
!> @brief
!! Provides the ability for a test to raise an exception and subsequently test 
!! for whether an exception has been raised.  In the absence of language
!! support, this unfortunately requires the use of globally accessible 
!! quantities.
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
module Exception_mod
   use Params_mod, only: MAX_LEN_MSG
   use pFUnitException_mod
   implicit none
   private

   public :: RaiseException
   public :: PopException
   public :: ExceptionRaised
   public :: ClearExceptions

   interface ExceptionRaised
      module procedure ExceptionRaised_any
      module procedure ExceptionRaised_msg
   end Interface

contains

   !---------------------------------------------------------------------------
   !> Raises the expection when the error occcurs.
   !!
   !! @param message - given identifying message 
   !! @param serverity - given level of severity
   !!
   !! @throw Exception when any error occurs.
   !---------------------------------------------------------------------------
   subroutine RaiseException(msg, severity)
      character(len=*),  intent(In) :: msg
      integer, optional, intent(in) :: severity
      integer :: severity_

      call throw(Exception(msg))

   end subroutine RaiseException

   !---------------------------------------------------------------------------
   !> Pops up the stack of exceptions. 
   !!
   !! @param n - given optional index of the exception
   !!
   !! @throw Exception when any error occurs.
   !---------------------------------------------------------------------------
   subroutine PopException(n)
      integer, optional, intent(in) :: n

      integer :: n_
      integer :: i

      n_ = 1
      if (present(n)) n_ = n

      do i = 1, n_
         if (.not. catch()) exit
      end do

   end subroutine PopException

   !---------------------------------------------------------------------------
   !> Clears up all exceptions. 
   !---------------------------------------------------------------------------
   subroutine ClearExceptions()
      call clearAll()
   end subroutine ClearExceptions

   !---------------------------------------------------------------------------
   !> Determines if it catches any exception. 
   !---------------------------------------------------------------------------
   logical function ExceptionRaised_any()
      ExceptionRaised_any = catch()
   end function ExceptionRaised_any

   !---------------------------------------------------------------------------
   !> Determines if it catches any exception with the specific message. 
   !---------------------------------------------------------------------------
   logical function ExceptionRaised_msg(msg)
      character(len=*), intent(in) :: msg
      ExceptionRaised_msg = catch(msg)
   end function ExceptionRaised_msg
  
end module Exception_mod
