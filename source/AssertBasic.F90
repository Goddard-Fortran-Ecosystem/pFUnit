module AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   implicit none
   private
   
   public :: assertTrue
   public :: assertExceptionRaised

   interface assertTrue
      module procedure assertTrue_
      module procedure assertTrue_withMessage
   end interface

   interface assertExceptionRaised
      module procedure assertExceptionRaisedBasic
      module procedure assertExceptionRaisedMessage
   end interface assertExceptionRaised

   character(len=*), parameter :: NULL_MESSAGE = '<>'

contains

   subroutine assertTrue_(condition, location)
      logical, intent(in) :: condition
      type (SourceLocation), optional, intent(in) :: location
      call assertTrue(condition, NULL_MESSAGE, location)
   end subroutine assertTrue_

   subroutine assertTrue_withMessage(condition, message, location)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      if (.not. condition) call throw(trim(message), location)
   end subroutine assertTrue_withMessage

   subroutine assertTrueMessage(condition, message)
      use Exception_mod, only: throw
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message
      if (.not. condition) call throw('Logical assertion failed :: '//trim(message))
   end subroutine assertTrueMessage

   subroutine assertExceptionRaisedBasic()
      use Exception_mod, only: throw, catch

      if (.not. catch()) then
         call throw('Failed to throw exception.')
      end if

   end subroutine assertExceptionRaisedBasic

   subroutine assertExceptionRaisedMessage(message)
      use Exception_mod, only: throw, catch
      character(len=*), intent(in) :: message

      if (.not. catch(message)) then
         call throw('Failed to throw exception: <' // trim(message) // '>')
      end if

   end subroutine assertExceptionRaisedMessage

end module AssertBasic_mod
