module AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   implicit none
   private
   
   public :: fail
   public :: assertTrue
   public :: assertExceptionRaised
   public :: conformable
   public :: nonConformable
   public :: toString

   interface fail
      module procedure fail_
      module procedure fail_withMessage
   end interface fail

   interface assertTrue
      module procedure assertTrue_
      module procedure assertTrue_withMessage
   end interface

   interface assertExceptionRaised
      module procedure assertExceptionRaisedBasic
      module procedure assertExceptionRaisedMessage
   end interface assertExceptionRaised

   interface toString
      module procedure toString_shape
   end interface toString

   character(len=*), parameter :: NULL_MESSAGE = '<>'

contains

   subroutine fail_(location)
      type (SourceLocation), optional, intent(in) :: location
      call fail(NO_MESSAGE, location)
   end subroutine fail_
   
   subroutine fail_withMessage(message, location)
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      call throw(message, location)
   end subroutine fail_withMessage

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

   logical function conformable(shapeA, shapeB)
      integer, intent(in) :: shapeA(:)
      integer, intent(in) :: shapeB(:)

      if (size(shapeA) == 0 .or. size(shapeB) == 0) then
         conformable = .true.
         return
      end if

      conformable = size(shapeA) == size(shapeB)
      if (conformable) then
         conformable = all(shapeA == shapeB)
      end if
   end function conformable

   logical function nonConformable(shapeA, shapeB)
      integer, intent(in) :: shapeA(:)
      integer, intent(in) :: shapeB(:)

      nonConformable = .not. conformable(shapeA, shapeB)

   end function nonConformable

   function toString_shape(arrayShape) result(string)
      integer, intent(in) :: arrayShape(:)
      integer, parameter :: MAX_LEN_STRING = 80
      character(len=MAX_LEN_STRING) :: string

      integer :: i
      
      select case (size(arrayShape)) ! rank
      case (0) ! scalar
         string = '0'
      case (1)
         write(string,'(i0)'),arrayShape(1)
      case (2:)
         write(string,'(i0,14(",",i0:))') arrayShape(1:)
      end select

      string = '[' // trim(string) // ']'
   end function toString_shape

end module AssertBasic_mod
