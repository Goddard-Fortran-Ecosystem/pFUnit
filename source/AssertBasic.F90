module AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   implicit none
   private
   
   public :: fail

   public :: assertTrue
   public :: assertFalse
   public :: assertEqual
   public :: assertExceptionRaised
   public :: assertSameShape

   ! Utility procedures
   public :: conformable
   public :: nonConformable
   public :: toString
   public :: appendWithSpace

   interface fail
      module procedure fail_
      module procedure fail_withMessage
   end interface fail

   interface assertTrue
      module procedure assertTrue_
      module procedure assertTrue_withMessage
   end interface

   interface assertEqual
      module procedure assertEqualString
   end interface

   interface assertExceptionRaised
      module procedure assertExceptionRaisedBasic
      module procedure assertExceptionRaisedMessage
   end interface assertExceptionRaised

   interface toString
      module procedure toString_shape
   end interface toString

contains

   subroutine fail_(location)
      type (SourceLocation), optional, intent(in) :: location
      call fail(NULL_MESSAGE, location)
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

   subroutine assertSameShape(shapeA, shapeB, message, location)
      integer, intent(in) :: shapeA(:)
      integer, intent(in) :: shapeB(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage
      character(len=MAXLEN_MESSAGE) :: message_

      message_ = NULL_MESSAGE
      if (present(message)) message_ = message

      if (nonConformable(shapeA, shapeB)) then
         throwMessage = 'nonconforming arrays - expected shape: ' // &
              & trim(toString(shapeA)) // ' but found shape: ' // &
              & trim(toString(shapeB))
         call throw(appendWithSpace(message_, throwMessage), location)
      end if
         
   end subroutine assertSameShape

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
         write(string,'(i0)') arrayShape(1)
      case (2:)
         write(string,'(i0,14(",",i0:))') arrayShape(1:)
      end select

      string = '[' // trim(string) // ']'
   end function toString_shape

   ! Joins two strings with a space separator unless first string is
   ! empty.
   function appendWithSpace(a, b) result(ab)
      character(len=*), intent(in) :: a
      character(len=*), intent(in) :: b
      character(len=MAXLEN_MESSAGE) :: ab

      if (len_trim(a) > 0) then
         ab = trim(a) // ' ' // trim(b)
      else
         ab = trim(b)
      end if

   end function appendWithSpace

   subroutine assertFalse(condition)
      logical, intent(in) :: condition
      call assertTrue(.not. condition)
   end subroutine assertFalse

   subroutine assertEqualString(expected, found)
      use Exception_mod, only: throw, MAXLEN_MESSAGE
      character(len=*), intent(in) :: expected
      character(len=*), intent(in) :: found

      character(len=MAXLEN_MESSAGE) :: throwMessage
      integer :: i
      integer :: numSameCharacters

      if (trim(expected) /= trim(found)) then
         numSameCharacters = 0
         do i = 1, min(len_trim(expected), len_trim(found))
            if (expected(i:i) /= found(i:i)) exit
            numSameCharacters = numSameCharacters + 1
         end do
         write(throwMessage,'((a,a),2(a,a,a,a),(a,a,a))') 'String assertion failed:', new_line('A'), &
              & '    expected: <"', trim(expected), '">', new_line('A'), &
              & '   but found: <"', trim(found), '">', new_line('A'), &
              & '  first diff:   ', repeat('-', numSameCharacters), '^'
         call throw(throwMessage)
      end if
   end subroutine assertEqualString

end module AssertBasic_mod
